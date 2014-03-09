
* SECTION-01: Moving rooms  
* SECTION-02: Opening hatches  
* SECTION-03: Coordinating high-level functions for turns and game state  
* SECTION-04: Game state analysis  
* SECTION-05: Exporting to JSON
* SECTION-06: Sample data  


> import Data.List
> import Data.List.Split


> data Room = Room Int RoomState deriving Show

> instance Ord Room where
>    compare (Room x1 s1) (Room x2 s2) | x1 < x2   = LT
>                                      | x1 > x2   = GT
>                                      | otherwise = compare s1 s2

> instance Eq Room where
>     r1 == r2 = (compare r1 r2) == EQ


> data RoomState = Clear | LowFlood | HighFlood | Fire  deriving (Show, Eq, Ord)

> data Hatch =  Hatch (Int, Int) HatchState deriving (Eq, Show)
> data HatchState = Open | Closed | Blocked  deriving (Show, Eq)

> data Action = Move Room Int | OpenHatch Hatch Int | Flood Room Room Int deriving Show -- Int is Cost

> data GameState = GameState {
>                               rooms::[Room],
>                               hatches::[Hatch],
>                               actions:: [Action]
>                            } deriving Show



SECTION-01: Moving rooms
---

> --Helper functions for the Move object

> isMove :: Action -> Bool
> isMove (Move room _ ) = True
> isMove _              = False

> roomNum :: Room -> Int
> roomNum (Room num _ ) = num

> roomState :: Room -> RoomState
> roomState (Room _ state) = state

> --Given a room, return a list of adjacent rooms. Only uses Open hatches
> adjacentRooms :: Room -> GameState -> [Room]
> adjacentRooms (Room n _ ) gst = filter (\(Room x _ ) -> x `elem` adjRoomNums) (rooms gst)
>         where adjRoomNums = [ x | (Hatch (x, y) state) <- (hatches gst), y == n, state == Open]
>                          ++ [ y | (Hatch (x, y) state) <- (hatches gst), x == n, state == Open]

> --A room cannot be entered if it is HighFlood or Fire
> canEnterRoom :: Room -> Bool
> canEnterRoom (Room _ HighFlood) = False
> canEnterRoom (Room _ Fire)      = False
> canEnterRoom  _                 = True

> -- Some adjacent rooms may not be able to be entered - we need to filter them out from adjacentRooms
> adjacentRoomsCanEnter :: Room -> GameState -> [Room]
> adjacentRoomsCanEnter room gst =  filter (canEnterRoom) (adjacentRooms room gst)

> -- Entering a room that is flooded to LowFlood costs 1 minute, all others are 0
> costToEnter                  :: Room -> Int
> costToEnter (Room _ LowFlood) = 1
> costToEnter _                 = 0




> -- Takes in the action list from the game state (which may include Moves and HatchOpen actions) and returns all the visited Rooms
> -- Note: these are in reverse order of the actual visits
> visitedRooms :: GameState -> [Room]
> visitedRooms gst | null moves =  error "The action list must include at least the starting room"
>                  | otherwise   = map (\(Move room _) -> room) moves --This lambda is not exhaustive, but we should have filtered out any non-Move actions
>             where moves = (filter isMove (actions gst))

For this function, we can pass in the entire action list from the GameState, no need to filter to moves
The order is important.  The initial room is the last element of the Actions list, and the final (or current) room is the head of the actions list.

> currentRoom         :: GameState -> Room
> currentRoom gst = case visitedRooms gst of
>                        []      -> error "The action list must include at least the starting room"
>                        visited -> head visited

> startRoom         :: GameState -> Room
> startRoom gst  = case visitedRooms gst of
>                       []       -> error "The action list must include at least the starting room"
>                       visited  -> last visited


> isVisited                  :: Room -> GameState -> Bool
> isVisited (Room num _) gst  = num `elem` (map roomNum (visitedRooms gst))

For a given GameState, show all possible GameStates that can follow from a single move
Obeys the following rules: 
  * The new room must be adjacent.
  * The new room must have not been visited in this game state
  * The new room's state must not be HighFlood or Fire

> moveRooms :: GameState -> [GameState]
> moveRooms gst  =  [ gst{actions = ((Move newRoom  (costToEnter newRoom)) --Add the new room to the head of the actions list
>                                    :(actions gst)) }
>                                        | newRoom <- adjRooms,
>                                          notVisited newRoom,
>                                          canEnterRoom newRoom,
>                                          roomState currRoom /= Fire] -- The player may not leave a room on fire
>        where currRoom    = currentRoom gst
>              adjRooms    = adjacentRooms currRoom gst
> -- This is more than just `elem` -- the game state may have changed since it was visited, so we need to compare number and ignore state
>              notVisited (Room num _ ) =  num `notElem` (map roomNum (visitedRooms gst))


SECTION-02: Opening hatches
---

> --Helper functions
> hatchRooms                     :: Hatch -> (Int, Int)
> hatchRooms (Hatch rooms state) = rooms

> -- Given a room, return a list of all connecting hatches that are currently closed
> -- Opened hatches should not be re-opened, and blocked hatches should not be opened.
> adjacentClosedHatches                  :: Room -> GameState -> [Hatch]
> adjacentClosedHatches (Room n _ ) gst  =   [ (Hatch (x, y) state) | (Hatch (x, y) state) <- (hatches gst), (x == n ||y == n), state == Closed]





> -- Pass in a hatch and look for it in the existing hatch list.  If found, replace its state with Open
> -- If newHatch is not found, it is NOT added.  This should only be used when updating an existing hatch.
> -- Also note that the order of the room pair (x,y) matters.  
> openHatch :: Hatch -> [Hatch] -> [Hatch]
> openHatch (Hatch rooms _) hatches  = map (\oldHatch -> if hatchRooms oldHatch == rooms
>                                                  then (Hatch rooms Open)
>                                                  else oldHatch)
>                                            hatches


> -- Pass in an updated room and look for it in the existing room list.  If found, replace it with the new room.
> -- If newRoom is not found, it is NOT added.
> floodRoom                    :: Room -> [Room] -> [Room]
> floodRoom (Room num _) rooms  = map (\oldRoom -> if roomNum oldRoom == num
>                                                   then (Room num LowFlood)
>                                                   else oldRoom)
>                                       rooms


> -- Given a hatch (assumed already opened) and the original list of rooms, check the final states of the two adjacent rooms
> -- If one is at HighFlood and the other is Clear or Fire, the rooms equalize to LowFlood.
> -- Return Nothing if we do not need to flow, and Maybe (Room, Room) if the rooms should be flooded
> -- Note: Will error out if the Room list does not contain both numbers listed in the Hatch.
> roomsToFlood                   :: Hatch -> [Room] -> Maybe (Room, Room)
> roomsToFlood openedHatch rooms =
>              if (oldFirstState == HighFlood && (oldSecondState == Clear || oldSecondState == Fire))
>                 || (oldSecondState == HighFlood && (oldFirstState == Clear || oldFirstState == Fire))
>              then Just (oldFirstRoom, oldSecondRoom)
>              else Nothing
>              where firstRoomNum = fst (hatchRooms openedHatch)
>                    secondRoomNum = snd (hatchRooms openedHatch)
>                    oldFirstRoom = head (filter (\x -> roomNum x == firstRoomNum) rooms)
>                    oldSecondRoom = head (filter (\x -> roomNum x == secondRoomNum) rooms)
>                    oldFirstState = roomState oldFirstRoom
>                    oldSecondState = roomState oldSecondRoom


> {-
> -- Given a hatch (assumed already opened) and the original list of rooms, check the final states of the two adjacent rooms
> -- If one is at HighFlood and the other is Clear or Fire, the rooms equalize to LowFlood.
> -- Note: Will error out if the Room list does not contain both numbers listed in the Hatch.
> flowRooms                   :: Hatch -> [Room] -> [Room]
> flowRooms openedHatch rooms =
>              if (oldFirstState == HighFlood && (oldSecondState == Clear || oldSecondState == Fire))
>                 || (oldSecondState == HighFlood && (oldFirstState == Clear || oldFirstState == Fire))
>              then (floodRoom oldFirstRoom  . floodRoom oldSecondRoom) rooms
>              else rooms
>              where firstRoomNum = fst (hatchRooms openedHatch)
>                    secondRoomNum = snd (hatchRooms openedHatch)
>                    oldFirstRoom = head (filter (\x -> roomNum x == firstRoomNum) rooms)
>                    oldSecondRoom = head (filter (\x -> roomNum x == secondRoomNum) rooms)
>                    oldFirstState = roomState oldFirstRoom
>                    oldSecondState = roomState oldSecondRoom
> -}



For a given GameState, openHatches shows all possible GameStates that can follow from opening a single hatch
Obeys the following rules:
* Only Closed hatches can be opened, not Open or Blocked hatches
* The two adjacent rooms can flow water between them
* The rooms and hatches list of the Sub are modified as needed.

> openHatches       :: GameState -> [GameState]
> openHatches gst    = [ GameState {
>                                     rooms = newRooms,
>                                     hatches = newHatches, -- Replace the closed hatch in the list with the opened hatch
>                                     actions = newActions
>                                    }  
>                        | openedHatch <- adjHatches, 
>                                         let floodRooms = roomsToFlood openedHatch (rooms gst),
> --If rooms were flooded, update the rooms list to show the side effects
>                                         let newRooms = case floodRooms of Nothing -> rooms gst
>                                                                           Just (f, s) -> ( floodRoom f . floodRoom s ) (rooms gst),
>                                         let newHatches = openHatch openedHatch (hatches gst),
> --In all cases, add a 1-minute OpenHatch action
>                                         let baseActions = (OpenHatch openedHatch 1) : (actions gst),
> --In the case that rooms were flooded, add an additional special Flood action to indicate which rooms were flooded
>                                         let newActions = case floodRooms of Nothing ->  baseActions
>                                                                             Just (f, s) -> (Flood f s 0) : baseActions
>                       ]
>            where currRoom    = currentRoom gst
>                  adjHatches  = adjacentClosedHatches currRoom gst




SECTION-03: Coordinating high-level functions for turns and game state
---


> -- Take one turn
> takeTurn :: [GameState] -> [GameState]
> takeTurn gameState =  (concat . (map moveRooms)) gameState 
>                        ++ (concat . (map openHatches)) gameState 

> --Get all moves, regardless of depth.  This terminates when a row in the grid has no results (no additional moves from the previous row)
> allMoves :: [GameState] -> [GameState]
> allMoves gameState = concat $ takeWhile (not . null) (iterate takeTurn gameState)

testing:
moves only:  map actions (allMoves mini)


> startGame :: GameState -> [GameState]
> startGame gst = prune (allMoves ([gst]))


test: startGame mini

SECTION-04: Game state analysis 
---


> -- Helper functions


> --Find the total cost of a GameState (used for final output of results)
> actionCost                    :: Action -> Int
> actionCost (Move _ cost)      = cost
> actionCost (OpenHatch _ cost) = cost
> actionCost (Flood _ _ cost)   = cost

> totalCost                      :: GameState -> Int
> totalCost gst = sum (map actionCost (actions gst))

> -- Ordering and equality functions to order & group by various aspects of the GameState
> orderByCost         :: GameState -> GameState -> Ordering 
> orderByCost gs1 gs2 = compare (totalCost gs1) (totalCost gs2)

> orderByRooms         :: GameState -> GameState -> Ordering
> orderByRooms gs1 gs2 | currRoom1 < currRoom2  = LT -- Use Ord for rooms
>                      | currRoom1 > currRoom2  = GT
>                      | otherwise              = compare (rooms gs1) (rooms gs2)
>     where currRoom1 = currentRoom gs1
>           currRoom2 = currentRoom gs2

> equalByRooms         :: GameState -> GameState -> Bool
> equalByRooms gs1 gs2 = (orderByRooms gs1 gs2) == EQ 


> --Given a list of possible game states, group into "equal" game states (same final room, same state of all rooms) and choose the lowest cost game state from each equivalent group
> -- Note that haskell groupBy only groups adjacent elements, so I need to sortBy orderByRooms first
> -- Also remove the zero-cost state that involves doing nothing.
> prune    :: [GameState] -> [GameState]
> prune gst = filter (\gs -> totalCost gs /= 0) 
>             (map (head . sortBy orderByCost) 
>             (groupBy equalByRooms (sortBy orderByRooms   gst)))


SECTION-05: Exporting to JSON
--- 


> class Json a where
>  json :: a -> String

> instance Json a => Json [a] where
>    json a = "[\n" ++
>                 intercalate ",\n" (map json a) ++
>                "\n]"

> instance Json Room where
>    json (Room number state)   = "{ \"number\": " ++ (show number) ++
>                                 " , \"state\": \"" ++ (show state) ++ "\" }"

> --Represent the numbers from the hatch as an string Hatch (1,2) _ -> "number" : "1-2"
> instance Json Hatch where
>    json (Hatch (x,y) state)  = "{ \"number\": \"" ++ (show x) ++ "-" ++ (show y) ++
>                                "\" , \"state\": \"" ++ (show state) ++ "\" }"


> instance Json Action where
>    json (Move room cost)         = "{ \"type\": \"Move\", \"room\": " ++ json room ++ ", \"cost\": " ++ show cost ++ " }"
>    json (OpenHatch hatch cost)   = "{ \"type\": \"OpenHatch\", \"hatch\": " ++ json hatch ++ ", \"cost\": " ++ show cost ++ " }"
>    json (Flood room1 room2 cost) = "{ \"type\": \"Flood\", \"room1\": " ++ json room1 ++ ", \"room2\": " ++ json room2 ++ ", \"cost\": " ++ show cost ++ " }"

> instance Json GameState where
>    json gst                     = "{ \"rooms\": " ++ json (rooms gst) ++
>                                   ", \n \"hatches\": " ++ json (hatches gst) ++  
>                                   ", \n \"actions\": " ++ json (tail (reverse (actions gst))) ++  
> -- We store the actions in reverse order, flip them to print. Also, remove the starting room (which is the head after reversal)
>                                   ", \n \"finalRoom\": " ++ json (currentRoom gst) ++
>                                   ", \n \"startRoom\": " ++ json (startRoom gst) ++
>                                   ", \n \"totalCost\": " ++ show (totalCost gst) ++
>                                   "}"

Test: putStrLn (json mini)

IO actions:  print out to /html/sub.js
For now, just take in a hardcoded initial GameState.

> outputResults :: GameState -> IO ()
> outputResults gst = do
>          writeFile "html/sub.js" "var starterSub = "
>          appendFile "html/sub.js" (json gst)
>          appendFile "html/sub.js" "; \n\n var results = "
>          appendFile "html/sub.js" (json (startGame gst))
>          appendFile "html/sub.js" ";"
>

SECTION-06: Sample data
---

> mini = GameState { rooms =
>                [ Room 1 Clear,
>                  Room 2 Clear,
>                  Room 3 Clear,
>                  Room 4 Clear]
>             , hatches =
>                [ Hatch (1,2) Closed,
>                  Hatch (1,3) Closed,
>                  Hatch (2,3) Closed,
>                  Hatch (2,4) Closed,
>                  Hatch (3,4) Closed]
>              , actions = [Move (Room 1 Clear) 0]
>                 }


> miniFlood = GameState  { rooms =
>                [ Room 1 Clear,
>                  Room 2 HighFlood,
>                  Room 3 Fire,
>                  Room 4 Clear]
>             , hatches =
>                [ Hatch (1,2) Closed,
>                  Hatch (1,3) Closed,
>                  Hatch (2,3) Closed,
>                  Hatch (2,4) Closed,
>                  Hatch (3,4) Closed]
>             , actions = [Move (Room 1 Clear) 0]
>                 }

> {-
> starterSub = GameState { rooms = 
>                   [(Room 1 Clear),
>                   (Room 2 Clear),
>                   (Room 3 Clear),
>                   (Room 4 Clear),
>                   (Room 5 Clear),
>                   (Room 6 Clear),
>                   (Room 7 Clear),
>                   (Room 8 Clear),
>                   (Room 9 Clear),
>                   (Room 10 Clear)] 
>                   , hatches =
>                  [ (Hatch (1, 2) Open),
>                    (Hatch (1, 3) Open),
>                    (Hatch (2, 3) Open),
>                    (Hatch (2, 4) Closed),
>                    (Hatch (2, 5) Open),
>                    (Hatch (3, 4) Open),
>                    (Hatch (4, 5) Open),
>                    (Hatch (5, 6) Open),
>                    (Hatch (5, 7) Open),
>                    (Hatch (5, 8) Closed),
>                    (Hatch (7, 8) Open),
>                    (Hatch (7, 9) Open),
>                    (Hatch (8, 9) Open),
>                    (Hatch (8, 10) Open),
>                    (Hatch (9, 10) Open)
>                  ]
>                  }
>                  

> -}

