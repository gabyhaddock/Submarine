{-
SECTION-01: Moving rooms
SECTION-02: Opening hatches
SECTION-03: Coordinating high-level functions for turns and game state
SECTION-04: Game state analysis 
SECTION-05: Sample data
-}

import Data.List

data Sub = Sub {rooms::[Room], hatches::[Hatch]} deriving Show -- This structure got smaller, move out of record syntax?

data Room = Room Int RoomState deriving Show

instance Ord Room where
    compare (Room x1 s1) (Room x2 s2) | x1 < x2   = LT
                                     | x1 > x2   = GT
                                     | otherwise = compare s1 s2

instance Eq Room where
    r1 == r2 = (compare r1 r2) == EQ


data RoomState = Clear | LowFlood | HighFlood | Fire  deriving (Show, Eq, Ord)

data Hatch =  Hatch (Int, Int) HatchState deriving (Eq, Show)
data HatchState = Open | Closed | Blocked  deriving (Show, Eq)

data Action = Move Room Int | OpenHatch Hatch Int deriving Show -- Int is Cost
{-
data Move = Move Room Int deriving Show -- Int is Cost
data OpenHatch = OpenHatch Hatch Int deriving Show -- Int is Cost
-}

data GameState = GameState Sub [Action] deriving Show

-- =========================
-- SECTION-01: Moving rooms
-- =========================

-- Helper functions for the Move object
isMove :: Action -> Bool
isMove (Move room _ ) = True
isMove _              = False

roomNum :: Room -> Int
roomNum (Room num _ ) = num

roomState :: Room -> RoomState
roomState (Room _ state) = state

-- Given a room, return a list of adjacent rooms. Only uses Open hatches
adjacentRooms :: Room -> Sub -> [Room]
adjacentRooms (Room n _ ) sub = filter (\(Room x _ ) -> x `elem` adjRoomNums) (rooms sub)
        where adjRoomNums = [ x | (Hatch (x, y) state) <- (hatches sub), y == n, state == Open]
                         ++ [ y | (Hatch (x, y) state) <- (hatches sub), x == n, state == Open]

-- A room cannot be entered if it is HighFlood or Fire
canEnterRoom :: Room -> Bool
canEnterRoom (Room _ HighFlood) = False
canEnterRoom (Room _ Fire)      = False
canEnterRoom  _                 = True

-- Some adjacent rooms may not be able to be entered - we need to filter them out from adjacentRooms
adjacentRoomsCanEnter :: Room -> Sub -> [Room]
adjacentRoomsCanEnter room sub =  filter (canEnterRoom) (adjacentRooms room sub)

-- Entering a room that is flooded to LowFlood costs 2 minutes
costToEnter                  :: Room -> Int
costToEnter (Room _ LowFlood) = 2
costToEnter _                 = 1




-- Takes in the action list from the game state (which may include Moves and HatchOpen actions) and returns all the visited Rooms
-- Note: these are in reverse order of the actual visits
visitedRooms :: [Action] -> [Room]
visitedRooms actions | null moves =  error "The action list must include at least the starting room"
                     | otherwise   = map (\(Move room _) -> room) moves --This lambda is not exhaustive, but we should have filtered out any non-Move actions
            where moves = (filter isMove actions)

-- For this function, we can pass in the entire action list from the GameState, no need to filter to moves
currentRoom :: [Action] -> Room
currentRoom actions = case visitedRooms actions of
                           []      -> error "The action list must include at least the starting room"
                           visited -> head visited

-- For a given GameState, show all possible GameStates that can follow from a single move
-- Obeys the following rules: 
--  * The new room must be adjacent.
--  * The new room must have nott been visited in this game state
--  * The new room's state must not be HighFlood or Fire
moveRooms :: GameState -> [GameState]
moveRooms (GameState sub actions)  =  [ (GameState sub ((Move newRoom  (costToEnter newRoom)):actions))
                                       | newRoom <- (adjRooms),
                                         newRoom `notElem` visited,
                                         canEnterRoom newRoom,
                                         roomState currRoom /= Fire] -- The player may not leave a room on fire
       where currRoom    = currentRoom actions
             visited     = visitedRooms actions
             adjRooms    = adjacentRooms currRoom sub

-- =============================
-- SECTION-02: Opening hatches
-- =============================

--Helper function
hatchRooms                     :: Hatch -> (Int, Int)
hatchRooms (Hatch rooms state) = rooms

-- Given a room, return a list of all connecting hatches (regardless of state)
adjacentHatches                  :: Room -> Sub -> [Hatch]
adjacentHatches (Room n _ ) sub  =   [ (Hatch (x, y) state) | (Hatch (x, y) state) <- (hatches sub), (x == n ||y == n)]

canOpenHatch                  :: Hatch -> Bool
canOpenHatch (Hatch _ Closed) = True
canOpenHatch _                = False

-- Returns adjacent hatches that are currently Closed.  Open and Blocked hatches cannot be opened.
adjacentHatchesCanOpen          :: Room -> Sub -> [Hatch]
adjacentHatchesCanOpen room sub = filter (canOpenHatch) (adjacentHatches room sub)

-- For a single hatch, update its state to the HatchState in the first parameter.  So far, Open is the only parameter actually used.
updateHatchTo                                 :: HatchState -> Hatch -> Hatch
updateHatchTo newState (Hatch (x,y) oldState)  = (Hatch (x,y) newState)

-- Pass in an updated hatch and look for it in the existing hatch list.  If found, replace it with the new state.
-- If newHatch is not found, it is NOT added.  This should only be used when updating an existing hatch.
-- Also note that the order of the room pair (x,y) matters.  TODO: allow flipped pairs?
updateHatchesWith :: Hatch -> [Hatch] -> [Hatch]
updateHatchesWith newHatch hatches  = map (\oldHatch -> if hatchRooms oldHatch == hatchRooms newHatch
                                                 then newHatch
                                                 else oldHatch)
                                           hatches


-- Pass in an updated room and look for it in the existing room list.  If found, replace it with the new room.
-- If newRoom is not found, it is NOT added.
updateRoomsWith                :: Room -> [Room] -> [Room]
updateRoomsWith newRoom rooms  = map (\oldRoom -> if roomNum oldRoom == roomNum newRoom
                                                  then newRoom
                                                  else oldRoom)
                                      rooms


-- Given an opened hatch and the original list of rooms, check the final states of the two adjacent rooms
-- If one is at HighFlood and the other is Clear or Fire, the rooms equalize to LowFlood.
-- Note: Will error out if the Room list does not contain both numbers listed in the Hatch.
flowRooms                   :: Hatch -> [Room] -> [Room]
flowRooms openedHatch rooms =
             if (oldFirstState == HighFlood && (oldSecondState == Clear || oldSecondState == Fire))
                || (oldSecondState == HighFlood && (oldFirstState == Clear || oldFirstState == Fire))
             then updateRoomsWith (Room firstRoomNum LowFlood) (updateRoomsWith (Room secondRoomNum LowFlood) rooms)
             else rooms
             where firstRoomNum = fst (hatchRooms openedHatch)
                   secondRoomNum = snd (hatchRooms openedHatch)
                   oldFirstRoom = head (filter (\x -> roomNum x == firstRoomNum) rooms)
                   oldSecondRoom = head (filter (\x -> roomNum x == secondRoomNum) rooms)
                   oldFirstState = roomState oldFirstRoom
                   oldSecondState = roomState oldSecondRoom

-- For a given GameState, show all possible GameStates that can follow from opening a single hatch
-- Obeys the following rules:
--  * Only Closed hatches can be opened, not Open or Blocked hatches
--  * The two adjacent rooms can flow water between them
--  * The rooms and hatches list of the Sub are modified as needed.
openHatches :: GameState -> [GameState]
openHatches (GameState sub actions) = [ (GameState 
                                        sub{
                                            rooms = flowRooms openedHatch (rooms sub),
                                            hatches = updateHatchesWith openedHatch (hatches sub) -- Replace the closed hatch in the list with the opened hatch
                                           } 
                                        ( (OpenHatch openedHatch 1) : actions) )
                                        | openedHatch <- map (updateHatchTo Open) adjHatches ]
           where currRoom    = currentRoom actions
                 adjHatches  = adjacentHatchesCanOpen currRoom sub


-- ===================================================================
-- SECTION-03: Coordinating high-level functions for turns and game state
-- ===================================================================


-- Take one turn
takeTurn :: [GameState] -> [GameState]
takeTurn gameState =  (concat . (map moveRooms)) gameState 
                       ++ (concat . (map openHatches)) gameState 

--Get all moves, regardless of depth.  This terminates when a row in the grid has no results (no additional moves from the previous row)
allMoves :: [GameState] -> [GameState]
allMoves gameState = concat $ takeWhile (not . null) (iterate takeTurn gameState)

--moves only:  map (\(GameState sub moves) -> moves) (allMoves miniList)


--concat $ map moveRooms $ moveRooms (GameState mini [(Move (Room 1) 0)])

startGame :: Sub -> Int -> [GameState]
startGame sub roomNum = if roomNum `elem` (map (\(Room n _) -> n) (rooms sub)) 
                        then [GameState sub [(Move room 0)]]
                        else error "Start room is not found in the description of the sub"
               where room = head (filter (\(Room n s) -> n == roomNum) (rooms sub))

-- test: allMoves (startGame mini 1)

-- =============================
-- SECTION-04: Game state analysis 
-- =============================


-- Helper functions
gameStateActions                    :: GameState -> [Action]
gameStateActions (GameState sub acts) = acts

gameStateSub                     :: GameState -> Sub
gameStateSub (GameState sub acts) = sub

gameStateCurrentRoom :: GameState -> Room
gameStateCurrentRoom gs = currentRoom (gameStateActions gs)

-- Find the total cost of a GameState (used for final output of results

actionCost                    :: Action -> Int
actionCost (Move _ cost)      = cost
actionCost (OpenHatch _ cost) = cost

totalCost                      :: GameState -> Int
totalCost (GameState sub actions) = sum (map actionCost actions)

-- Ordering and equality functions to order & group by various aspects of the GameState
orderByCost         :: GameState -> GameState -> Ordering 
orderByCost gs1 gs2 = compare (totalCost gs1) (totalCost gs2)

{-
--DELETE from here....
equalByCost         :: GameState -> GameState -> Bool
equalByCost gs1 gs2 = (totalCost gs1) == (totalCost gs2)


orderByCurrentRoom :: GameState -> GameState -> Ordering
orderByCurrentRoom gs1 gs2 = compare  (roomNum (gameStateCurrentRoom gs1)) (roomNum (gameStateCurrentRoom gs2))

equalByCurrentRoom :: GameState -> GameState -> Bool
equalByCurrentRoom gs1 gs2 = (roomNum (gameStateCurrentRoom gs1)) == (roomNum (gameStateCurrentRoom gs2))

equalBySubRooms :: GameState -> GameState -> Bool
equalBySubRooms gs1 gs2 = (rooms (gameStateSub gs1)) == (rooms (gameStateSub gs2))
--- to here?
-}

orderByGameState :: GameState -> GameState -> Ordering
orderByGameState gs1 gs2 | currRoom1 < currRoom2  = LT -- Use Ord for rooms
                         | currRoom1 > currRoom2  = GT
                         | otherwise              = compare (rooms (gameStateSub gs1)) (rooms (gameStateSub gs2))
    where currRoom1 = gameStateCurrentRoom gs1
          currRoom2 = gameStateCurrentRoom gs2


equalByGameState :: GameState -> GameState -> Bool
equalByGameState gs1 gs2 = (orderByGameState gs1 gs2) == EQ 


-- Given a list of possible game states, group into "equal" game states (same final room, same room state) and choose the lowest cost game state from each equivalent group
-- Note that haskell groupBy only groups adjacent elements, so I need to sortBy orderByGameState first
prune :: [GameState] -> [GameState]
prune gs = map (head . sortBy orderByCost) (groupBy equalByGameState ( sortBy orderByGameState gs))

-- =============================
-- SECTION-05: Sample data
-- =============================

mini = Sub { rooms =
               [ Room 1 Clear,
                 Room 2 Clear,
                 Room 3 Clear,
                 Room 4 Clear]
            , hatches =
               [ Hatch (1,2) Closed,
                 Hatch (1,3) Closed,
                 Hatch (2,3) Closed,
                 Hatch (2,4) Closed,
                 Hatch (3,4) Closed]
                }


miniFlood = Sub  { rooms =
               [ Room 1 Clear,
                 Room 2 HighFlood,
                 Room 3 Fire,
                 Room 4 Clear]
            , hatches =
               [ Hatch (1,2) Closed,
                 Hatch (1,3) Closed,
                 Hatch (2,3) Closed,
                 Hatch (2,4) Closed,
                 Hatch (3,4) Closed]
                }

miniList = startGame mini 1