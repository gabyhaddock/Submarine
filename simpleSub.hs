{-
SECTION-01: Moving rooms
SECTION-02: Opening hatches
SECTION-03: Coordinating high-level functions for turns and game state
SECTION-04: Game state analysis 
SECTION-05: Sample data
-}

import Data.List

data Sub = Sub {rooms::[Room], hatches::[Hatch]} deriving Show -- This structure got smaller, move out of record syntax?

data Room = Room Int RoomState deriving (Eq, Show)
data RoomState = Clear | HighFlood | LowFlood | Fire  deriving (Show, Eq)

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
            where moves = (dropWhile (not . isMove) actions)

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


-- ===================================================================
-- SECTION-03: Coordinating high-level functions for turns and game state
-- ===================================================================

-- Take one turn
plusDepth :: [GameState] -> [GameState]
plusDepth gameState =  (concat . (map moveRooms)) gameState 

--Get all moves, regardless of depth.  This terminates when a row in the grid has no results (no additional moves from the previous row)
allMoves :: [GameState] -> [GameState]
allMoves gameState = concat $ takeWhile (not . null) (iterate plusDepth gameState)

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

-- Find the total cost of a GameState (used for final output of results
totalCost :: GameState -> Int
totalCost (GameState sub moves) = sum (map (\(Move room cost) -> cost) moves)


-- =============================
-- SECTION-05: Sample data
-- =============================

mini = Sub { rooms =
               [ Room 1 Clear,
                 Room 2 LowFlood,
                 Room 3 Fire,
                 Room 4 Clear]
            , hatches =
               [ Hatch (1,2) Open,
                 Hatch (1,3) Open,
                 Hatch (2,3) Open,
                 Hatch (2,4) Open,
                 Hatch (3,4) Closed]
                }


miniList = startGame mini 1