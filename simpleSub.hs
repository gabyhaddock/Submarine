import Data.List

data Sub = Sub {rooms::[Room], hatches::[Hatch]} deriving Show

data Room = Room Int RoomState deriving (Eq, Show)
data RoomState = Clear | HighFlood | LowFlood | Fire  deriving (Show, Eq)

data Hatch =  Hatch (Int, Int) HatchState deriving (Eq, Show)
data HatchState = Open | Closed | Blocked  deriving (Show, Eq)

data Move = Move Room Int deriving Show -- Int is Cost


data GameState = GameState Sub [Move] deriving Show


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

totalCost :: GameState -> Int
totalCost (GameState sub moves) = sum (map (\(Move room cost) -> cost) moves)

moveRooms :: GameState -> [GameState]
moveRooms (GameState sub moves)  =  [ (GameState sub ((Move newRoom  (costToEnter newRoom)):moves))
                                    | newRoom <- (adjacentRooms currRoom sub),
                                     newRoom `notElem` visited,
                                     canEnterRoom newRoom]
       where (Move currRoom _) = (head moves)
             visited     = map (\(Move room _) -> room) moves


plusDepth :: [GameState] -> [GameState]
plusDepth gameState =  (concat . (map moveRooms)) gameState 

tenMoves :: [GameState] -> [GameState]
tenMoves gameState = concat $ take 10 $ iterate plusDepth gameState

--Get all moves, regardless of depth:
allMoves :: [GameState] -> [GameState]
allMoves gameState = concat $ takeWhile (not . null) (iterate plusDepth miniList)

--moves only:  map (\(GameState sub moves) -> moves) (allMoves miniList)


--concat $ map moveRooms $ moveRooms (GameState mini [(Move (Room 1) 0)])

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


miniList = [(GameState mini [(Move (Room 1 Clear) 0)])]