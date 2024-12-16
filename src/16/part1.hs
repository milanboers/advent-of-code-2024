import Algorithm.Search (dijkstra)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Dir = (Int, Int)

type State = (Pos, Dir)

data RotateDir = Clockwise | CounterClockwise deriving (Show, Eq)

rotate :: RotateDir -> State -> State
rotate Clockwise (pos, (di, dj)) = (pos, (dj, -di))
rotate CounterClockwise (pos, (di, dj)) = (pos, (-dj, di))

moveForward :: State -> State
moveForward ((i, j), dir@(di, dj)) = ((i + di, j + dj), dir)

validState :: Set Pos -> State -> Bool
validState walls ((i, j), _) = (i, j) `Set.notMember` walls

nextStates :: Set Pos -> State -> Set State
nextStates walls state =
  Set.fromList $
    filter (validState walls) [rotate Clockwise state, rotate CounterClockwise state, moveForward state]

cost :: State -> State -> Int
cost (_, d1) (_, d2) | d1 /= d2 = 1000
cost _ _ = 1

isEnd :: Pos -> State -> Bool
isEnd end (pos, _) = pos == end

findAnswer :: Set Pos -> Pos -> Pos -> Int
findAnswer walls start end = fst . fromJust $ dijkstra (nextStates walls) cost (isEnd end) (start, (0, 1))

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let fps = [(i, j, x) | (i, line) <- zip [1 ..] input, (j, x) <- zip [1 ..] line]
  let walls = Set.fromList [(i, j) | (i, j, x) <- fps, x == '#']
  let start = head [(i, j) | (i, j, x) <- fps, x == 'S']
  let end = head [(i, j) | (i, j, x) <- fps, x == 'E']
  print $ findAnswer walls start end
