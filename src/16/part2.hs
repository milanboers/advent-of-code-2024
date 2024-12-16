import Data.List (groupBy, sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Graph.DijkstraSimple (EdgeTo (EdgeTo), Graph (Graph), Path (pathWeight), Paths (pathsAsMap), lightestPaths)
import Graph.DijkstraSimple.Weighters (cumulativeWeighter)

type Pos = (Int, Int)

type Dir = (Int, Int)

type State = (Pos, Dir)

data RotateDir = Clockwise | CounterClockwise deriving (Show, Eq)

rotate :: RotateDir -> State -> State
rotate Clockwise (pos, (di, dj)) = (pos, (dj, -di))
rotate CounterClockwise (pos, (di, dj)) = (pos, (-dj, di))

moveForward :: State -> State
moveForward ((i, j), dir@(di, dj)) = ((i + di, j + dj), dir)

moveBackward :: State -> State
moveBackward ((i, j), dir@(di, dj)) = ((i - di, j - dj), dir)

validState :: Set Pos -> State -> Bool
validState walls ((i, j), _) = (i, j) `Set.notMember` walls

nextStates :: Set Pos -> State -> [State]
nextStates walls state =
  filter (validState walls) [rotate Clockwise state, rotate CounterClockwise state, moveForward state]

prevStates :: Set Pos -> State -> [State]
prevStates walls state =
  filter (validState walls) [rotate CounterClockwise state, rotate Clockwise state, moveBackward state]

cost :: State -> State -> Int
cost (_, d1) (_, d2) | d1 /= d2 = 1000
cost _ _ = 1

edges :: Set Pos -> State -> [EdgeTo State Int]
edges walls s = [EdgeTo s' (cost s s') | s' <- nextStates walls s]

allStates :: Set Pos -> [State]
allStates walls =
  [ ((i, j), dir)
    | i <- [1 .. m],
      j <- [1 .. n],
      (i, j) `Set.notMember` walls,
      dir <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
  ]
  where
    m = maximum $ Set.map fst walls
    n = maximum $ Set.map snd walls

allDists :: Graph State Int -> State -> Map State Int
allDists graph start = Map.map pathWeight . pathsAsMap $ lightestPaths graph start cumulativeWeighter

allOnPaths :: Set Pos -> Map State Int -> State -> Set State
allOnPaths walls dists end = Set.insert end (Set.unions $ Set.map (allOnPaths walls dists) prevOnPaths)
  where
    prevOnPaths =
      Set.fromList
        . map snd
        . filter ((<= dists ! end) . fst)
        . head
        . groupBy (\a b -> fst a == fst b)
        . sort
        . map (\s -> (dists ! s + cost end s, s))
        $ prevStates walls end

findAnswer :: Set Pos -> Pos -> Pos -> Int
findAnswer walls start end =
  Set.size
    . Set.map fst
    . Set.unions
    . Set.map (allOnPaths walls dists)
    . Set.filter ((== shortestPathLength) . (dists !))
    . Set.fromList
    $ endStates
  where
    states = allStates walls
    graph = Graph $ Map.fromList [(v, edges walls v) | v <- states]

    startState = (start, (0, 1))
    dists = allDists graph startState

    endStates = [(v, d) | (v, d) <- states, v == end]
    shortestPathLength = minimum $ map (dists !) endStates

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let fps = [(i, j, x) | (i, line) <- zip [1 ..] input, (j, x) <- zip [1 ..] line]
  let walls = Set.fromList [(i, j) | (i, j, x) <- fps, x == '#']
  let start = head [(i, j) | (i, j, x) <- fps, x == 'S']
  let end = head [(i, j) | (i, j, x) <- fps, x == 'E']
  print $ findAnswer walls start end
