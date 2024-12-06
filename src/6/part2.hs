import Data.Set (Set)
import qualified Data.Set as Set

type Dir = (Int, Int)

type Pos = (Int, Int)

type Field = ((Int, Int), Set Pos) -- // bounds, obstacles

withObstacle :: Field -> Pos -> Field
withObstacle ((m, n), obstacles) (i, j) = ((m, n), Set.insert (i, j) obstacles)

turnRight :: Dir -> Dir
turnRight (1, 0) = (0, -1)
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (0, -1) = (-1, 0)
turnRight _ = error "invalid direction"

isLoop :: Field -> Set (Pos, Dir) -> Pos -> Dir -> Bool
isLoop field@((m, n), obstacles) seen pos@(i, j) dir@(di, dj)
  | (pos, dir) `Set.member` seen = True
  | i < 1 || j < 1 || i > m || j > n = False
  | newPos `Set.member` obstacles = isLoop field newSeen pos (turnRight dir)
  | otherwise = isLoop field newSeen newPos dir
  where
    newSeen = Set.insert (pos, dir) seen
    newPos = (i + di, j + dj)

findAnswer :: Field -> (Int, Int) -> Int
findAnswer field@((m, n), _) pos =
  length
    [(i, j) | i <- [1 .. m], j <- [1 .. n], (i, j) /= pos, isLoop (withObstacle field (i, j)) Set.empty pos (-1, 0)]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let m = length input
  let n = length . head $ input
  let els = [(i, j, x) | (i, line) <- zip [1 ..] input, (j, x) <- zip [1 ..] line]
  let obstacles = Set.fromList [(i, j) | (i, j, x) <- els, x == '#']
  let start = head [(i, j) | (i, j, x) <- els, x == '^']
  let field = ((m, n), obstacles)
  print $ findAnswer field start
