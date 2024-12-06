import Data.Set (Set)
import qualified Data.Set as Set

type Dir = (Int, Int)

type Pos = (Int, Int)

type Field = ((Int, Int), Set Pos) -- // bounds, obstacles

turnRight :: Dir -> Dir
turnRight (1, 0) = (0, -1)
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (0, -1) = (-1, 0)
turnRight _ = error "invalid direction"

run :: Field -> Set Pos -> Pos -> Dir -> Set Pos
run field@((m, n), obstacles) seen pos@(i, j) dir@(di, dj)
  | i < 1 || j < 1 || i > m || j > n = seen
  | newPos `Set.member` obstacles = run field seen pos (turnRight dir)
  | otherwise = run field (Set.insert pos seen) newPos dir
  where
    newPos = (i + di, j + dj)

findAnswer :: Field -> (Int, Int) -> Int
findAnswer field pos = Set.size $ run field Set.empty pos (-1, 0)

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
