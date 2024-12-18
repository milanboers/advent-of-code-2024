import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Bounds = (Int, Int)

type Field = (Bounds, Set Pos) -- bounds, bytes

inBounds :: Field -> Pos -> Bool
inBounds ((bx, by), _) (x, y) = x >= 0 && y >= 0 && x <= bx && y <= by

neighbors :: Field -> Pos -> Set Pos
neighbors field@(_, bytes) (x, y) =
  Set.fromList
    . filter (`Set.notMember` bytes)
    . filter (inBounds field)
    $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

bfs :: Field -> Pos -> Set Pos -> Set Pos -> Int
bfs _ end _ current | end `Set.member` current = 0
bfs field end seen current = 1 + bfs field end newSeen newCurrent
  where
    newCurrent = Set.unions (Set.map (neighbors field) current) `Set.difference` seen
    newSeen = seen `Set.union` current

findAnswer :: Field -> Int
findAnswer field@(bounds, _) = bfs field bounds Set.empty (Set.singleton (0, 0))

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "list not of length 2"

parseLine :: String -> (Int, Int)
parseLine = tuplify . map read . splitOn ","

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let bytes = map parseLine input
  let maxX = maximum $ map fst bytes
  let maxY = maximum $ map snd bytes
  print $ findAnswer ((maxX, maxY), Set.fromList (take 1024 bytes))
