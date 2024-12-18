import Data.List (inits)
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

bfs :: Field -> Pos -> Set Pos -> Set Pos -> Bool
bfs _ _ _ current | Set.null current = False
bfs _ end _ current | end `Set.member` current = True
bfs field end seen current = bfs field end newSeen newCurrent
  where
    newCurrent = Set.unions (Set.map (neighbors field) current) `Set.difference` seen
    newSeen = seen `Set.union` current

binSearch :: Bounds -> [[Pos]] -> Pos
binSearch _ poss | length poss == 1 = last . head $ poss
binSearch bounds poss =
  if canReach
    then binSearch bounds (drop (middlei + 1) poss)
    else binSearch bounds (take (middlei + 1) poss)
  where
    middlei = (length poss - 1) `div` 2
    canReach = bfs (bounds, Set.fromList (poss !! middlei)) bounds Set.empty (Set.singleton (0, 0))

findAnswer :: Bounds -> [Pos] -> Pos
findAnswer bounds bytes = binSearch bounds (inits bytes)

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
  let (x, y) = findAnswer (maxX, maxY) bytes
  putStrLn $ show x ++ "," ++ show y
