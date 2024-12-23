import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (intercalate, maximumBy)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Network = Map String (Set String)

bronKerboschInner :: Network -> Set String -> Set String -> Set String -> [String] -> [Set String]
bronKerboschInner _ _ _ _ [] = []
bronKerboschInner m r p x (v : vs) =
  bronKerbosch m (Set.insert v r) (Set.intersection p nv) (Set.intersection x nv)
    ++ bronKerboschInner m r newP newX vs
  where
    nv = m ! v
    newP = Set.delete v p
    newX = Set.insert v x

bronKerbosch :: Network -> Set String -> Set String -> Set String -> [Set String]
bronKerbosch _ r p x | Set.null p && Set.null x = [r | not (Set.null r)]
bronKerbosch m r p x = bronKerboschInner m r p x vs
  where
    u = maximumBy (compare `on` (Set.size . (m !))) $ Set.union p x
    vs = Set.toList $ p `Set.difference` (m ! u)

findAnswer :: Network -> String
findAnswer m =
  intercalate ","
    . Set.toList
    . maximumBy (compare `on` Set.size)
    $ bronKerbosch m Set.empty (Map.keysSet m) Set.empty

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "list not of length 2"

parseLine :: String -> (String, String)
parseLine = tuplify . splitOn "-"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let dirConns = map parseLine input
  let conns = dirConns ++ map swap dirConns
  let m = Map.fromListWith Set.union $ map (second Set.singleton) conns
  print $ findAnswer m
