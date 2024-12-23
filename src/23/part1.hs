import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Network = Map String (Set String)

findAnswer :: Network -> Int
findAnswer m =
  Set.size $
    Set.fromList
      [ Set.fromList [a, b, c]
        | a <- nodes,
          head a == 't',
          b <- nodes,
          a `Set.member` (m ! b),
          c <- nodes,
          b `Set.member` (m ! c),
          c `Set.member` (m ! a)
      ]
  where
    nodes = Map.keys m

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
