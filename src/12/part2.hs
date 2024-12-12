import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = Hor | Ver deriving (Show, Eq, Ord)

type Fence = (Dir, (Int, Int), Bool) -- dir, pos, facing

cluster' :: (Ord a) => (a -> Set a) -> Set a -> Set a -> Set a
cluster' _ seen current | Set.null current = seen
cluster' nbs seen current = cluster' nbs newSeen newCurrent
  where
    newCurrent = Set.unions (Set.map nbs current) `Set.difference` seen
    newSeen = seen `Set.union` current

cluster :: (Ord a) => (a -> Set a) -> a -> Set a
cluster nbs start = cluster' nbs Set.empty (Set.singleton start)

allClusters :: (Ord a) => (a -> Set a) -> Set a -> [Set a]
allClusters _ unseen | Set.null unseen = []
allClusters nbs unseen = newCluster : allClusters nbs newUnseen
  where
    start = Set.elemAt 0 unseen
    newCluster = cluster nbs start
    newUnseen = unseen `Set.difference` newCluster

neighbors :: Matrix Char -> (Int, Int) -> Set (Int, Int)
neighbors m (i, j) =
  Set.fromList
    [ (ni, nj)
      | (ni, nj) <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)],
        Just (m ! (i, j)) == Matrix.safeGet ni nj m
    ]

regions :: Matrix Char -> Set (Int, Int) -> [Set (Int, Int)]
regions m = allClusters (neighbors m)

fences :: (Int, Int) -> Set Fence
fences (i, j) =
  Set.fromList
    [ (Hor, (i - 1, j - 1), True),
      (Hor, (i, j - 1), False),
      (Ver, (i - 1, j - 1), True),
      (Ver, (i - 1, j), False)
    ]

flipFence :: Fence -> Fence
flipFence (d, p, f) = (d, p, not f)

perimeterFences :: Matrix Char -> (Int, Int) -> Set Fence
perimeterFences m (i, j) = ownFences `Set.difference` Set.map flipFence neighborFences
  where
    ownFences = fences (i, j)
    neighborFences = Set.unions . Set.map fences $ neighbors m (i, j)

perimeters :: Matrix Char -> Set (Int, Int) -> Set Fence
perimeters m = Set.unions . Set.map (perimeterFences m)

adjacentFences :: Set Fence -> Fence -> Set Fence
adjacentFences ps (Hor, (i, j), f) = Set.fromList [(Hor, (i, j - 1), f), (Hor, (i, j + 1), f)] `Set.intersection` ps
adjacentFences ps (Ver, (i, j), f) = Set.fromList [(Ver, (i - 1, j), f), (Ver, (i + 1, j), f)] `Set.intersection` ps

sides :: Set Fence -> [Set Fence]
sides ps = allClusters (adjacentFences ps) ps

price :: Matrix Char -> Set (Int, Int) -> Int
price m r = Set.size r * length (sides $ perimeters m r)

findAnswer :: Matrix Char -> Int
findAnswer m =
  sum
    . map (price m)
    . regions m
    $ Set.fromList [(i, j) | i <- [1 .. Matrix.nrows m], j <- [1 .. Matrix.ncols m]]

main :: IO ()
main = do
  contents <- getContents
  let m = Matrix.fromLists (lines contents)
  print $ findAnswer m
