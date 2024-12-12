import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Set (Set)
import qualified Data.Set as Set

neighbors :: Matrix Char -> (Int, Int) -> Set (Int, Int)
neighbors m (i, j) =
  Set.fromList
    [ (ni, nj)
      | (ni, nj) <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)],
        Just (m ! (i, j)) == Matrix.safeGet ni nj m
    ]

region :: Matrix Char -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
region _ seen current | Set.null current = seen
region m seen current = region m newSeen newCurrent
  where
    newCurrent = Set.unions (Set.map (neighbors m) current) `Set.difference` seen
    newSeen = seen `Set.union` current

regions :: Matrix Char -> Set (Int, Int) -> [Set (Int, Int)]
regions _ unseen | Set.null unseen = []
regions m unseen = newRegion : regions m newUnseen
  where
    start = Set.elemAt 0 unseen
    newRegion = region m Set.empty (Set.singleton start)
    newUnseen = unseen `Set.difference` newRegion

perimeters :: Matrix Char -> Set (Int, Int) -> Int
perimeters m = sum . map (\p -> 4 - Set.size (neighbors m p)) . Set.toList

price :: Matrix Char -> Set (Int, Int) -> Int
price m r = Set.size r * perimeters m r

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
