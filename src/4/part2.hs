import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (mapMaybe)

isMas :: Matrix Char -> [(Int, Int)] -> Bool
isMas m cs = w == "MAS" || reverse w == "MAS"
  where
    w = mapMaybe (\(i, j) -> Matrix.safeGet i j m) cs

isXmas :: Matrix Char -> (Int, Int) -> Bool
isXmas m (i, j) = isMas m w1 && isMas m w2
  where
    w1 = [(i - 1, j - 1), (i, j), (i + 1, j + 1)]
    w2 = [(i - 1, j + 1), (i, j), (i + 1, j - 1)]

findAnswer :: Matrix Char -> Int
findAnswer m = length $ filter (isXmas m) starts
  where
    starts = [(i, j) | i <- [1 .. Matrix.nrows m], j <- [1 .. Matrix.ncols m]]

main :: IO ()
main = do
  contents <- getContents
  let m = Matrix.fromLists $ lines contents
  print $ findAnswer m
