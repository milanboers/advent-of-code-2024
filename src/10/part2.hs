import Data.Char (digitToInt)
import Data.Matrix (Matrix (ncols, nrows), (!))
import qualified Data.Matrix as Matrix

type Pos = (Int, Int)

dirs :: [(Int, Int)]
dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

ends :: Matrix Int -> Pos -> Int
ends m (i, j) = case m ! (i, j) of
  9 -> 1
  x ->
    sum [ends m (ni, nj) | (di, dj) <- dirs, let (ni, nj) = (i + di, j + dj), Matrix.safeGet ni nj m == Just (x + 1)]

findAnswer :: Matrix Int -> Int
findAnswer m = sum [ends m (i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m], m ! (i, j) == 0]

main :: IO ()
main = do
  contents <- getContents
  let m = Matrix.fromLists [[digitToInt x | x <- line] | line <- lines contents]
  print $ findAnswer m
