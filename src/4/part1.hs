import Data.List (isPrefixOf)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

wordInDir :: Matrix Char -> (Int, Int) -> (Int, Int) -> String
wordInDir m (i, j) d@(di, dj) = case Matrix.safeGet i j m of
  Just x -> x : wordInDir m (i + di, j + dj) d
  Nothing -> []

findAnswer :: Matrix Char -> Int
findAnswer m = length $ filter ("XMAS" `isPrefixOf`) ws
  where
    starts = [(i, j) | i <- [1 .. Matrix.nrows m], j <- [1 .. Matrix.ncols m]]
    dirs = [(di, dj) | di <- [-1 .. 1], dj <- [-1 .. 1], (di, dj) /= (0, 0)]
    ws = [wordInDir m start dir | start <- starts, dir <- dirs]

main :: IO ()
main = do
  contents <- getContents
  let m = Matrix.fromLists $ lines contents
  print $ findAnswer m
