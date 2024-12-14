import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Robot = ((Int, Int), (Int, Int))

posAfter :: (Int, Int) -> Int -> Robot -> Robot
posAfter (dimx, dimy) steps ((x, y), (dx, dy)) = (((x + dx * steps) `mod` dimx, (y + dy * steps) `mod` dimy), (dx, dy))

quadrant :: (Int, Int) -> Robot -> Maybe (Int, Int)
quadrant (dimx, dimy) ((x, y), _)
  | x == halfx || y == halfy = Nothing
  | otherwise = Just (x `div` (halfx + 1), y `div` (halfy + 1))
  where
    halfx = dimx `div` 2
    halfy = dimy `div` 2

findAnswer :: [Robot] -> Int
findAnswer = product . map length . group . sort . mapMaybe (quadrant dims . posAfter dims 100)
  where
    -- dims = (11, 7)
    dims = (101, 103)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "list not of size 2"

parseTup :: String -> (Int, Int)
parseTup = tuplify . map read . splitOn ","

parseRobot :: String -> Robot
parseRobot = tuplify . map (parseTup . (!! 1) . splitOn "=") . words

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let robots = map parseRobot input
  print $ findAnswer robots
