import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Robot = ((Int, Int), (Int, Int))

dims :: (Int, Int)
dims = (101, 103)

step :: Robot -> Robot
step ((x, y), (dx, dy)) = (((x + dx) `mod` dimx, (y + dy) `mod` dimy), (dx, dy))
  where
    (dimx, dimy) = dims

stepAll :: [Robot] -> [Robot]
stepAll = map step

positions :: [Robot] -> Set (Int, Int)
positions = Set.fromList . map fst

maybeTree :: [Robot] -> Bool
maybeTree robots = length withNbs > length robots `div` 2
  where
    poss = positions robots
    withNbs = filter (\((x, y), _) -> any (`Set.member` poss) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]) robots

findAnswer :: [Robot] -> (Int, [Robot])
findAnswer = fromJust . find (maybeTree . snd) . zip [0 ..] . iterate stepAll

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "list not of size 2"

parseTup :: String -> (Int, Int)
parseTup = tuplify . map read . splitOn ","

parseRobot :: String -> Robot
parseRobot = tuplify . map (parseTup . (!! 1) . splitOn "=") . words

picture :: [Robot] -> String
picture robots = unlines [[if (x, y) `Set.member` poss then 'X' else '.' | x <- [0 .. dimx - 1]] | y <- [0 .. dimy - 1]]
  where
    (dimx, dimy) = dims
    poss = positions robots

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let robots = map parseRobot input
  let (i, tree) = findAnswer robots
  putStrLn $ picture tree
  print i
