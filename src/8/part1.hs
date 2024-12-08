import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

type Bounds = (Int, Int)

type Pos = (Int, Int)

type Antennas = Map Char [Pos]

inBounds :: Bounds -> Pos -> Bool
inBounds (m, n) (i, j) = i > 0 && j > 0 && i <= m && j <= n

antinodes :: Bounds -> Pos -> Pos -> [Pos]
antinodes bounds (i1, j1) (i2, j2) = filter (inBounds bounds) [(i1 - di, j1 - dj), (i2 + di, j2 + dj)]
  where
    di = i2 - i1
    dj = j2 - j1

pairs :: [Pos] -> [(Pos, Pos)]
pairs ps = [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]

freqAntinodes :: Bounds -> [Pos] -> [Pos]
freqAntinodes bounds ants = [a | (p1, p2) <- pairs ants, a <- antinodes bounds p1 p2]

findAnswer :: Bounds -> Antennas -> Int
findAnswer bounds = length . nub . concat . Map.elems . fmap (freqAntinodes bounds)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let m = length input
  let n = length . head $ input
  let ants = [(x, [(i, j)]) | (i, line) <- zip [1 ..] input, (j, x) <- zip [1 ..] line, x /= '.']
  let antsMap = Map.fromListWith (++) ants
  print $ findAnswer (m, n) antsMap
