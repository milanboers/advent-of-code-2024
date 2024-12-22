import Data.Bits (xor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

step :: (Int -> Int) -> Int -> Int
step f x = (f x `xor` x) `mod` 16777216

nextNum :: Int -> Int
nextNum x = foldr step x [(* 2048), (`div` 32), (* 64)]

nums :: Int -> [Int]
nums = iterate nextNum

prices :: [Int] -> [Int]
prices = map (`mod` 10)

withDeltas :: [Int] -> [(Int, Int)]
withDeltas (x : y : ys) = (y, y - x) : withDeltas (y : ys)
withDeltas _ = []

seqAndPrices :: [(Int, Int)] -> Map (Int, Int, Int, Int) Int
seqAndPrices ((_, d0) : pd1@(_, d1) : pd2@(_, d2) : pd3@(p3, d3) : pds) =
  Map.insert (d0, d1, d2, d3) p3 (seqAndPrices (pd1 : pd2 : pd3 : pds))
seqAndPrices _ = Map.empty

findAnswer :: [Int] -> Int
findAnswer =
  maximum
    . Map.unionsWith (+)
    . map (seqAndPrices . take 2001 . withDeltas . prices . nums)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let secrets = map read input
  print $ findAnswer secrets
