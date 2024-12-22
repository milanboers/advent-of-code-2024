import Data.Bits (xor)

step :: (Int -> Int) -> Int -> Int
step f x = (f x `xor` x) `mod` 16777216

nextNum :: Int -> Int
nextNum x = foldr step x [(* 2048), (`div` 32), (* 64)]

nums :: Int -> [Int]
nums = iterate nextNum

findAnswer :: [Int] -> Int
findAnswer = sum . map ((!! 2000) . nums)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let secrets = map read input
  print $ findAnswer secrets
