import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set

type NeighborsFunction = Char -> [(Char, Char)]

type Path = (String, Char) -- actions, end

numpadNbs :: Char -> [(Char, Char)]
numpadNbs '7' = [('>', '8'), ('v', '4')]
numpadNbs '8' = [('<', '7'), ('v', '5'), ('>', '9')]
numpadNbs '9' = [('<', '8'), ('v', '6')]
numpadNbs '4' = [('^', '7'), ('>', '5'), ('v', '1')]
numpadNbs '5' = [('^', '8'), ('<', '4'), ('>', '6'), ('v', '2')]
numpadNbs '6' = [('^', '9'), ('<', '5'), ('v', '3')]
numpadNbs '1' = [('^', '4'), ('>', '2')]
numpadNbs '2' = [('^', '5'), ('>', '3'), ('v', '0'), ('<', '1')]
numpadNbs '3' = [('^', '6'), ('<', '2'), ('v', 'A')]
numpadNbs '0' = [('^', '2'), ('>', 'A')]
numpadNbs 'A' = [('^', '3'), ('<', '0')]
numpadNbs _ = error "invalid keypad num"

dirpadNbs :: Char -> [(Char, Char)]
dirpadNbs '^' = [('v', 'v'), ('>', 'A')]
dirpadNbs 'A' = [('<', '^'), ('v', '>')]
dirpadNbs '<' = [('>', 'v')]
dirpadNbs 'v' = [('<', '<'), ('^', '^'), ('>', '>')]
dirpadNbs '>' = [('<', 'v'), ('^', 'A')]
dirpadNbs _ = error "invalid keypad dir"

shortestPaths :: NeighborsFunction -> Char -> [Path] -> Set Char -> [String]
shortestPaths _ end current _ | (not . null) fullPaths = map (reverse . fst) fullPaths
  where
    fullPaths = filter ((== end) . snd) current
shortestPaths nbs end current seen = shortestPaths nbs end newCurrent newSeen
  where
    newCurrent = [(nba : ca, nbe) | (ca, ce) <- current, (nba, nbe) <- nbs ce, nbe `Set.notMember` seen]
    newSeen = Set.union seen (Set.fromList $ map snd current)

inputSequences :: NeighborsFunction -> String -> [String]
inputSequences nbs (x : y : ys) =
  [xtoy ++ "A" ++ rest | xtoy <- shortestPaths nbs y [([], x)] Set.empty, rest <- inputSequences nbs (y : ys)]
inputSequences _ _ = [""]

shortestSequenceLength :: String -> Int
shortestSequenceLength l0 =
  minimum
    [ length l3
      | l1 <- inputSequences numpadNbs ('A' : l0),
        l2 <- inputSequences dirpadNbs ('A' : l1),
        l3 <- inputSequences dirpadNbs ('A' : l2)
    ]

numericPart :: String -> Int
numericPart = read . filter isDigit

findAnswer :: [String] -> Int
findAnswer = sum . map (\x -> numericPart x * shortestSequenceLength x)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
