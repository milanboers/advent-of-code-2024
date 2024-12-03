import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, many, parse, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

tup :: Parser (Int, Int)
tup = (,) <$> decimal <* char ',' <*> decimal

mul :: Parser (Int, Int)
mul = string "mul(" *> tup <* char ')'

maybeMul :: Parser (Maybe (Int, Int))
maybeMul = Just <$> try mul <|> Nothing <$ anySingle

muls :: Parser [(Int, Int)]
muls = catMaybes <$> many maybeMul

parseContents :: String -> [(Int, Int)]
parseContents x = fromRight [] (parse muls "" x)

findAnswer :: [(Int, Int)] -> Int
findAnswer xs = sum [x * y | (x, y) <- xs]

main :: IO ()
main = do
  contents <- getContents
  let parsed = parseContents contents
  print $ findAnswer parsed
