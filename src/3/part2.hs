import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, many, parse, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Term = Mul (Int, Int) | Do | Dont deriving (Show, Eq)

type Parser = Parsec Void String

tup :: Parser (Int, Int)
tup = (,) <$> decimal <* char ',' <*> decimal

mul :: Parser Term
mul = string "mul(" *> (Mul <$> tup) <* char ')'

do' :: Parser Term
do' = Do <$ string "do()"

dont :: Parser Term
dont = Dont <$ string "don't()"

term :: Parser Term
term = mul <|> do' <|> dont

maybeTerm :: Parser (Maybe Term)
maybeTerm = Just <$> try term <|> Nothing <$ anySingle

terms :: Parser [Term]
terms = catMaybes <$> many maybeTerm

parseContents :: String -> [Term]
parseContents x = fromRight [] (parse terms "" x)

run :: Bool -> [Term] -> Int
run _ [] = 0
run True ((Mul (x, y)) : xs) = x * y + run True xs
run False ((Mul _) : xs) = run False xs
run _ (Do : xs) = run True xs
run _ (Dont : xs) = run False xs

findAnswer :: [Term] -> Int
findAnswer = run True

main :: IO ()
main = do
  contents <- getContents
  let parsed = parseContents contents
  print $ findAnswer parsed
