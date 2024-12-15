{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Dir = (Int, Int)

type Field = (Set Pos, Set Pos) -- walls, boxes

type State = (Field, Pos)

move :: Dir -> Pos -> Pos
move (di, dj) (i, j) = (i + di, j + dj)

boxesToMove :: Field -> Pos -> Dir -> Maybe (Set Pos)
boxesToMove f@(walls, boxes) p d
  | nextPos `Set.member` walls = Nothing
  | nextPos `Set.member` boxes && d == (0, 1) = Set.insert nextPos <$> boxesToMove f rightOfNextPos d
  | leftOfNextPos `Set.member` boxes && d == (0, -1) = Set.insert leftOfNextPos <$> boxesToMove f leftOfNextPos d
  | nextPos `Set.member` boxes = case (boxesToMove f nextPos d, boxesToMove f rightOfNextPos d) of
      (Just x, Just y) -> Just $ Set.insert nextPos (Set.union x y)
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
  | leftOfNextPos `Set.member` boxes = case (boxesToMove f leftOfNextPos d, boxesToMove f nextPos d) of
      (Just x, Just y) -> Just $ Set.insert leftOfNextPos (Set.union x y)
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
  | otherwise = Just Set.empty
  where
    nextPos = move p d
    leftOfNextPos = move nextPos (0, -1)
    rightOfNextPos = move nextPos (0, 1)

moveBoxes :: Dir -> Field -> Set Pos -> Field
moveBoxes d (walls, boxes) bs = (walls, newBoxes)
  where
    newBoxes = Set.map (\p -> if p `Set.member` bs then move d p else p) boxes

moveSelf :: State -> Dir -> State
moveSelf (f, p) d = case boxesToMove f p d of
  Nothing -> (f, p)
  Just bs -> (moveBoxes d f bs, move d p)

doAllMoves :: State -> [Dir] -> State
doAllMoves = foldl' moveSelf

boxCoord :: Pos -> Int
boxCoord (i, j) = 100 * (i - 1) + (j - 1)

findAnswer :: State -> [Dir] -> Int
findAnswer s moves = sum . map boxCoord $ Set.toList endBoxes
  where
    ((_, endBoxes), _) = doAllMoves s moves

parseMove :: Char -> Dir
parseMove '^' = (-1, 0)
parseMove '>' = (0, 1)
parseMove '<' = (0, -1)
parseMove 'v' = (1, 0)
parseMove _ = error "invalid move "

expand :: String -> String
expand [] = []
expand ('#' : xs) = '#' : '#' : expand xs
expand ('O' : xs) = 'O' : '.' : expand xs
expand ('.' : xs) = '.' : '.' : expand xs
expand ('@' : xs) = '@' : '.' : expand xs
expand _ = error "invalid map "

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawField, rawMoves] = splitOn [""] input
  let fps = [(i, j, x) | (i, line) <- zip [1 ..] rawField, (j, x) <- zip [1 ..] (expand line)]
  let walls = Set.fromList [(i, j) | (i, j, x) <- fps, x == '#']
  let boxes = Set.fromList [(i, j) | (i, j, x) <- fps, x == 'O']
  let pos = head [(i, j) | (i, j, x) <- fps, x == '@']
  let moves = map parseMove (concat rawMoves)
  print $ findAnswer ((walls, boxes), pos) moves
