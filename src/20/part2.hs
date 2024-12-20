import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Bounds = (Int, Int)

type Pos = (Int, Int)

type DistMap = Map Pos Int

mhd :: Pos -> Pos -> Int
mhd (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

inBounds :: Bounds -> Pos -> Bool
inBounds (n, m) (i, j) = i >= 1 && j >= 1 && i <= n && j <= m

neighbors :: Bounds -> Set Pos -> Pos -> Set Pos
neighbors bounds walls (i, j) =
  Set.fromList
    . filter (`Set.notMember` walls)
    . filter (inBounds bounds)
    $ [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

distMap :: Bounds -> Set Pos -> Int -> Set Pos -> Set Pos -> DistMap
distMap _ _ _ current _ | Set.null current = Map.empty
distMap bounds walls i current seen = Map.union newDistances $ distMap bounds walls (i + 1) newCurrent newSeen
  where
    newCurrent = Set.unions (Set.map (neighbors bounds walls) current) `Set.difference` seen
    newSeen = Set.union seen current
    newDistances = Map.fromList [(p, i) | p <- Set.toList current]

cheatDist :: DistMap -> DistMap -> (Pos, Pos) -> Int
cheatDist startMap endMap (c1, c2) = mhd c1 c2 + sd + ed
  where
    sd = minimum $ map (startMap !) [c1, c2]
    ed = minimum $ map (endMap !) [c1, c2]

findAnswer :: Bounds -> Set Pos -> Pos -> Pos -> Int
findAnswer bounds@(n, m) walls start end =
  length
    . filter ((>= 100) . (noCheatsDist -) . cheatDist startMap endMap)
    $ [ (p1, p2)
        | p1 <- [(i, j) | i <- [1 .. n], j <- [1 .. m]],
          p2 <- [(i, j) | i <- [1 .. n], j <- [1 .. m]],
          p1 < p2,
          mhd p1 p2 <= 20,
          p1 `Set.notMember` walls,
          p2 `Set.notMember` walls
      ]
  where
    startMap = distMap bounds walls 0 (Set.singleton start) Set.empty
    endMap = distMap bounds walls 0 (Set.singleton end) Set.empty
    noCheatsDist = startMap ! end

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let ps = [(i, j, x) | (i, line) <- zip [1 ..] input, (j, x) <- zip [1 ..] line]
  let start = head [(i, j) | (i, j, x) <- ps, x == 'S']
  let end = head [(i, j) | (i, j, x) <- ps, x == 'E']
  let walls = Set.fromList [(i, j) | (i, j, x) <- ps, x == '#']
  let n = Set.findMax $ Set.map fst walls
  let m = Set.findMax $ Set.map snd walls
  let bounds = (n, m)
  print $ findAnswer bounds walls start end
