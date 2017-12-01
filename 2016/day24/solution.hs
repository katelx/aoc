module Main where
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char(ord)
import Data.Maybe
import Runner

import Debug.Trace

data Maze = Open|Wall|Mark Int deriving(Show)

val '#' = Wall
val '.' = Open
val n = Mark (ord n - ord '0')

isMark (Mark _) = True
isMark _ = False
fromMaze (Mark x) = x

parse = M.fromList . concat . map cells . zip [0..] . init . tail . lines
  where cells (y, row) = map (cell y) . zip [0..] . tail . init $ row
        cell y (x, v) = ((x, y), val v)

step maze vis (x, y) = map fst . M.toList . restrictKeys maze $ S.fromList $ filter ok [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
  where ok xy
          | S.member xy vis = False
	  | otherwise = pass xy
	pass xy = case M.lookup xy maze of
                    Nothing -> False
                    Just Wall -> False
                    _ -> True
        restrictKeys m s = M.filterWithKey (\k _ -> k `S.member` s) m

--dist maze s d vis cnt | trace (show (length s) ++ " " ++ show (S.size vis) ++ " " ++ show cnt) False = undefined
dist maze s d vis cnt
  | S.member d nvis = cnt
  | otherwise = dist maze ((nub s) >>= step maze vis) d nvis (cnt+1)
  where nvis = S.unions [vis, S.fromList s]

dists maze = [((fromMaze (snd a), fromMaze(snd b)), dist maze [fst a] (fst b) (S.singleton (fst a)) 0) | a <- marks, b <- marks, fst a < fst b]
  where marks = M.toList . M.filter isMark $ maze

dbl ((x, y), d) = [((x,y), d), ((y,x), d)]

perms = map (\l -> [0]++l++[0]) (permutations [1..7])

sm d (x:y:r) = fromJust (M.lookup (x, y) d) + sm d (y:r)
sm _ _ = 0

mn d = minimum $ map (sm d) perms

solve solver = show . mn . M.fromList . concat . map dbl . dists . parse

main = runDay 24 [solve id]
