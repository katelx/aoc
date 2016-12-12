module Main where
import qualified Data.Set as S
import Runner

sys fav = fun
  where fun (x, y) | x < 0 || y < 0 = False
        fun (x, y) = even . bits . (+fav) . calc $ (x, y)
        calc (x, y) = x*x+3*x+2*x*y+y+y*y
        bits 0 = 0
        bits x = if odd x then 1 + bits' else bits' where bits' = bits (div x 2)

step fun ((x, y), path) = map next . filter visited $ dirs
  where dirs = filter fun [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        visited = not . (flip S.member $ path)
        next dir = (dir, S.insert dir path)

search dest cnt visited fun
  | any (S.member dest . snd) visited = cnt
  | otherwise = search dest (cnt+1) next fun
  where next = visited >>= step fun

distinct mx reached cnt visited fun
  | cnt > mx = S.size reached
  | otherwise = distinct mx reached' (cnt+1) next fun
  where next = visited >>= step fun
        reached' = S.unions . ((:) reached) . map snd $ visited

start = [((1, 1), S.singleton (1, 1))]

solve solver = show . solver 0 start . sys . read

main = runDay 13 [solve (search (31, 39)), solve (distinct 50 S.empty)]
