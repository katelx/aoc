module Main where
import Runner

spiral = make_spiral 2 
  where make_spiral n = [(1, 0)] ++ replicate (n-1) (0, 1) ++ replicate n (-1, 0) ++ replicate n (0, -1) ++ replicate n (1, 0) ++ make_spiral (n+2)

cord_sum (x1, y1) (x2, y2) = (x1+x2, y1+y2)

cord_spiral = scanl cord_sum (0, 0) spiral

adj_spiral adjs ((x, y):cords) = res : adj_spiral (adjs ++ [res]) cords
  where res = ((x, y), sum . map adj_val $ adjs)
        adj_val ((a, b), v)
          | a == x && b == y = 0
          | abs (a-x) < 2 && abs (b-y) < 2 = v
          | otherwise = 0

dist n = abs x + abs y
  where (x, y) = cord_spiral !! (n-1)

max_sum n = head . dropWhile (<= n) . map snd . adj_spiral [((0, 0), 1)] $ cord_spiral

solve solver = show . solver . read

main = runDay 3 [solve dist, solve max_sum]
