module Main where
import Runner
import Data.List(foldl1)

items = map (foldl1 add) [[w, a, rl, rr] | w <- ws, a <- as, rl <- rs, rr <- rs, rl /= rr || rl == nil]
  where nil = (0, 0, 0)
        ws = weapons
        as = nil:armor
        rs = nil:rings
        add (a, b, c) (x, y, z) = (a+x, b+y, c+z)

data Turn = Player|Boss deriving (Show)

duel Player hp (bhp, bd, ba) (c, d, a)
  | reshp <= 0 = (c, True)
  | otherwise = duel Boss hp (reshp, bd, ba) (c, d, a)
  where reshp = bhp - max (d-ba) 1
duel Boss hp (bhp, bd, ba) (c, d, a)
  | reshp <= 0 = (c, False)
  | otherwise = duel Player reshp (bhp, bd, ba) (c, d, a)
  where reshp = hp - max (bd-a) 1

solve agg op inp = show . agg . map fst . filter (op . snd) . map (duel Player 100 boss) $ items
  where [hp, d, a] = map (read . last . words) . lines $ inp
        boss = (hp, d, a)

main = runDay 21 [solve minimum id, solve maximum not]

weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]

armor = [(13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]

rings = [(25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]
