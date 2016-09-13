import Data.List(nub,partition)

val '^' = (0, 1)
val '>' = (1, 0)
val 'v' = (0, -1)
val '<' = (-1, 0)

tupAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

splitEvenOdd = map (map snd) . (\t -> [fst t, snd t]) . partition (even . fst) . zip [1..]

calcTrails = map $ scanl tupAdd (0, 0)

main = readFile "input.txt" >>= return . length . nub . concat . calcTrails . splitEvenOdd . map val >>= print
