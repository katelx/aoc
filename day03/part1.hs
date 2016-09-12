import Data.List(nub)

val '^' = (0, 1)
val '>' = (1, 0)
val 'v' = (0, -1)
val '<' = (-1, 0)

tupAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

main = readFile "input.txt" >>= return . length . nub . scanl tupAdd (0, 0) . map val >>= print
