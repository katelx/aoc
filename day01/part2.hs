val '(' = 1
val ')' = -1

main = readFile "input.txt" >>= return . length . takeWhile (>=0) . scanl (+) 0 . map val >>= print
