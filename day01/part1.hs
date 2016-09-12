val '(' = 1
val ')' = -1

main = readFile "input.txt" >>= return . sum . map val >>= print
