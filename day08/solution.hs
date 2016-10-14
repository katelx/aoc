module Main where
import Runner

countMem [] = 0
countMem ('\\':'\\':xs) = 1 + countMem xs
countMem ('\\':'\"':xs) = 1 + countMem xs
countMem ('\\':'x':_:_:xs) = 1 + countMem xs
countMem (x:xs) = 1 + countMem xs

memSolve str = length str - countMem (init . tail $ str)

countEsc [] = 2
countEsc ('\\':xs) = 2 + countEsc xs
countEsc ('\"':xs) = 2 + countEsc xs
countEsc (x:xs) = 1 + countEsc xs

escSolve str = countEsc str - length str

solve solver = show . sum . map solver . lines

main = runDay 8 [solve memSolve, solve escSolve]
