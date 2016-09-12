import Data.List(groupBy)

parse = map read . filter (/= "x") . groupBy (\x y -> x /= 'x' && y /= 'x')

calc [h, l, w] = (+ minimum ar) . sum $ map (*2) ar where ar = [h*l, h*w, l*w]

main = readFile "input.txt" >>= return . sum . map calc . map parse . words >>= print
