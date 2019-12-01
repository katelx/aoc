
f = pred . pred . flip div 3
r = sum . tail . takeWhile (> 0) . iterate f

main = let c v = sum . map v . map read . words <$> readFile "input" >>= print in c f >> c r
