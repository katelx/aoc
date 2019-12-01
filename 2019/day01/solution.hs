
f = pred . pred . flip div 3
r v | v' <= 0 = 0 | otherwise = v' + r v' where v' = f v

main = let
  i = map read . words <$> readFile "input"
  c v = sum . map v <$> i >>= print in c f >> c r
