import qualified Data.Set as S

dup ps (n:ns) | S.member n ps = n | otherwise = dup (S.insert n ps) ns

main = let input = map read . lines . filter (/= '+') <$> readFile "input" in do
  sum <$> input >>= print
  dup S.empty . scanl (+) 0 . cycle <$> input >>= print
