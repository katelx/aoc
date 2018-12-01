module Main where
import Runner
import qualified Data.Set as S

solve solver = show . solver . map read . lines . filter (/= '+')

dup ps (n:ns) | S.member n ps = n | otherwise = dup (S.insert n ps) ns

main = runDay 1 [solve sum, solve (dup S.empty . scanl (+) 0 . cycle)]
