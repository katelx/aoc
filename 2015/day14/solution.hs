module Main where
import Runner
import Data.Char(isDigit)
import Data.List(replicate, scanl1, transpose)

parse row = (s, t, r) where [s, t, r] = map read . filter (all isDigit) $ row

calc ct mt True c@(s, t, _)
  | ct + t < mt = replicate t s ++ calc (ct + t) mt False c
  | otherwise = replicate (mt - ct) s
calc ct mt _ c@(_, _, r)
  | ct + r < mt = replicate r 0 ++ calc (ct + r) mt True c
  | otherwise = []

points ds = map (length . filter (\x -> fst x == snd x) . zip mxs) $ dsc
  where dsc = map (scanl1 (+)) ds
        mxs = map maximum . transpose $ dsc

solve solver = show . maximum . solver . map (calc 0 2503 True) . map parse . map words . lines

main = runDay 14 [solve (map sum), solve points]
