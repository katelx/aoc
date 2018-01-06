module Main where
import Runner

import qualified Data.Map as M

start = ((M.fromList . zip ['a'..'h'] . repeat $ 0, 0), 0)

run is ((rs, p), ms) = case i of
  "set" -> ((M.insert hr v rs, succ p), ms)
  "sub" -> ((M.adjust (subtract v) hr rs, succ p), ms)
  "mul" -> ((M.adjust (* v) hr rs, succ p), succ ms)
  "jnz" -> ((rs, (if rv == 0 then succ else (+ v)) p), ms)
  where [i, r, y] = (M.!) is p
        v = e y
        hr = head r
        rv = e r
        e c = case M.lookup (head c) rs of
          Just q -> q
          _ -> read c

first is = snd . head . dropWhile valid . iterate (run is) $ start
  where valid ((_, p), _) = M.member p is

second _ = length . filter (not . flip elem prs) . map nx $ [0..mx]
  where nx n = 106700 + n * 17
        mx = 1000
        prs = takeWhile (< nx mx) (pg [2..])
        pg (p:ns) = p:pg [n | n <- ns, mod n p /= 0]

solve solver = show . solver . M.fromList . zip [0..] . map words . lines

main = runDay 23 [solve first, solve second]
