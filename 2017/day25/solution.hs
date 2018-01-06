module Main where
import Runner

import Data.Char(ord)
import Data.List.Split(chunksOf)
import qualified Data.Set as S


parses [st, cs] = (pst st, pcs cs)
  where pst = (subtract . ord $ 'A') . ord . head . init . last
        pcs = read . last . init

parsed [_, _, v0, d0, n0, _, v1, d1, n1] = (ins v0 d0 n0, ins v1 d1 n1)
  where ins v d n = (pv v, if pd d == "right" then 1 else -1, pn n)
        pv = read . init . last
        pd = init . last
        pn = (subtract . ord $ 'A') . ord . head . init . last

run def ((state, pos), tape) = next (if S.member pos tape then n1 else n0)
    where next (v, d, n) = ((n, pos + d), (if v == 1 then S.insert else S.delete) pos tape)
          (n0, n1) = def !! state

solve inp = show . S.size . snd . head . drop chksum . iterate (run def) $ ((start, 0), S.empty)
 where ls = filter (not . null) . lines $ inp
       def = map parsed . chunksOf 9 . map words $ (drop 2 ls)
       (start, chksum) = parses (map words . take 2 $ ls)

main = runDay 25 [solve]
