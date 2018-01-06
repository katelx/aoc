module Main where
import Runner

import qualified Data.Map as M

burst emod ((m, xy@(x, y), d), i) = case M.lookup xy m of
  Just 3 -> ((M.insert xy emod m, xyr, dr), i)
  Just 2 -> ((M.insert xy 3 m, xyd, d), succ i)
  Just 1 -> ((M.insert xy 0 m, xyf, df), i)
  _ -> ((M.insert xy (3-emod) m, xyl, dl), i+1-emod)
  where dr = flip mod 4 . succ $ d
        dl = -1 + if d == 0 then 4 else d
        df = flip mod 4 . succ . succ $ d
        xyr = nxy dr
        xyl = nxy dl
        xyf = nxy df
        xyd = nxy d
        nxy v = ([0, 1, 0, -1] !! v + x, [-1, 0, 1, 0] !! v + y)

build ls = ((m, (12, 12), 0), 0)
  where m = M.fromList . concat . map cells . zip [0..] $ ls
        cells (y, r) = map (cell y) (zip r [0..])
        cell y (i, x) = ((x, y), if i == '#' then 3 else 0)

solve solver it = show . snd . head . drop it . iterate solver . build . lines

main = runDay 22 [solve (burst 0) 10000, solve (burst 1) 10000000]
