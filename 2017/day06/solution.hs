module Main where
import Runner

import Data.List(maximumBy)
import Data.Function(on)
import qualified Data.Map as M

dist rem pos banks
  | rem == 0 = banks
  | pos == M.size banks = dist rem 0 banks
  | otherwise = dist (rem-1) (pos+1) (M.adjust (+1) pos banks)

redist banks = dist rem (pos+1) (M.adjust (const 0) pos banks)
  where (pos, rem) = maximumBy (on compare swap) $ M.toList banks
        swap (a, b) = (b, -a)

find prev banks
  | M.member banks prev = (len, len - M.findWithDefault 0 banks prev)
  | otherwise = find (M.insert banks len prev) (redist banks)
    where len = M.size prev

solve ext = show . ext . find M.empty . M.fromList . zip [0..] . map read . words

main = runDay 6 [solve fst, solve snd]
