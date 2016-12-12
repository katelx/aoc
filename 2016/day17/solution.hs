module Main where
import Runner
import Data.Hex
import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char
import Data.Function

import Debug.Trace

step e@((x, y), key)
  | (x, y) == (3, -3) = [e]
  | otherwise = filter ok [
          v u (0,1) 'U',
          v d (0,-1) 'D',
          v l (-1,0) 'L',
          v r (1, 0) 'R']
  where (u:d:l:r:_) = B.unpack . hex . hash . B.pack $ key
        v s (a, b) c | elem s "BCDEF" = ((x+a, y+b), key ++ [c])
        v _ _ _ = ((-1, -1), [])
        ok ((x, y), _) = x >= 0 && y <= 0 && x <= 3 && y >= -3

--walk res | trace (show res) False = undefined
walk cmp res
  | all done res = dropWhile isLower . snd . head . sortBy (on compare (cmp . length . snd)) $ res
  | otherwise = walk cmp (res >>= step)
  where done = (==(3,-3)) . fst

solve solver solver2 = solver2 . walk solver . return . (,) (0, 0)

main = runDay 17 [solve id id, solve ((*) (-1)) (show . length)]
