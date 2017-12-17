module Main where
import Runner

import Data.List.Split
import Data.Char(ord)
import Data.List(foldl1,zipWith,nub,sort)
import Data.Bits(xor)
import Text.Printf
import qualified Data.Set as S

max_len = 256

rev pos len list
  | npos <= max_len = take pos list ++ revd ++ drop npos list
  | otherwise = drop (len - carry) revd ++ (take (max_len - len) . drop carry $ list) ++ take (max_len - pos) revd  
  where npos = pos + len
        carry = mod npos max_len
        revd = reverse . take len . drop pos . cycle $ list
  
hash (list, (skip, pos)) len = (rev pos len list, (skip + 1, mod (pos + len + skip) max_len))

knot = concat . map (printf "%08b") . map (foldl1 xor) . chunksOf 16 . fst . foldl hash ([0..max_len-1], (0, 0)) . concat . replicate 64 . (++ [17, 31, 73, 47, 23]) . map ord

squares = filter (== '1') . concat

valid disk visited (x, y)
  | x < 0 || y < 0 || x > 127 || y > 127 = False
  | disk !! x !! y == '0' = False
  | S.member (x, y) visited = False
  | otherwise = True

mark disk visited xys
  | null vxys = []
  | null nxys = vxys
  | otherwise = vxys ++ mark disk nvisited nxys 
  where nxys = filter (valid disk visited) (vxys >>= nexts)
        vxys = filter (valid disk visited) xys
        nexts (x, y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
        nvisited = foldl add visited vxys
        add d (a, b) = S.insert (a, b) d

adj disk (x, y) = mark disk S.empty [(x, y)] 

groups disk = filter (not . null) . nub . map (sort . nub) . map (adj disk) $ [(x, y) | x <- [0..127], y <- [0..127]]

solve solver = show . length . solver . map knot . zipWith row [0..127] . repeat
  where row n i = i ++ "-" ++ show n


main = runDay 14 [solve squares, solve groups]
