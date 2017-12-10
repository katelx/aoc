module Main where
import Runner

import Data.List.Split
import Data.Char(ord)
import Data.List(foldl1)
import Data.Bits(xor)
import Text.Printf

max_len = 256

rev pos len list
  | npos <= max_len = take pos list ++ revd ++ drop npos list
  | otherwise = drop (len - carry) revd ++ (take (max_len - len) . drop carry $ list) ++ take (max_len - pos) revd  
  where npos = pos + len
        carry = mod npos max_len
        revd = reverse . take len . drop pos . cycle $ list
  
hash (list, (skip, pos)) len = (rev pos len list, (skip + 1, mod (pos + len + skip) max_len))

solve parser r formater = formater . fst . foldl hash ([0..max_len-1], (0, 0)) . concat . replicate r . parser

parse1 = map read . splitOn ","

parse2 inp = map ord inp ++ [17, 31, 73, 47, 23]

format1 = show . product . take 2

format2 = concat . map (printf "%02x") . map (foldl1 xor) . chunksOf 16

main = runDay 10 [solve parse1 1 format1, solve parse2 64 format2]
