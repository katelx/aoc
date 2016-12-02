module Main where
import Runner

digs p 'U' | not $ elem p [1, 2, 3] = p - 3
digs p 'D' | not $ elem p [7, 8, 9] = p + 3
digs p 'L' | not $ elem p [1, 4, 7] = p - 1
digs p 'R' | not $ elem p [3, 6, 9] = p + 1
digs p _ = p

digd p 'U' | elem p [3, 13] = p - 2
digd p 'U' | elem p [6, 7, 8, 10, 11, 12] = p - 4
digd p 'D' | elem p [1, 11] = p + 2
digd p 'D' | elem p [2, 3, 4, 6, 7, 8] = p + 4
digd p 'L' | not $ elem p [1, 2, 5, 10, 13] = p - 1
digd p 'R' | not $ elem p [1, 4, 9, 12, 13] = p + 1
digd p _ = p

digits folder (curr, res) proc = (next, next:res)
  where next = foldl folder curr proc

toHex "10" = "A"
toHex "11" = "B"
toHex "12" = "C"
toHex "13" = "D"
toHex d = d

solve solver = concat . map (toHex . show) . reverse . snd . foldl (digits solver) (5, []) . lines

main = runDay 2 [solve digs, solve digd]
