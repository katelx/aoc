module Main where
import Runner

gen l a
  | length a >= l = take l a
  | otherwise = gen l $ a ++ "0" ++ b
  where b = map neg . reverse $ a
        neg '0' = '1'
        neg '1' = '0'

chk cs
  | even . length $ cs = chk . trans $ cs
  | otherwise = cs
  where trans [] = []
        trans (a:b:cs) = (if a == b then '1' else '0'):trans cs
  

solve solver = chk . gen solver

main = runDay 16 [solve 272, solve 35651584]
