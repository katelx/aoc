module Main where
import Data.List.Split
import Data.List
import Runner

data Op = Row Int Int|Col Int Int|Rect Int Int

parse [_,"row",a,_,b] = Row (read . drop 2 $ a) (read b)
parse [_,"column",a,_,b] = Col (read . drop 2 $ a) (read b)
parse [_,ab] = Rect a b where [a, b] = map read . splitOn "x" $ ab

screen = replicate 6 . replicate 50 $ ' '

op scr (Row a b) = take a scr ++ [new] ++ drop (a+1) scr
  where old = reverse (scr !! a)
        new = reverse (take b old) ++ reverse (drop b old)
op scr (Col a b) = transpose $ op (transpose scr) (Row a b)
op scr (Rect a b) = onx scr 0
  where onx (r:rs) x | x < b = ony r 0:onx rs (x+1)
        onx rs _ = rs
        ony (c:cs) y | y < a = '#':ony cs (y+1)
        ony cs _ = cs

count = show . length . filter (=='#') . concat
code = (++) "\n" . unlines

solve solver = solver . foldl op screen . map (parse . words) . lines

main = runDay 8 [solve count, solve code]
