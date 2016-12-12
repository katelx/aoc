module Main where
import Runner
import Data.List

typ "^^." = '^'
typ ".^^" = '^'
typ "^.." = '^'
typ "..^" = '^'
typ _ = '.'

next (a:b:c:d) = typ [a,b,c]:next (b:c:d)
next _ = []

count r fr = length . filter (=='.') . concat . map snd . unfoldr bld $ (r, fr)
  where bld (cnt, row)
          | cnt > 0 = Just ((cnt, row), (cnt-1, next ("." ++ row ++ ".")))
          | otherwise = Nothing

solve solver = show . count solver

main = runDay 18 [solve 40, solve 400000]
