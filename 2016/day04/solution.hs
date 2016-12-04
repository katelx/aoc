module Main where
import Runner
import Data.List(break,span,sortBy)
import Data.Char(ord,chr,isDigit,isAlpha)

parse s = (id, (name, chk))
  where (name', s') = break isDigit s
        (id', s'') = span isDigit s'
        id = read id'
        name = filter isAlpha name'
        chk = init . tail $ s''

checksum = take 5 . map snd . sortBy numal . foldl calc []
  where calc [] a = [(1, a)]
        calc ((n, x):xs) a | x == a = (n+1, x):xs
        calc (x:xs) a = x:calc xs a
        numal (n1, a1) (n2, a2) = case compare n2 n1 of
                                  EQ -> compare a1 a2
                                  gl -> gl

shift n = map shc
  where shc c = chr $ mod ((ord c - orda) + n) numa + orda
        orda = ord 'a'
        numa = ord 'z' - orda + 1

real (_, (name, chk)) = chk == checksum name

true (id, (name, _)) = shift id name == "northpoleobjectstorage"

solve solver = show . sum . map fst . filter solver . map parse . lines

main = runDay 4 [solve real, solve true]
