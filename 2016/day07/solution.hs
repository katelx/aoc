module Main where
import Data.List.Split
import Data.List
import Runner

spl = extr . partition (odd . fst) . zip [1..] . splitOneOf "[]"
  where extr (o, i) = (map snd o, map snd i)

tls addr = any abba o && all (not . abba) i
  where (o, i) = spl addr
        abba w@(a:b:c:d:_) = a /= b && a == d && b == c || abba (tail w)
        abba _ = False

ssl addr = not . null $ intersectBy eq (o >>= aba) (i >>= aba)
  where (o, i) = spl addr
        aba w@(a:b:c:_) | a /= b && a == c = [a, b, c]:abas
                        | otherwise = abas
                        where abas = aba (tail w)
        aba _ = []
        eq (a1:b1:_) (a2:b2:_) = a1 == b2 && a2 == b1

solve solver = show . length . filter solver . lines

main = runDay 7 [solve tls, solve ssl]
