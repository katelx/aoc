module Main where
import Runner
import Data.List(sort)

parsep [] = []
parsep (p:v:ps) = (p, read v :: Int):parsep ps

parse sue = (n, props)
  where ((_, n):props) = parsep . words . filter (not . (`elem` ":,")) $ sue

cntEq t a | t == a = 1
cntEq _ _ = 0

cntSp (tp, tn) (ap, an) | tp /= ap = 0
cntSp ("cats", tn) (_, an) | tn < an = 1
cntSp ("trees", tn) (_, an) | tn < an = 1
cntSp ("pomerenians", tn) (_, an) | tn > an = 1
cntSp ("goldfish", tn) (_, an) | tn > an = 1
cntSp t a = cntEq t a

summ cnt (n, props) = (sum [cnt t a | t <- tt, a <- props], n)

solve solver = show . snd . last . sort . map (summ solver) . map parse . lines

tt = [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)]

main = runDay 16 [solve cntEq, solve cntSp]
