module Main where
import Runner
import Data.List(group,sort)

data Dirs = N|W|S|E

parse (t:n) = (t, read n)

toNE (N,_) ('L',n) = (W,(-n,0))
toNE (W,_) ('L',n) = (S,(0,-n))
toNE (S,_) ('L',n) = (E,(n,0))
toNE (E,_) ('L',n) = (N,(0,n))
toNE (N,_) ('R',n) = (E,(n,0))
toNE (W,_) ('R',n) = (N,(0,n))
toNE (S,_) ('R',n) = (W,(-n,0))
toNE (E,_) ('R',n) = (S,(0,-n))

sumxy a b = (fst a + fst b, snd a + snd b)

absxy (x,y) = abs x + abs y

explode (x, 0) = [(signum x, 0)|_<-[1..abs x]]
explode (_, y) = [(0, signum y)|_<-[1..abs y]]

visited (c:cs) = if elem c cs then c else visited cs

dist = foldl sumxy (0,0)

double = visited . scanl sumxy (0, 0) . concat . map explode

solve solver = show . absxy . solver . map snd . scanl toNE (N,(0,0)) . map (parse . init) . words

main = runDay 1 [solve dist, solve double]
