import Data.List(sort,group,groupBy,maximumBy)
import Data.Function(on)
import Data.Char(isDigit)

parse [] = []
parse (l:ls) = (n '#' l, map (n ':') . takeWhile t $ ls):parse rs
  where t = not . elem '#'
        rs = dropWhile t ls
        n c = read . takeWhile isDigit . tail . dropWhile (/= c)

minutes (id, ms) = (id, concat . range $ ms)
  where range [] = []
        range (a:b:s) = [a..b-1]:range s

grouped = map join . groupBy (on (==) fst) . sort
  where join gs = (fst . head $ gs, sort . concatMap snd $ gs)

most f = mul . maximumBy (on compare (f . snd))
  where mul (g, ms) = g * head (maximumBy (on compare length) (group ms))

main = let input = grouped . map minutes . parse . sort . lines <$> readFile "input" in do
  most length <$> input >>= print
  most (maximum . (0 :) . map length . group) <$> input >>= print
