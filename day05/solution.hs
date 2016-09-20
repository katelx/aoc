module Day05(solvers) where

import Control.Applicative
import Data.List(sort, group, isInfixOf)

pairs (a:b:[]) = []
pairs (a:b:p) = (a, b):pairs (b:p)

cntPairs (wa:wb:w) (a,b)
  | wa == a && wb == b = 1 + cntPairs w (a, b)
  | otherwise = cntPairs (wb:w) (a, b)
cntPairs _ _ = 0

threesomes (a:b:c:[]) = [(a, b, c)]
threesomes (a:b:c:p) = (a, b, c):threesomes (b:c:p)

matchList pred = not . null . filter pred

threeVowels = (>2) . length . filter (`elem` "aeiou")
duplicates = matchList (>1) . map length . group
illegal word = not . or $ map (`isInfixOf` word) ["ab", "cd", "pq", "xy"]
pair word = matchList (>1) . map (cntPairs word) $ pairs word
threesome = matchList (\(a, b, c) -> a == c) . threesomes 

nice predicates = and . (<*>) predicates . pure

solve predicates = show . length . filter (nice predicates) . words

solvers = [solve [threeVowels, duplicates, illegal], solve [pair, threesome]]
