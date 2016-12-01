module Main where
import Runner
import Data.List(nub, isPrefixOf)
import Data.Char(isUpper,isLower)
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Data.Function

import Debug.Trace

parseSymbol [] = []
parseSymbol [x] = [[x]]
parseSymbol (x:y:r)
  | isUpper x && isLower y = [x, y]:parseSymbol r
  | otherwise = [x]:parseSymbol (y:r)

parse ([part, "=>", repl]:xs) = (mol, (part, parseSymbol repl):rest)
  where (mol, rest) = parse xs
parse ([]:xs) = parse xs
parse [[mol]] = (parseSymbol mol, [])

replace _ [] _ = []
replace d (l:ls) v@(p, r)
         | p == l = (d ++ r ++ ls):replace (d++[p]) ls v
         | otherwise = replace (d++[l]) ls v 
                 
replacea (mol, r) = show . length . nub . concat $ map (replace [] mol) r

step d [] _ = d
step d fl@(l:ls) v@(r, p)
  | isPrefixOf p fl = d++[r]++drop (length p) fl
  | otherwise = step (d++[l]) ls v

build r mol = map (step [] mol) r

filtermx col = filter (\p -> length p == len) col
  where len = minimum (map length col)

builda n mols (mol, r) = if res then show n else builda (n+1) next (mol, r)
  where next = take 15 . nub . filtermx $ (mols >>= build r)
        res = (==1) . length . head $ next

buildaa (mol, r) = builda 1 [mol] (mol, r)

solve solver = solver . parse . map words . lines

main = runDay 19 [solve replacea, solve buildaa]
