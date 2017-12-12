module Main where
import Runner

import qualified Data.Map as M
import Data.List(nub,sort,groupBy)
import Data.Function(on)

parse [] = []
parse [p] = [p]
parse (p:"<->":ps) = p:parse ps
parse (p:ps) = init p:parse ps

invert (p:ps) = map (flip (:) . return $ p) ps 

all_paths ps = map to_tuple . groupBy (on (==) head) . sort . (++ ps) $ ps >>= invert
  where to_tuple g = (head . head $ g, nub . concat $ g)

reachable m n | null n' = sort n | otherwise = reachable m' (nub (n++n'))
  where n' = concat . map (flip (M.findWithDefault []) m) $ n
        m' = foldl (flip M.delete) m n 

solve solver input = show . length . solver . map (reachable paths . return . head) $ ps
  where ps = map (parse . words) . lines $ input
        paths = M.fromList . all_paths $ ps

main = runDay 12 [solve head, solve nub]
