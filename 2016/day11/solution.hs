module Main where
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Data.Function
import Runner

import Debug.Trace

data C = G String|M String deriving (Show,Eq,Ord)

isG (G _) = True
isG _ = False
isM (M _) = True
isM _ = False

extr (G x) = x
extr (M x) = x

eNext 0 = [1]
eNext 1 = [2, 0]
eNext 2 = [3, 1]
eNext 3 = [2]

parse [] = []
parse (x:"generator":xs) = G x:parse xs
parse (x:"compatible":xs) = M x:parse xs
parse (_:xs) = parse xs

--intern x | trace (show x) False = undefined
intern (e, fs) = (-1, e):mgs
  where mgs = sort . map fmt . groupBy (on (==) (extr . snd)) . sortBy (on compare (extr . snd)) .  concat . map mark . zip [0..] $ fs
        mark (f, cs) = map ((,) f) cs
--        fmt x | trace (show x) False = undefined
        fmt [(m, M _), (g, G _)] = (m, g)
        fmt [(g, G _), (m, M _)] = (m, g)
	prnt x = trace ("wtf " ++ show x) x

--step (e, fs) | trace (show e ++ ": " ++ show fs) False = undefined
step prev (e, fs) = filter validate [(n, upd n l) | n <- eNext e, l <- load]
  where f = fs !! e
        load = [[a, b] | a <- f, b <- f, a<b] ++ map return f
        upd n l = del e l $ add n l fs
        add n l t = set n (l ++ fs !! n) t
        del n l t = set n (filter (not . (`elem` l)) f) t
        set n l t = take n t ++ [sort l] ++ drop (n+1) t
        validate st@(e', fs') = not (S.member (intern st) prev) && (null (map extr ms \\ map extr gs) || null gs)
          where f' = fs' !! e'
                (ms, gs) = partition isM f'

eval cnt prev sts | trace (show cnt ++ ": " ++ show (S.size prev) ++ " " ++ show (length sts)) False = undefined
eval cnt prev sts
  | any done sts = cnt
  | otherwise = eval (cnt+1) prev' (sts' >>= step prev')
  where done (_, [[],[],[],_]) = True
        done _ = False
        prev' = S.union prev . S.fromList . map intern $ sts'
	sts' = nubBy (on (==) (intern)) sts

solve = show . eval 0 S.empty . prn . return . (,) 0 . map (parse . splitOneOf " .,-") . lines
  where prn x = trace (show x) x

main = runDay 11 [solve]
