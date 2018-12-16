import Data.List(nub,delete,unfoldr,(\\),sort)
import Data.Char(ord)

cost = (+) 61 . (+) (- ord 'A') . ord

eval ([], _, _, []) = Nothing
eval ([], _, _, [l]) = Just ((cost l, l), ([], [], 0, []))
eval (es, ps, w, ls) = Just (dn, (es', ps', w, ls'))
  where req = take (w - length ps) $ sort ((ls \\ map last es) \\ map snd ps)
        tks = sort $ ps ++ zip (map cost req) req
        dn@(tm, tk) = head tks
        es' = filter ((/= tk) . head) es
        ps' = map (\(s, v) -> (s - tm, v)) $ tail tks
        ls' = delete tk ls

solve w es = unfoldr eval (es, [], w, nub . concat $ es)

main = let input = map (concat . filter ((== 1) . length) . words) . lines <$> readFile "input" in do
  map snd . solve 1 <$> input >>= putStrLn
  sum . map fst . solve 5 <$> input >>= print
