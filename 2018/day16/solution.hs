import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.List(groupBy,sort,sortBy,unfoldr,delete)
import Data.Function(on)

data RV = R|V

execute (tests, prog) = flip (M.!) 0 $ foldl (execOp codes) (M.fromList . zip [0..3] . repeat $ 0) prog 
  where codes = map snd . sort . unfoldr reduce . zip [0..] . map (S.toList . S.unions . map snd) . groupBy (on (==) fst) . sortBy (on compare fst) . map matches $ tests
        reduce [] = Nothing
        reduce ls = let op@(code, [idx]) = find ls in Just ((code, idx), map (remove idx) . delete op $ ls)
        find ((code, [idx]):_) = (code, [idx])
        find (_:ls) = find ls
        remove idx (code, idxs) = (code, delete idx idxs)
        execOp opMap rs [op, a, b, c] = (ops !! (opMap !! op)) a b c rs

threeOrMore = length . filter ((>= 3) . S.size . snd) . map matches . fst

matches (rb, [op, a, b, c], ra) = (,) op . S.fromList . map fst . filter (match . snd) . zip [0..] $ ops
  where match op = op a b c rb == ra

ops = [
  op (+) R R,
  op (+) R V,
  op (*) R R,
  op (*) R V,
  op (.&.) R R,
  op (.&.) R V,
  op (.|.) R R,
  op (.|.) R V,
  op set R V,
  op set V V,
  op gt V R,
  op gt R V,
  op gt R R,
  op eq V R,
  op eq R V,
  op eq R R
  ]
  where op f ar br = \a b c rs -> let v = f (e rs ar a) (e rs br b) in M.insert c v rs
        e rs r v = case r of
          R -> (M.!) rs v
          _ -> v
        set a _ = a
        gt a b = if a > b then 1 else 0
        eq a b = if a == b then 1 else 0

parse input = (ptests input, pprog input)
  where ptests ls
          | isTest ls = ptest ls:ptests (drop 3 ls)
          | otherwise = []
        pprog ls
          | null ls = []
          | isTest ls = pprog (drop 3 ls)
          | otherwise = map (map read . words) ls
        ptest (b:t:a:_) = (preg b, map read . words $ t, preg a)
        preg = M.fromList . zip [0..] . read . drop 8
        isTest = (== "Before:") . head . words . head

main = let input = parse . filter (not . null) . lines <$> readFile "input" in do
  threeOrMore <$> input >>= print
  execute <$> input >>= print
