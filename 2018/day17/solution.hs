import qualified Data.Map as M
import Data.List(sort,groupBy)
import Data.Char(isDigit)
import Data.Function(on)

fill ymax g def xy@(x, y)
  | y > ymax = tryFinish g def
  | otherwise = case M.lookup xyNext g of
      Nothing -> fill ymax withFlow def xyNext
      Just '|' -> tryFinish withFlow def
      _ -> if xMinClosed && xMaxClosed then fill ymax (withRow '~') def xyPrev else tryFinish (withRow '|') deferred
  where xyNext = (x, succ y)
        xyPrev = (x, pred y)
        tryFinish g' def' = if null def' then g' else fill ymax g' (tail def') (head def')
        withFlow = (M.insert xy '|' g)
        withRow r = foldl (\g' xy' -> M.insert xy' r g') g $ map (flip (,) y) [xMin..xMax]
        (xMin, xMinClosed) = findX pred x
        (xMax, xMaxClosed) = findX succ x
        findX f x' = let x'' = f x' in case M.lookup (x'', y) g of
          Just '#' -> (x', True)
          _ -> case M.lookup (f x'', succ y) g of
            Just '#' -> findX f x''
            Just '~' -> findX f x''
            _ -> (x'', False)
        deferred = (if not xMaxClosed then [(succ xMax, y)] else []) ++ (if not xMinClosed then [(pred xMin, y)] else []) ++ def

runAndCnt es g = length . filter (flip elem es) . M.elems $ fill yn g [] (500, y1)
  where (y1, yn) = let ys = map snd . M.keys $ g in (minimum ys, maximum ys)

parse = M.fromList . flip zip (repeat '#') . concatMap parse' . lines
  where parse' ln = let [xs, ys] = map (toRange . nums) . sort . words $ ln in (,) <$> xs <*> ys
        nums = map read . filter (isDigit . head) . groupBy (on (==) isDigit)
        toRange [a] = [a]
        toRange [a, b] = [a..b]

main = let input = parse <$> readFile "input" in do
   runAndCnt ['|', '~'] <$> input >>= print
   runAndCnt ['~'] <$> input >>= print
