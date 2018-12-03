import Data.List(groupBy,group,sort,(\\))
import Data.Char(isDigit)
import Data.Function(on)

claims = length . filter (> 1) . map length . group . sort . (>>= claim) where claim [_, x, y, h, v] = (,) <$> [x .. x + h - 1] <*> [y .. y + v - 1]

divg ds = head $ map head ds \\ concat [[i, j] | [i, x1, y1, h1, v1] <- ds, [j, x2, y2, h2, v2] <- ds, i /= j, x1 <= x2 + h2 && x2 <= x1 + h1, y1 <= y2 + v2 && y2 <= y1 + v1]

main = let input = map (map read . filter (isDigit . head) . groupBy (on (==) isDigit)) . lines <$> readFile "input" in do
  claims <$> input >>= print
  divg <$> input >>= print
