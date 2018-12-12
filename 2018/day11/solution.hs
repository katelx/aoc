import qualified Data.Map as M
import Data.Function(on)

ser = 7672

powers = M.fromList [((x, y), let r = x + 10 in (+) (-5) . flip mod 10 . flip div 100 . (*) r . (+) ser . (*) r $ y) | x <- ps, y <- ps] where ps = [1..300]

largest s = maximum . map off $ (,) <$> ps <*> ps
  where ps = [1..301-s]
        off (x, y) = (sum [(M.!) powers (x + ix, y + iy) | ix <- ss, iy <- ss], (x, y, s))
        ss = [0..s-1]

solve = init . tail . show . snd . maximum . map largest

main = do
  putStrLn $ solve [3..3]
  putStrLn $ solve [14..14]
