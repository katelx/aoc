import Data.List(group, sort)
import Data.Tuple(uncurry)

chksum xs = cnt 2 * cnt 3 where cnt n = length . filter (any ((== n) . length) . group . sort) $ xs

diff xs = map fst . filter (uncurry (==)) . uncurry zip . head . filter ((== 1) . length . filter id . uncurry (zipWith (/=))) $ (,) <$> xs <*> xs

main = let input = words <$> readFile "input" in do
  chksum <$> input >>= print
  diff <$> input >>= print
