import Data.Char(toUpper)

destroy c (p:ps)
  | c /= p && toUpper c == toUpper p = ps
  | otherwise = c:p:ps
destroy c [] = [c]

react = length . foldr destroy []

clean l = filter (/= toUpper l) . filter (/= l)

main = let input = readFile "input" in do
  react <$> input >>= print
  (\i -> minimum $ map (react . flip clean i) ['a'..'z']) <$> input >>= print
