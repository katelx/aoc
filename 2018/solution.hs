main = let input = 0 <$> readFile "input" in do
  1 <$> input >>= print
  --2 <$> input >>= print
