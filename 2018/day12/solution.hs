
fives s@([_, _, _, _, _]) = [s]
fives s = take 5 s:fives (tail s)

next ts = map new . fives . (++ "...") . ("..." ++)
  where new s | elem s ts = '#' | otherwise = '.'

psum (n, ps) = sum [i | (i, p) <- zip [-n..] ps, p == '#']

cyc prev ((n, p):np:nps) | elem p' prev = (+ s) . (* l) . abs . (+ s) $ - psum np | otherwise = cyc (p':prev) (np:nps)
  where p' = strip . strip $ p
        s = psum (n, p)
        l = 50000000000 - n
        strip = reverse . dropWhile (== '.')

main = do
  input <- lines <$> readFile "input"
  let state = drop 15 . head $ input
  let ts = map (take 5) . filter ((== '#') . last) . drop 2 $ input
  let gen = zip [0..] . iterate (next ts) $ state
  print $ head . drop 20 . map psum $ gen
  print $ cyc [] $ gen
