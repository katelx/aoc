module Main where
import Runner

data State = Group Int|Cancel State|Garbage State deriving Show

score (Cancel s, t) _ = (s, t)
score (s, t) '!' = (Cancel s, t)
score (Garbage s, t) '>' = (s, t)
score (Garbage s, (p, g)) _ = (Garbage s, (p, g+1))
score (s, t) '<' = (Garbage s, t)
score (Group n, t) '{' = (Group (n+1), t)
score (Group n, (p, g)) '}' = (Group (n-1), (p+n, g))
score s _ = s

solve ext = show . ext . snd . foldl score (Group 0, (0, 0))

main = runDay 9 [solve fst, solve snd]
