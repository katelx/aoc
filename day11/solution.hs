module Main where
import Runner
import Control.Applicative
import Data.List(nub, group)

threesome (a:b:c:[]) = [(a, b, c)]
threesome (a:b:c:s) = (a, b, c):threesome (b:c:s)

increasing = any valid . threesome
  where valid (a, b, c) = b == succ a && c == succ b

legal = not . any (`elem` "iol")

pairs = (>1) . length . nub . filter ((>1) . length) . group

succs pass = if l == 'z' then succs i ++ "a" else i ++ [succ l]
  where l = last pass
        i = init pass
                                                    

next pass | and $ [increasing, legal, pairs] <*> pure pass = pass
next pass = next $ succs pass

solve solver = solver

main = runDay 11 [solve next, solve (next . succs . next)]
