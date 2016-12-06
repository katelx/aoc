module Main where
import Runner
import Data.List
import Data.Function(on)

solve solver = map (head . solver (on compare length) . group . sort) . transpose . lines

main = runDay 6 [solve maximumBy, solve minimumBy]
