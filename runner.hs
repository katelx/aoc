import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13

import Control.Monad
import Control.Applicative
import Text.Printf(printf)

days = [
  Day01.solvers,
  Day02.solvers,
  Day03.solvers,
  Day04.solvers,
  Day05.solvers,
  Day06.solvers,
  Day07.solvers,
  Day08.solvers,
  Day09.solvers,
  Day10.solvers,
  Day11.solvers,
  Day12.solvers,
  Day13.solvers
  ]


run (solution, part) = putStr "part" >> putStr (show part) >> putStr ": " >> putStrLn solution

runDay n = do
  putStrLn $ "solving day " ++ day ++ ":"
  input <- readFile path
  mapM_ run $ zip (solvers <*> [input]) [1..]
  putStrLn ""
  where day = printf "%02d" n
        path = "day" ++ day ++ "/input"
        solvers = days !! (n-1)

main = do
  putStrLn ""
  n <- fmap read getLine
  if n == 0 then
    mapM_ runDay [1..length days]
  else
    runDay n
