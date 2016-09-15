import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04

import Control.Monad
import Control.Applicative

days = [
  Day01.solvers,
  Day02.solvers,
  Day03.solvers,
  Day04.solvers
  ]


run (solution, part) = putStr "part" >> putStr (show part) >> putStr ": " >> putStrLn solution

runDay n = do
  putStrLn $ "solving day " ++ day ++ ":"
  input <- readFile path
  mapM_ run $ zip (solvers <*> [input]) [1..]
  putStrLn ""
  where day = show n
        path = "day0" ++ day ++ "/input"
        solvers = days !! (n-1)

main = do
  putStrLn ""
  n <- fmap read getLine
  if n == 0 then
    mapM_ runDay [1..length days]
  else
    runDay n
