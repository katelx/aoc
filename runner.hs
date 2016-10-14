module Runner(runDayM,runDay) where

import Control.Monad
import Control.Applicative
import Text.Printf(printf)

run (solution, part) = putStr "part" >> putStr (show part) >> putStr ": " >> solution >>= putStrLn

runDayM :: Int -> [String -> IO String] -> IO ()
runDayM n solvers = do
  putStrLn $ "solving day " ++ day ++ ":"
  input <- readFile path
  mapM_ run $ zip (map (\s -> s input) solvers) [1..]
  putStrLn ""
  where day = printf "%02d" n
        path = "day" ++ day ++ "/input"

runDay n solvers = runDayM n (map (\s -> return . s) solvers)
