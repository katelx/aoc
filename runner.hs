module Runner(runDayM,runDay) where

import Control.Monad
import Control.Applicative
import Text.Printf(printf)

run (solution, part) = solution >>= putStrLn . info 
  where info res = "part" ++ show part ++ ": " ++ res

runDayM :: Int -> [String -> IO String] -> IO ()
runDayM n solvers = putStrLn info >> readFile path >>= \input -> mapM_ run (zip (map (\s -> s input) solvers) [1..]) >> putStrLn ""
  where day = printf "%02d" n
        info = printf "solving day %s:" day
        path = "day" ++ day ++ "/input"

runDay n = runDayM n . map (\s -> return . s)
