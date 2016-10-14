module Main where
import Runner
import Data.Array.IO
import Data.Array.Base
import Control.Monad

add :: Int -> Int -> Int -> Int -> Int -> IOUArray Int Int -> IO (IOUArray Int Int)
add step idx lim cnt clim arr | step >= lim = return arr
add step idx lim cnt clim arr | cnt == clim || idx >= lim = add (step+1) (step+1) lim 0 clim arr
add step idx lim cnt clim arr = unsafeRead arr idx >>= unsafeWrite arr idx . (+step) >> add step (idx+step) lim (cnt+1) clim arr

min_index idx mul lim arr = unsafeRead arr idx >>= \v -> if v*mul >= lim then return idx else min_index (idx+1) mul lim arr

solve mul clim num = newArray (1, mx) 1  >>= add 2 2 mx 0 clim >>= min_index 1 mul lim >>= return . show
  where mx = 1000000
        lim = read num
        

main = runDayM 20 [solve 10 99999999, solve 11 50]
