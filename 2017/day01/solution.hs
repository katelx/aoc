module Main where
import Runner
import Data.Char(digitToInt)


captcha skip digits = sum . map digitToInt $ zip digits shifted >>= pairedOnly
  where shifted = drop (skip digits) . cycle $ digits
        pairedOnly (x, y)
          | x == y = [x]
          | otherwise = []

solve skip = show . captcha skip

main = runDay 1 [solve (const 1), solve $ (`div` 2) . length]
