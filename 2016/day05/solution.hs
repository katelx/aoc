module Main where
import Runner
import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as W
import Text.Printf
import Data.Word

getMD5 key = hash . B.append key . B.pack . show

prefix = B.pack "\0\0\16"

simple dig pass = upd pass
  where char = printf "%x" $ W.index dig 2
        upd [] = []
        upd ('_':xs) = head char:xs
        upd (x:xs) = x:upd xs

better dig pass = if valid then upd else pass
  where pos = fromIntegral $ W.index dig 2
        valid = pos < 8 && pass !! pos == '_'
        char = printf "%x" $ div (W.index dig 3) 16
        upd = take pos pass ++ char ++ drop (pos+1) pass      

calc key matcher n pass = if match then onmatch else calcnext pass
  where dig = getMD5 key n
        match = prefix > dig
        newpass = matcher dig pass
        found = all (/= '_') newpass
        calcnext = calc key matcher (n+1)
        onmatch = if found then newpass else calcnext newpass

solve solver key = calc (B.pack key) solver 0 (replicate 8 '_')

main = runDay 5 [solve simple, solve better]
