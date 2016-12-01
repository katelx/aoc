module Main where
import Runner
import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B

getMD5 key = hash . B.append key . B.pack . show

findMD5 prefix key n = if prefix > getMD5 key n
  then n
  else findMD5 prefix key (n+1)

solve prefix key = show $ findMD5 (B.pack prefix) (B.pack key) 0

main = runDay 4 [solve "\0\0\16", solve "\0\0\1"]
