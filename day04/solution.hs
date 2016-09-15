module Day04(solvers) where
       
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(isPrefixOf)

numToBS = B.pack . show

getMD5 key = show . md5 . B.append key . numToBS

findMD5 prefix key n =
  if isPrefixOf prefix hash
    then show n
    else findMD5 prefix key (n+1)
  where hash = getMD5 key n

solve zeros key = findMD5 zeros (B.pack key) 0

solvers = [solve "00000", solve "000000"]
