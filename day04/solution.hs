import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(isPrefixOf)

numToBS = B.pack . show

getMD5 key = show . md5 . B.append key . numToBS

findMD5 prefix key n =
  if isPrefixOf prefix hash
    then print n
    else findMD5 prefix key (n+1)
  where hash = getMD5 key n

main = do
  key <- B.readFile "input.dat"
  putStr "part1: " >> findMD5 "00000" key 0
  putStr "part2: " >> findMD5 "000000" key 0
