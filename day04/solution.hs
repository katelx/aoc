module Main where
import Runner
import Crypto.Hash.MD5
import Data.List(isPrefixOf)

import Data.ByteString.Internal
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Unsafe
import Data.Bits
import Data.Word
import Control.Monad

numToBS = B.pack . show

getMD5 key = hash . B.append key . numToBS

findMD5 prefix key n =
  if B.isPrefixOf prefix dig
    then show n
    else findMD5 prefix key (n+1)
  where dig = toHex $ getMD5 key n

solve zeros key = findMD5 (B.pack zeros) (B.pack key) 0

main = runDay 4 [solve "00000", solve "000000"]

toHex bs = unsafeCreate nl (go 0)
  where len = B.length bs
        nl  = 2 * len
        hexDig d = d + if d < 10 then 48 else 87
        go i p = when (i /= len) (do
          let w = unsafeIndex bs i
          poke p (hexDig $ w `shiftR` 4)
          poke (p `plusPtr` 1) (hexDig $ w .&. 0xF)
          go (i+1) (p `plusPtr` 2))
