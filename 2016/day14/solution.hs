module Main where
import Runner
import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import Data.Hex
import Data.List
import Data.Char

hashes 0 h = h
hashes i h = hashes (i-1) $ B.map toLower . hex . hash $ h

getMD5 i key = B.unpack . hashes i . B.append key . B.pack . show

keys i key = map (group . getMD5 i key) [0..]

index cnt (x:xs)
  | null triples = next
  | null fives = next
  | otherwise = cnt
  where triples = filter ((>=3) . length) x
        triple = head . head $ triples
        five = replicate 5 triple
        fives = filter (any (isInfixOf five)) . take 1000 $ xs
        next = index (cnt+1) xs

indices cnt xs = idx:indices (idx+1) xs'
  where idx = index cnt xs
        xs' = drop (idx-cnt+1) xs 

solve solver = show . head . drop 63 . indices 0 . keys solver . B.pack

main = runDay 14 [solve 1, solve 2017]
