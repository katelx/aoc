module Main where
import Runner
import Text.JSON

parse _ (JSRational _ num) = truncate num
parse f (JSArray vals) = sum $ map (parse f) vals
parse f (JSObject vals) = sum . map (parse f . snd) . f . fromJSObject $ vals
parse _ _ = 0

res (Ok v) = v

isNotRed (JSString str) = (/= "red") . fromJSString $ str
isNotRed _ = True
emptyIfRed xs = if all (isNotRed . snd) $ xs then xs else []

solve solver = show . parse solver . res . decode 
  
main = runDay 12 [solve id, solve emptyIfRed]
