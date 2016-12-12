module Main where
import Runner
import Data.List

data Op = Swap Int Int|Rep Char Char|RotL Int|RotR Int|Rot Char|Rev Int Int|Mov Int Int deriving(Show)

parse ["swap","position",x,_,_,y] = Swap (read x) (read y)
parse ["swap","letter",x,_,_,y] = Rep (head x) (head y)
parse ["rotate","left",x,_] = RotL (read x)
parse ["rotate","right",x,_] = RotR (read x)
parse ["rotate",_,_,_,_,_,x] = Rot (head x)
parse ["reverse",_,x,_,y] = Rev (read x) (read y)
parse ["move",_,x,_,_,y] = Mov (read x) (read y)

eval pass (Swap x y) = map swap . zip [0..] $ pass
  where px = pass !! x
        py = pass !! y
        swap (p, c) | p == x = py | p == y = px | otherwise = c

eval pass (Rep x y) = map swap pass
  where swap c | c == x = y | c == y = x | otherwise = c

eval pass (RotL x) = drop x' pass ++ take x' pass where x' = mod x (length pass)

eval pass (RotR x) = eval pass (RotL x') where x' = length pass - x

eval pass (Rot x) = eval pass (RotR i)
  where ix = head . elemIndices x $ pass
        i = 1 + ix + if ix >= 4 then 1 else 0

eval pass (Rev x y) = pre ++ rev ++ suf
  where pre = take x pass
        rev = reverse . take (y-x+1) $ drop x pass
        suf = drop (y+1) pass

eval pass (Mov x y) = (add . rem) pass
  where rem p = take x p ++ drop (x+1) p
        add p = take y p ++ [pass !! x] ++ drop y p

uneval (RotL x) = flip eval $ RotR x
uneval (RotR x) = flip eval $ RotL x
uneval (Rot x) = head . ps
  where ps pass = filter ((==pass) . (flip eval $ (Rot x))) . map (\i -> eval pass (RotL i)) $ [0..length pass-1]
uneval (Mov x y) = flip eval $ Mov y x
uneval op = flip eval $ op

solve solver = solver . map (parse . words) . lines

main = runDay 21 [solve (foldl eval "abcdefgh"), solve (foldr uneval "fbgdceah")]