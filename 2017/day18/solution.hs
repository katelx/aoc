module Main where
import Runner

import qualified Data.Map as M

data V = V Int|R Char deriving (Show)
data Op = Snd V|Set V V|Add V V|Mul V V|Mod V V|Rcv V|Jgz V V deriving (Show)


parsev [x] | x >= 'a' && x <= 'z' = R x
parsev x = V (read x)

parseop [op, x] = (ex op) (parsev x)
  where ex "snd" = Snd
        ex "rcv" = Rcv
parseop [op, x, y] = (ex op) (parsev x) (parsev y)
  where ex "set" = Set
        ex "add" = Add
        ex "mul" = Mul
        ex "mod" = Mod
        ex "jgz" = Jgz

data P = P { pos :: Int, regs :: M.Map Char Int, ins :: M.Map Int Op, iq :: [Int], oq :: [Int], st :: Bool, sent :: Int }

initp i t p = if t then r else r { regs = M.insert 'p' i (regs r) }
  where r = P { pos = 0, regs = M.fromList . zip ['a'..'z'] . repeat $ 0, ins = p, iq = [], oq = [], st = t, sent = 0 }

run p = case (M.!) (ins p) (pos p) of
  Snd x -> run $ succp { oq = (get x:oq p), sent = succ . sent $ p }
  Rcv x -> case st p of
    True -> if get x == 0 then run succp else p
    _ -> if null (iq p) then p else run succp { iq = tail (iq p), regs = set x (head . iq $ p) }
  Set x y -> upd const x y
  Add x y -> upd (+) x y
  Mul x y -> upd (*) x y
  Mod x y -> upd (flip mod) x y
  Jgz x y -> let np = get x in run p { pos = (pos p + if np > 0 then get y else 1) }
  where get (V v) = v
        get (R r) = (M.!) (regs p) r
        set (R r) v = M.insert r v (regs p)
        upd fn (R r) y = run succp { regs = (M.adjust (fn (get y)) r (regs p)) }
        succp = p { pos = succ . pos $ p }

first = head . oq . run . initp 0 True

second p = comm (run $ initp 0 False p) (run $ initp 1 False p) 
  where comm (P { oq = [] }) p1@(P { oq = [] }) = sent p1
        comm p1 p2 = comm (runp p1 p2) (runp p2 p1)
        runp p1 p2 = run p1 { iq = iq p1 ++ reverse (oq p2), oq = [] } 
        

solve solver inp = show . solver $ p
  where p = M.fromList . zip [0..] . map (parseop . words) . lines $ inp

main = runDay 18 [solve first, solve second]
