module Main where
import Data.List
import Data.Char
import Runner

import Debug.Trace

data Val = R Int|V Int deriving (Show)

data Op = Cpy Val Val|Inc Val|Dec Val|Jnz Val Val|Tgl Val deriving(Show)

parsev a | any isDigit a = V (read a)
parsev a = R . toReg . head $ a where toReg c = ord c - ord 'a'

parse (op:ps) = case op of
  "cpy" -> Cpy x y
  "inc" -> Inc x
  "dec" -> Dec x
  "jnz" -> Jnz x y
  "tgl" -> Tgl x
  where x = parsev . head $ ps
        y = parsev . last $ ps
-- ++ show (drop i prog)
run regs i prog | trace (show regs ++ " " ++ show i) False = undefined
run regs i prog = case drop i prog of
                    ((Cpy x _):(Inc a):(Dec _):(Jnz _ (V (-2))):(Dec y):(Jnz _ (V (-5))):_) -> run' 5 $ zr 2 $ zr 3 $ wr a a (+ (rd x * rd y))
                    _ -> op1
  where op1 = case op of
                Cpy x y -> run' 1 $ wr y x id
                Inc x -> run' 1 $ wr x x (+1)
                Dec x -> run' 1 $ wr x x (subtract 1)
                Jnz x y -> run' (if rd x == 0 then 1 else rd y) regs
                Tgl x -> tgl regs (i+1) prog (i + rd x)
        op = prog !! i
        rd (V v) = v
        rd (R r) = regs !! r
        wr (R r) s f = take r regs ++ [f . rd $ s] ++ drop (r+1) regs
	zr r rs = take r rs ++ [0] ++ drop (r+1) rs
        run' j nregs
          | 0 <= n && n < length prog = run nregs n prog
          | otherwise  = head nregs
          where n = i + j

tgl regs i prog ti
  | ti < 0 || ti >= length prog = run regs i prog
  | otherwise = run regs i nprog
  where op = prog !! ti
        nop = case op of
                Cpy x y -> Jnz x y
                Inc x -> Dec x
                Dec x -> Inc x
                Jnz x y -> Cpy x y
                Tgl x -> Inc x
        nprog = take ti prog ++ [nop] ++ drop (ti+1) prog

solve solver = show . run [solver, 0, 0, 0] 0 . map (parse . words) . lines

main = runDay 23 [solve 7, solve 12]
