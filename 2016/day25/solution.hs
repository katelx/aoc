module Main where
import Data.List
import Data.Char
import Runner

data Val = R Int|V Int

data Op = Cpy Val Val|Inc Val|Dec Val|Jnz Val Val|Out Val

parsev a | any isDigit a = V (read a)
parsev a = R . toReg . head $ a where toReg c = ord c - ord 'a'

parse (op:ps) = case op of
  "cpy" -> Cpy x y
  "inc" -> Inc x
  "dec" -> Dec x
  "jnz" -> Jnz x y
  "out" -> Out x
  where x = parsev . head $ ps
        y = parsev . last $ ps

run regs i prog = case op of
  Cpy x y -> run' 1 $ wr y x id
  Inc x -> run' 1 $ wr x x (+1)
  Dec x -> run' 1 $ wr x x (subtract 1)
  Jnz x y -> run' (if rd x == 0 then 1 else rd y) regs
  Out x -> rd x:run regs (i+1) prog
  where op = prog !! i
        rd (V v) = v
        rd (R r) = regs !! r
        wr (R r) s f = take r regs ++ [f . rd $ s] ++ drop (r+1) regs
        run' j nregs
          | 0 <= n && n < length prog = run nregs n prog
          | otherwise  = []
          where n = i + j

runs i prog
  | all valid (zip cyc runi) = i
  | otherwise = runs (i+1) prog
  where runi = run [i, 0, 0, 0] 0 prog
        valid (x, y) = x == y
	cyc = take 10 (cycle [0, 1])

solve solver = show . runs 1 . map (parse . words) . lines

main = runDay 25 [solve id]
