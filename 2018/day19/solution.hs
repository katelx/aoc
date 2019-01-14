import qualified Data.Map as M
import Data.Bits

data RV = R|V

execute rs prog@(instr, ipr)
  | ip == 2 = sum . divisors . maximum . M.elems $ rs
  | otherwise = execute rs' prog
  where ip = (M.!) rs ipr
        rs' = M.adjust succ ipr (instr !! ip $ rs)
        divisors n = 1 : n : filter ((==0) . rem n) [2 .. div n 2]

ops = M.fromList [
  ("addr", op (+) R R),
  ("addi", op (+) R V),
  ("mulr", op (*) R R),
  ("muli", op (*) R V),
  ("banr", op (.&.) R R),
  ("bani", op (.&.) R V),
  ("borr", op (.|.) R R),
  ("bori", op (.|.) R V),
  ("setr", op set R V),
  ("seti", op set V V),
  ("gtir", op gt V R),
  ("gtri", op gt R V),
  ("gtrr", op gt R R),
  ("eqir", op eq V R),
  ("eqri", op eq R V),
  ("eqrr", op eq R R)
  ]
  where op f ar br = \a b c rs -> let v = f (e rs ar a) (e rs br b) in M.insert c v rs
        e rs r v = case r of
          R -> (M.!) rs v
          _ -> v
        set a _ = a
        gt a b = if a > b then 1 else 0
        eq a b = if a == b then 1 else 0

parse (ipr:prog) = (map (parse' . words) prog, read . drop 4 $ ipr)
  where parse' (op:abc) = let [a, b, c] = map read abc in ((M.!) ops op) a b c

start = M.fromList . zip [0..5] . repeat $ 0 

main = let input = parse . lines <$> readFile "input" in do
  execute start <$> input >>= print
  execute (M.insert 0 1 start) <$> input >>= print
