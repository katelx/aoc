import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits

data RV = R|V

execute prev rs prog@(instr, ipr)
  | ip == 28 && S.member r prev = []
  | ip == 28 = r : next (S.insert r prev)
  | otherwise = next prev
  where ip = (M.!) rs ipr
        r = (M.!) rs 2
        next prev' = execute prev' (M.adjust succ ipr (instr !! ip $ rs)) prog

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

start = M.fromList . zip [0..5] . (: repeat 0) $ 0

main = let input = execute S.empty start . parse . lines <$> readFile "input" in do
  head <$> input >>= print
  last <$> input >>= print
