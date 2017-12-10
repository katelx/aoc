module Main where
import Runner

import qualified Data.Map as M

eval regs [rd, i, v, _, rc, o, c]
  | (op o) (reg rc) (read c) = M.insert rd ((ins i) (reg rd) (read v)) regs
  | otherwise = regs
  where reg r = M.findWithDefault 0 r regs
        op "==" = (==)
        op "!=" = (/=)
        op ">" = (>)
        op "<" = (<)
        op ">=" = (>=)
        op "<=" = (<=)
        ins "inc" = (+)
        ins "dec" = (-)

solve solver = show . maximum . solver . tail . scanl eval M.empty . map words . lines

main = runDay 8 [solve last, solve (map maximum)]
