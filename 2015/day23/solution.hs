module Main where
import Runner

data Reg = A|B deriving (Show)

readReg "a" = A
readReg "b" = B

data Instr = Half Reg|Triple Reg|Inc Reg|Jump Int|JumpEven Reg Int|JumpOne Reg Int deriving (Show)

parse ["hlf", r] = Half $ readReg r
parse ["tpl", r] = Triple $ readReg r
parse ["inc", r] = Inc $ readReg r
parse ["jmp", p] = Jump $ read p
parse ["jie", r, p] = JumpEven (readReg r) (read p)
parse ["jio", r, p] = JumpOne (readReg r) (read p)

run pos (_, b) is | pos >= length is = b
run pos (a, b) is = case (is !! pos) of
  Half A -> run spos (div a 2, b) is
  Half B -> run spos (a, div b 2) is
  Triple A -> run spos (a * 3, b) is
  Triple B -> run spos (a, b * 3) is
  Inc A -> run spos (succ a, b) is
  Inc B -> run spos (a, succ b) is
  Jump p -> run (pos+p) (a, b) is
  JumpEven A p -> run (cpos (even a) p) (a, b) is
  JumpEven B p -> run (cpos (even b) p) (a, b) is
  JumpOne A p -> run (cpos (a == 1) p) (a, b) is
  JumpOne B p -> run (cpos (b == 1) p) (a, b) is
  where spos = succ pos
        cpos pred p = if pred then pos+p else spos
  
solve a = show . run 0 (a, 0) . map parse . map words . lines . filter (not . (`elem` ",+"))

main = runDay 23 [solve 0, solve 1]

