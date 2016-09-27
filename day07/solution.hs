module Day07(solvers) where

import qualified Data.Map as M
import Data.Maybe(fromJust,isJust)
import Data.Char(isDigit)
import Data.List(all)
import Data.Bits
import Data.Word

data Op = And|LShift|Not|Or|RShift deriving (Show)

data Gate = Raw Word16|Named String|Gate Op [Gate] deriving(Show) 

parseWire str | all isDigit str = Raw $ read str
parseWire str = Named str

createGate op = Gate op . map parseWire  

parse [w, "->", name] = (name, parseWire w)
parse [w1, "AND", w2, "->", name] = (name, createGate And [w1, w2]) 
parse [w1, "LSHIFT", w2, "->", name] = (name, createGate LShift [w1, w2])
parse ["NOT", w, "->", name] = (name, createGate Not [w])
parse [w1, "OR", w2, "->", name] = (name, createGate Or [w1, w2]) 
parse [w1, "RSHIFT", w2, "->", name] = (name, createGate RShift [w1, w2])

mget name = fromJust . M.lookup name
int = fromInteger . toInteger

eval (Raw value) m = value
eval (Named name) m = eval (mget name m) m
eval (Gate And [w1, w2]) m = eval w1 m .&. eval w2 m
eval (Gate LShift [w1, w2]) m = eval w1 m `shiftL` (int $ eval w2 m)
eval (Gate Not [w]) m = complement $ eval w m
eval (Gate Or [w1, w2]) m = eval w1 m .|. eval w2 m
eval (Gate RShift [w1, w2]) m = eval w1 m `shiftR` (int $ eval w2 m)

isRaw (Raw _) = True
isRaw _ = False

toRaw (Raw value) _ = Just value
toRaw (Named name) m | isRaw value = toRaw value m where value = mget name m
toRaw g@(Gate op ws) m | all isJust values = Just $ eval g m where values = map (\w -> toRaw w m) ws 
toRaw _ _ = Nothing

evalMap m | M.size m == (M.size $ M.filter isRaw m) = m 
evalMap m = evalMap $ M.map (\g -> case (toRaw g m) of
                       Nothing -> g
                       Just r -> Raw r) m

solve solver = show . eval (Named "a") . evalMap . solver . M.fromList . map parse . map words . lines

solvers = [solve id, \input -> solve (M.insert "b" (Raw . read . solve id $ input)) input]
