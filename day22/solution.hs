module Main where
import Runner
import Data.List(nub)

data Spell = Missile|Drain|Shield|Poison|Recharge

data Boss = Boss { bhp :: Int, dmg :: Int } deriving (Eq)

data Player = Player { php :: Int, mana :: Int, armor :: Int, used :: Int} deriving (Eq)

data Effects = Effects { shield :: Int, poison :: Int, recharge :: Int} deriving (Eq)

data Turn = Turn Boss Player Effects|Won Int deriving (Eq)

duels level ds
  | all valid ds = minimum . map extr $ ds
  | otherwise = duels level res
  where valid (Won _) = True
        valid _ = False
        extr (Won n) = n
        res = nub $ ds >>=
          pturn_before level >>= game_over >>=
          affect >>= game_over >>=
          pturn >>= game_over >>=
          affect >>= game_over >>=
          bturn >>= game_over
        affect (Won n) = [Won n]
        affect t = return . affect_shield . affect_poison . affect_recharge $ t

game_over (Won n) = [Won n]
game_over t@(Turn b p e)
  | bhp b <= 0 = [Won $ used p]
  | php p <= 0 = []
  | otherwise = [t]

pturn_before _ (Won n) = [Won n]
pturn_before level (Turn b p e) = [Turn b p { php = php p - level } e]

pturn (Won n) = [Won n]
pturn t@(Turn b p e) = filter (castable p e) [Recharge,Poison,Shield,Drain,Missile] >>= return . cast t

bturn (Won n) = [Won n]
bturn (Turn b p e) = [Turn b p { php = php p - max 1 (dmg b - armor p) } e]

affect_shield (Turn b p e)
  | shield e == 0 = Turn b p e
  | shield e == 1 = Turn b np ne
  | otherwise = Turn b p ne
  where ne = e { shield = shield e - 1 }
        np = p { armor = armor p - 7 }

affect_poison (Turn b p e)
  | poison e == 0 = Turn b p e
  | otherwise = Turn nb p ne
  where ne = e { poison = poison e - 1 }
        nb = b { bhp = bhp b - 3 }

affect_recharge (Turn b p e)
  | recharge e == 0 = Turn b p e
  | otherwise = Turn b np ne
  where ne = e { recharge = recharge e - 1 }
        np = p { mana = mana p + 101 }

castable p e s = spell_cost s <= mana p && expired s
  where expired Shield = shield e <= 0
        expired Poison = poison e <= 0
        expired Recharge = recharge e <= 0
        expired _ = True

cast (Turn b p e) spell = case spell of
  Missile -> Turn b { bhp = bhp b - 4 } np e
  Drain -> Turn b {bhp = bhp b - 2} np { php = php p + 2 } e
  Shield -> Turn b np { armor = armor p + 7 } e { shield = 6 }
  Poison -> Turn b np e { poison = 6 }
  Recharge -> Turn b np e { recharge = 5 }
  where np = p { mana = mana p - cost, used = used p + cost }
        cost = spell_cost spell

spell_cost Missile = 53
spell_cost Drain = 73
spell_cost Shield = 113
spell_cost Poison = 173
spell_cost Recharge = 229

solve level inp = show . duels level $ [Turn boss player effects]
  where [hp, d] = map (read . last . words) . lines $ inp
        boss = Boss {bhp = hp, dmg = d}
        player = Player {php = 50, mana = 500, armor = 0, used = 0}
        effects = Effects {shield = 0, poison = 0, recharge = 0}
main = runDay 22 [solve 0, solve 1]
 
