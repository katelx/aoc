import Data.List(nub,sort,(\\),unfoldr,sortBy,foldl')
import Data.Function(on)
import qualified Data.Map as M
import qualified Data.Set as S

data Creature = Creature { kind :: Char, hp :: Int, ap :: Int } deriving (Eq,Show)

adjacent (x, y) = [(x + i, y + j) | (i, j) <- [(-1,0), (0, -1), (0, 1), (1, 0)]]

enemies a b = kind a /= kind b

range com@(cav, cre) xy = filter (not . flip M.member cre) . filter ((== Just True) . flip M.lookup cav) . adjacent $ xy

enemyRanges com@(cav, cre) c = S.fromList . concatMap (range com . fst) . filter (enemies c . snd) . M.toList $ cre

closest com@(cav, cre) enems prevs curs
  | null curs' = Nothing
  | null enemyFields = closest com enems prevs' curs'
  | otherwise = Just . head $ enemyFields
  where curs' = nub . filter notVisited . concatMap (range com) $ curs
        prevs' = S.union prevs . S.fromList $ curs'
        enemyFields = sort . filter enemyField $ curs'
        enemyField = flip S.member enems
        notVisited = not . flip S.member prevs

nextPosition com@(cav, cre) paths prevs txy
  | null tpaths = nextPosition com paths' prevs' txy
  | otherwise = last . init . head $ tpaths
  where paths' = nub . concatMap dir $ paths
        dir ps@(p:_) = let last2 = reverse . take 2 . reverse $ ps in map (: last2) . filter notVisited . range com $ p
        prevs' = S.union prevs . S.fromList . map head $ paths'
        tpaths = sortBy (on compare (last . init)) . filter ((== txy) . head) $ paths'
        notVisited = not . flip S.member prevs

move com@(cav, cre) xyc@(xy, c) = case tgt of
  Nothing -> (com, xyc)
  Just txy -> let nxy = nextPosition com [[xy]] S.empty txy in ((cav, M.insert nxy c . M.delete xy $ cre), (nxy, c))
  where tgt = if inFight then Nothing else closest com (enemyRanges com c) (S.singleton xy) [xy]
        inFight = (not . null) . filter ((==) (Just True) . fmap (enemies c) . flip M.lookup cre) . adjacent $ xy

attack com@(cav, cre) (xy, c)
  | null tgts = com
  | hp t <= ap c = (cav, M.delete txy cre)
  | otherwise = (cav, M.adjust (\q -> q { hp = hp q - ap c }) txy cre)
  where tgts = filter (enemies c . (M.!) cre) . filter (flip M.member cre) . adjacent $ xy
        txy = head . map snd . sort . map dmg $ tgts
        t = (M.!) cre txy
        dmg a = (hp . (M.!) cre $ a, a)

turn com@(cav, cre) xyc@(xy, c)
  | M.member xy cre = attack mcom mxyc
  | otherwise = com
  where (mcom, mxyc) = move com xyc

rnd com@(cav, cre) = if com == next then Nothing else Just (next, next)
  where next = foldl' turn com . M.toAscList $ cre

parse elfAp lines = (M.fromList . concatMap parseCavern $ ylines, M.fromList . concatMap parseCreatures $ ylines) 
  where parseCavern (y, w) = [((y, x), c /= '#') | (x, c) <- zip [0..] w]
        parseCreatures (y, w) = [((y, x), parseCreature c) | (x, c) <- zip [0..] w, elem c "EG"]
        parseCreature c = Creature { kind = c, hp = 200, ap = if c == 'E' then elfAp else 3 } 
        ylines = zip [0..] lines

play elfAp = unfoldr rnd . parse elfAp

countElves = length . filter ((== 'E') . kind) . M.elems . snd

alwaysWin input = head . dropWhile checkElves . map (flip play input) $ [4..]
  where elves = countElves . parse 0 $ input
        checkElves = (< elves) . countElves . last

outcome coms = (length coms - 1) * left
  where left = sum . map hp . M.elems. snd . last $ coms

main = let input = lines <$> readFile "input" in do
  outcome . play 3 <$> input >>= print
  outcome . alwaysWin <$> input >>= print
