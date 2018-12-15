import Data.Sequence((|>),index,empty,fromList)
import qualified Data.Sequence as S
import Data.List(unfoldr)

gen (prev, a, b)
  | null prev = Just ([3, 7], (fromList [3, 7], a, b))
  | otherwise = Just (ab, (prev', nelf a, nelf b))
  where nelf x = flip mod (S.length prev') . (+) x . succ $ index prev x
        s = index prev a + index prev b
        (sa, sb) = (div s 10, mod s 10)
        (ab, prev') = if s < 10 then ([s], prev |> s) else ([sa, sb], prev |> sa |> sb) 

six (s, p:ps) = let s' = mod (s * 10 + p) 1000000 in Just (s', (s', ps))

main = do
  let input = 825401
  let scores = concat $ unfoldr gen (empty, 0, 1)
  putStrLn . concat . map show . take 10 . drop input $ scores
  print . (+) (-5) . length . takeWhile (/= input) . unfoldr six $ (0, scores)
