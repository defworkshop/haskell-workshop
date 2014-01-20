

module Solution.HausVomNikolaus where

import Data.List (intercalate)

-- /\
-- 34
-- 12
edges :: [(Int, Int)]
edges = [(1, 2), (1, 3), (1, 4), (3, 2), (3, 4), (3, 4), (2, 4)]


type PartialSol = [(Int, Int)]
type EdgeRest = [(Int, Int)]

data Sol = Sol PartialSol EdgeRest deriving (Show)

emptySol :: [Sol]
emptySol = [Sol [] edges]


extractSol :: Sol -> PartialSol
extractSol (Sol sol []) = sol
extractSol xs = error ("Solution not complete: " ++ show xs)

flipPair :: (a, b) -> (b, a)
flipPair (x, y) = (y, x)

extendSol :: Sol -> [Sol]
extendSol (Sol as bs) = go bs
  where go [] = []
        go (x:xs) = Sol (x:as) xs : Sol (flipPair x:as) xs : map g (go xs)
          where g (Sol c cs) = Sol c (x:cs)


ok :: Sol -> Bool
ok (Sol ((_, x):(y,_):_) _) = x == y
ok (Sol [] _) = True
ok (Sol [(_,1)] _) = True
ok _ = False

expand :: [Sol] -> [Sol]
expand = filter ok . concatMap extendSol

buildSolutions :: [PartialSol]
buildSolutions = half ++ map (map flipPair . reverse) half
  where half = map extractSol $ go emptySol
        go xs@(Sol _ [] : _) = xs
        go sols = go (expand sols)


main :: IO ()
main = do
  putStrLn $ intercalate "\n" (map show buildSolutions)
  putStrLn $ "Anzahl der Loesungen: " ++ show (length buildSolutions)