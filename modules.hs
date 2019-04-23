import Data.List
import qualified Data.Map as M

-- import Data.List (nub, sort)
-- import Data.List hiding (nub)
-- Prelude> :m + Data.List Data.Map
-- import qualified Data.Map        // Data.Map.filter
-- import qualified Data.Map as M   // M.filter

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- sumPoli :: [[a]] -> Number
sumPoli = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]

-- Prelude> let (a,b) = splitAt 3 "foobar" in b ++ a

findEdge = getDate . head . dropWhile (\(p,_,_,_) -> p < 1000) $ stock
  where stock   =  [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
        getDate = \(_,y,m,d) -> [d] ++ [m] ++ [y]


countRepeatedItems :: (Ord a, Enum a, Num a) => [(a, Int)]
countRepeatedItems =
  let items = (concat . replicate 3 $ [1..3]) ++ [2..8]
      counter l@(x:xs) = (x, length l)
  in map counter . group . sort $ items

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
      matcher acc xs = if take nlen xs == needle then True else acc
  in foldl matcher False (tails haystack)

partitioned = partition (`notElem` ['A'..'Z']) "fghYUIbnHJKnerJK"

-- :t find
-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) =
  if k == key
  then Just v
  else findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs =
  let matcher = \acc (k, v) -> if key == k then Just v else acc
  in foldl matcher Nothing xs

fromList' :: (Ord k) => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty
