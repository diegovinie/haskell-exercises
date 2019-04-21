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
