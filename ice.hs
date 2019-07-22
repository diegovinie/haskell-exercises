import System.IO

match money (pa, ia) (pb, ib)
  | ia == ib  = False
  | otherwise = pa + pb == money

matchMoney money items =
  foldl (\ac1 a -> foldl (\ac2 b -> resolve ac2 a b) ac1 indexed) [] indexed
    where indexed = zip items [1..]
          resolve acc a b = if match money a b then [snd b, snd a] else acc
