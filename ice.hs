import System.IO
import Data.List

----------------------------------------
-- solution
matchMoney money (pa, ia) (pb, ib)
  | ia == ib  = False
  | otherwise = pa + pb == money

foldUntil c i [] = []
foldUntil comparator item (x:xs)
  | comparator item x = [snd item, snd x]
  | otherwise         = foldUntil comparator item xs

refold money (i:is)
  | exec == []        = refold money is
  | length exec > 0   = exec
  | otherwise         = []
  where exec = foldUntil (matchMoney money) i is

whatFlavors (money, items) = refold money $ zip items [1..]
--------------------------------------

getCase = do
  money <- getLine
  itemsLength <- getLine
  itemsString <- getLine
  let items = map (\n -> read n :: Integer) $ words itemsString
  -- [money, items]
  return (read money :: Integer, items)

printResult items = putStrLn $ concat $ intersperse " " [show i | i <- items]

main = do
  firstLine <- getLine
  let sets = read firstLine :: Integer
  dataTuple <- mapM (\s -> getCase) [1..sets]

  mapM_ printResult $ map whatFlavors dataTuple

  return ()
