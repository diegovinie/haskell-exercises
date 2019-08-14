import Data.List

x |> f = f x

fac 0 = 1
fac n = n * fac (n - 1)

triangle n = map row [0..(n - 1)]
  where row y = [term y x | x <- [0..y]]
        term r c = quot (fac r) ((fac c) * (fac (r - c)))

printResult t = mapM_ (putStrLn .printLine) t
  where printLine line = intercalate " " (map show line)

main = do
  input <- getLine
  input |> (read :: String -> Int) |> triangle |> printResult
