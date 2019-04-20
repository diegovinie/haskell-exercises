lucky :: (Integral a) => a -> String
lucky 7 = "el siete"
lucky x = "No es siete."

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "Es una lista vacía"
tell (x:[]) = "Un solo elemento: " ++ show x
tell (x:y:[]) = "Dos elementos: " ++ show x ++ " y " ++ show y
tell all@(x:y:_) = "tiene más de dos elementos. " ++ show all

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.4 = "adada"
  | otherwise = "lalala"

max' :: (Ord a) => a -> a -> a
max' x y
  | x > y     = x
  | otherwise = y

max'' :: (Ord a) => a -> a -> a
max'' x y | x > y = x | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
x `myCompare` y
  | x > y     = GT
  | x == y    = EQ
  | otherwise = LT

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= skinny = "Emo."
  | bmi <= normal = "Normal!"
  | bmi <= fat = "gordo"
  | otherwise = "Ballena!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [let bmi wheight height = wheight / height ^2 in bmi w h | (w, h) <- xs]

calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^2, bmi >= 25.0]

describeList :: [a] -> String
describeList xs = "La lista es: " ++ what xs
  where what [] = "vacía"
        what (_:[]) = "singleton"
        what xs = "larga"

describeList' :: [a] -> String
describeList' xs = "la lista :" ++ case xs of [] -> "vacia"
                                              [x] -> "de una "
                                              xs -> "muchas"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "esta vacia"
maximum' [x] = x
maximum' (x:xs) = x `max` maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "vacia"
maximum'' [x] = x
maximum'' (x:xs)
  | x > maxTail = x
  |otherwise    = maxTail
  where maxTail = maximum'' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' i value
  | i <= 0    = []
  | otherwise = value:replicate' (i - 1) value

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' 0 val = []
replicate'' i val = val:replicate'' (i - 1) val

take' :: (Num a, Ord a, Eq b) => a -> [b] -> [b]
take' n xs | n <= 0  = [] | xs == [] = xs
take' i (x:xs)  = x : take' (i - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' el (x:xs)
  | el == x   = True
  | otherwise = elem' el xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) =
  let smallerSorted = quicksort [x | x <- as, x <= a]
      biggerSorted  = quicksort [x | x <- as, x > a]
  in smallerSorted ++ [a] ++ biggerSorted

-- la función fn se aplica a los valores de cada lista
-- los tipos a y b entran en la función y devuelve c
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' fn (x:xs) (y:ys) = fn x y : zipWith' fn xs ys


flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = fn x : map fn xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn (x:xs)
  | fn x      = x : filter fn xs
  | otherwise = filter fn xs

-- máximo divisor div
largestDivisible :: (Integral a) => a -> a
largestDivisible div = head (filter fn [100000, 99999..])
  where fn x = x `mod` div == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn [] = []
takeWhile' fn (x:xs)
  | fn x == False = []
  | otherwise = x : takeWhile' fn xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x  = x : chain (x `div` 2)
  | odd x   = x : chain (x * 3 + 1)

numLongChain :: Int
numLongChain = length (filter (\x -> length x > 15) (map chain [1..100]))

numLongChain' :: Int
numLongChain' = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numLongChain'' :: (Integral a) => Int -> [a] -> Int
numLongChain'' lim xs = length (filter isLong (map chain xs))
  where isLong x = length x > lim

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y = foldl (\acc x -> if x == y then True else acc) False

map'' :: (a -> b) -> [a] -> [b]
map'' fn = foldr (\x acc -> fn x : acc) []
