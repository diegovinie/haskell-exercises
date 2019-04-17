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
