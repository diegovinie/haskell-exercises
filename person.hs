data Person = Person  { firstname :: String
                      , lastname :: String
                      , age :: Int
                      , height :: Float
                      , phoneNumber :: String
                      , flavor :: String
                      } deriving (Show)

guy = Person {
  firstname = "Don",
  lastname = "Ramon",
  age = 20,
  height = 120.3,
  phoneNumber = "44433-44",
  flavor = "sugar"
}

data Car = Car {
                  company :: String
                , model :: String
                , year :: Int
} deriving (Show)

-- data Car a b c = Car {  company :: a
--                       , model :: b
--                       , year :: c } deriving (Show)

-- tellCar :: Car -> String
-- tellCar (Car {company = c, model = m, year = y }) = "This " ++ c ++ " " ++ m ++ " was made in " ++ Show y

data Vector a = Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vPlus` (Vector x y z) = Vector (i+x) (j+y) (k+z)
