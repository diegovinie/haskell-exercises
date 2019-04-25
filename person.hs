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
