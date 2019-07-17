-- integral :: [a] -> [a] -> a -> a -> a
integral :: (Enum t, Fractional t, Fractional a, Integral b) => [a] -> [b] -> t -> t -> a
integral as bs i e = sum $ map fn interval
  where fn = \x -> sum $ map (\d -> d * 0.01) [a^b | (a, b) <- zip as bs]
        interval = [x | x <- [0..(e - i)/0.01]]
