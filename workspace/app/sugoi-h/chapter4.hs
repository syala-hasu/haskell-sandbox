tri_number :: Int -> Int
tri_number 1 = 1
tri_number n = n + tri_number(n-1)

tetration :: Integer -> Integer -> Integer
tetration m 1 = m
tetration m n = m * (tetration m (n-1))

index :: Int -> [a] -> a
index n xs = 

main = print$tetration 3 4
