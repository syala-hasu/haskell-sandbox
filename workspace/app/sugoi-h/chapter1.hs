manlen :: (Int, Int) -> (Int, Int) -> Int
manlen p1 p2 = abs(fst p1 - fst p2) + abs(snd p1 - snd p2)

points :: Int -> [(Int, Int)]
points n = [(x,y) | x <- [-n..n], y <- [-n..n] ]

mancircle :: Int -> [(Int, Int)]
mancircle n = [p | p <- points n, manlen (0, 0) p == n]

main = print$mancircle 2
