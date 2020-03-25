manlen :: (Int, Int) -> (Int, Int) -> Int
manlen p1 p2 = abs(fst p1 - fst p2) + abs(snd p1 - snd p2)

main = print$manlen (1,2) (3,4)
