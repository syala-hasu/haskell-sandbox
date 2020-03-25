main=mapM f[1..100]
f n|d<-drop.(*4).mod n=putStrLn$max(show n)$d 3"Fizz"++d 5"Buzz"
