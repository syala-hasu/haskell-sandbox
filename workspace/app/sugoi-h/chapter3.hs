tri_pattern :: Int -> Int
tri_pattern 0 = 0
tri_pattern 1 = 0
tri_pattern 2 = 1
tri_pattern n = tri_pattern(n-3) + tri_pattern(n-2) + tri_pattern(n-1)

tri_guard :: Int -> Int
tri_guard n
  | n == 0  = 0
  | n == 1  = 0
  | n == 2  = 1
  | otherwise = tri_guard(n-3) + tri_guard(n-2) + tri_guard(n-1)

tri_case :: Int -> Int
tri_case n = case n of
               0 -> 0
               1 -> 0
               2 -> 1
               _ -> tri_case(n-3) + tri_case(n-2) + tri_case(n-1)


qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
qadd (m,n) (o,p) 
    | n == 0 = error "error"
    | p == 0 = error "error"
    | otherwise = (m*p+o*n, n*p)


qequel :: (Int, Int) -> (Int, Int) -> Bool
qequel (m,n) (o,p)
   | n == 0 = error "error"
   | p == 0 = error "error"
   | otherwise = m*p == n*o 

main :: IO()
main = print$qequel (1,2) (-4,-8)
-- main = print$[tri_case n | n <- [0..10]]
