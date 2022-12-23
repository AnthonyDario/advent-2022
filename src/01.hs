module One where

left :: (a, b) -> a
left (x, y) = x

right :: (a, b) -> b
right (x, y) = y

mapTup :: (a, b) -> (a -> c) -> (b -> d) -> (c, d)
mapTup (x, y) f g = (f x, g y)

split :: String -> Char -> [String]
split [] c = []
split s c = (\(x, y) -> x:(split y c)) (findLine s c)

-- findLine [x:xs] c = if c == x then (x, xs) else x:(findLine xs)
findLine :: [Char] -> Char -> (String, String)
findLine [] c   = ([], [])
findLine (x:xs) c
    | x == c    = ([], xs)
    | otherwise = mapTup (findLine xs c) (\w -> x:w) (\x -> x)

count :: [String] -> [Int]
count l = foldr (\x acc -> case acc of
                            []         | x == ""   -> [] 
                                       | otherwise -> [read x :: Int]
                            (cur:rest) | x == ""   -> 0:(cur:rest)
                                       | otherwise -> (cur + (read x :: Int)) : rest)
           []
           l

max :: Int -> Int -> Int
max x y = if x > y then x else y

maxThree :: Int -> (Int, Int, Int) -> (Int, Int, Int)
maxThree i curr@(a, b, c) = if i < min then curr else (i, x, y)
    where
    (min, x, y) = if a <= b && a <= c then (a, b, c) else 
                  if b <= a && b <= c then (b, a, c) else (c, b, a)

part1 :: String -> Int
part1 s = foldr One.max 0 (count (split s '\n'))

part2 :: String -> Int
part2 s = (\(a, b, c) -> a + b + c) (foldr maxThree (0, 0, 0) (count (One.split s '\n')))
