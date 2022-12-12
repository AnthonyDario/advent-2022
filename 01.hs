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

count :: [String] -> [Integer]
count l = foldr (\x acc -> case acc of
                            []         | x == ""   -> [] 
                                       | otherwise -> [read x :: Integer]
                            (cur:rest) | x == ""   -> 0:(cur:rest)
                                       | otherwise -> (cur + (read x :: Integer)) : rest)
           []
           l

max :: Integer -> Integer -> Integer
max x y = if x > y then x else y

maxThree :: Integer -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
maxThree i curr@(a, b, c) = if i < min then curr else (i, x, y)
    where
    (min, x, y) = if a <= b && a <= c then (a, b, c) else 
                  if b <= a && b <= c then (b, a, c) else (c, b, a)
main = do
    input <- readFile "input/01.txt"

    -- problem 1
    --print (foldr Main.max 0 (count (split input '\n')))
    
    -- problem 2
    print ((\(a, b, c) -> a + b + c) (foldr maxThree (0, 0, 0) (count (split input '\n'))))
