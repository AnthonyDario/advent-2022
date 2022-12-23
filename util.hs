module Util where

-- Split a string on the second argument
split :: String -> String -> [String]
split s sp = reverse (splitInner s sp [])

-- Split with an accumulator
splitInner :: String -> String -> [String] -> [String]
splitInner []      _   a@(y:ys) = if y == [] then ys else a
splitInner s@(x:xs) sp []       = case lookAhead s sp [] of
                                       Yes (pos, a') -> splitInner pos sp [[]]
                                       No  (pos, a') -> splitInner pos sp [a']
splitInner s@(x:xs) sp a@(y:ys) = case lookAhead s sp [] of
                                       Yes (pos, a') -> if y == [] 
                                                        then splitInner pos sp a
                                                        else splitInner pos sp ([]:a)
                                       No  (pos, a') -> splitInner pos sp ((y ++ a'):ys)

-- Whether the lookAhead matched or not
data Match = Yes (String , String) | No (String , String)

-- Look ahead to see if the string contains the prefix.
-- Returns suffix of the input string that wasn't looked at
-- and the accumulated prefix.
--
--           input     prefix    acc       match (pos   , acc   )
lookAhead :: String -> String -> String -> Match
lookAhead i        []   acc = Yes (i, acc)
lookAhead []       _    acc = No  ([], acc)
lookAhead (h:t) (c:cs)  acc = if h == c
                              then lookAhead t cs (h:acc)
                              else No (t, acc ++ [h])

-- Sum up a list of integers
suml :: [Int] -> Int
suml l = foldl (+) 0 l
