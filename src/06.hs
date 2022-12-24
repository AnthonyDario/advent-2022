module Six where

import Util

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains x (y:ys) = if x == y then True else contains x ys

uniq :: (Eq a) => [a] -> Bool
uniq []     = True
uniq (x:xs) = if contains x xs then False else uniq xs

findStart :: (Eq a) => Int -> [a] -> Int
findStart i []       = i
findStart i l@(x:xs) = if uniq (take i l) then i else (1 + (findStart i xs))

part1 :: String -> Int
part1 s = findStart 4 s

part2 :: String -> Int
part2 s = findStart 14 s
