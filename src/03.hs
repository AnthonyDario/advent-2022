module Three where

import Data.Char (ord, isUpper)
import Util

type Rucksack = (String, String)

bisect :: String -> Rucksack
bisect s = splitAt (((length s) + 1) `div` 2) s

findSame :: Rucksack -> Char
findSame ((l:ls), r) = if l `elem` r then l else findSame (ls, r)

getPrio :: Char -> Int
getPrio c = if isUpper c then (ord c) - 38 else (ord c) - 96

intoThrees :: [a] -> [[a]]
intoThrees l = case splitAt 3 l of
               (li, []) -> [li]
               (li, r) -> [li] ++ (intoThrees r)

findCommonN :: [[Char]] -> [Char]
findCommonN (l:ls) = foldl findCommon l ls

findCommon :: [Char] -> [Char] -> [Char]
findCommon [] _ = []
findCommon _ [] = []
findCommon (x:xs) ys = if   x `elem` ys
                       then (x:(findCommon xs ys))
                       else findCommon xs ys

part1 :: String -> Int
part1 s = suml (map getPrio
                  (map findSame 
                       (map bisect 
                            (split s "\n"))))

part2 :: String -> Int
part2 s = suml (map getPrio
                    (map head
                         (map findCommonN 
                              (intoThrees (split s "\n")))))
