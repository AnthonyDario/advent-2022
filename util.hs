module Util where

split :: String -> Char -> [String]
split s c = init (reverse (splitInner s c []))

splitInner :: String -> Char -> [String] -> [String]
splitInner []    _ acc    = acc
splitInner (h:t) c []     = if h == c
                            then splitInner t c [[]]
                            else splitInner t c [[h]]
splitInner (h:t) c (l:ls) = if h == c 
                            then splitInner t c ([]:(l:ls))
                            else splitInner t c ((l ++ [h]):ls)

suml :: [Int] -> Int
suml l = foldl (+) 0 l
