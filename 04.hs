import Data.Char (digitToInt)
import Util

type Range = (Int, Int)
type Pair  = (Range, Range)

toNum :: String -> Int
toNum s = toNumInner s 0 

toNumInner :: String -> Int -> Int
toNumInner []    acc = acc
toNumInner (h:t) acc = toNumInner t ((acc * 10) + (digitToInt h))

toPairs :: String -> [Pair]
toPairs [] = []
toPairs s = map toPair (init (split '\n' s))

toPair :: String -> Pair
toPair s = case (map toRange (split ',' s)) of
                (a:b:_) -> (a, b)

toRange :: String -> Range
toRange s = case take 2 (split '-' s) of 
                 (l:r:_) -> (toNum l, toNum r)

contains :: Pair -> Bool
contains (l, r) = (fst l <= fst r && snd l >= snd r) ||
                  (fst r <= fst l && snd r >= snd l) 

overlaps :: Pair -> Bool
overlaps (l, r) = (fst l <= fst r && snd l >= fst r) ||
                  (fst r <= fst l && snd r >= fst l)

-- Find pairs that fully contain the other

main = do
    input <- readFile "inputs/04.txt"

    {- Part 1
    print (foldl (\x y -> if contains y then x + 1 else x) 
                 0
                 (toPairs input))
    -}
    {- Part 2 -}
    print (foldl (\x y -> if overlaps y then x + 1 else x) 
                 0
                 (toPairs input))
