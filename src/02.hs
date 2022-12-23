module Two where

data Shape   = Rock | Paper | Scissors deriving Show
data Outcome = Win | Loss | Draw

type Game  = (Shape, Shape)
type Guide = (Shape, Outcome)


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

toShape :: Char -> Shape
toShape c | c `elem` ['A', 'X'] = Rock
          | c `elem` ['B', 'Y'] = Paper
          | c `elem` ['C', 'Z'] = Scissors

toOutcome :: Char -> Outcome
toOutcome c = case c of 
                   'Z' -> Win
                   'Y' -> Draw
                   'X' -> Loss

findShape :: Shape -> Outcome -> Shape
findShape s o = case o of 
                     Win  -> beats
                     Draw -> s
                     Loss -> findShape beats Win 
                where beats = case s of
                                   Rock     -> Paper
                                   Paper    -> Scissors
                                   Scissors -> Rock

toGame :: String -> Game
toGame (opp:_:you:_) = (toShape opp, toShape you)

toGuide :: String -> Guide
toGuide (opp:_:strat:_) = (toShape opp, toOutcome strat)

toGuideGame :: Guide -> Game
toGuideGame (opp, out) = (opp, findShape opp out)

outcome :: Game -> Outcome
outcome (Rock    , Scissors) = Loss
outcome (Paper   , Rock    ) = Loss
outcome (Scissors, Paper   ) = Loss
outcome (Rock    , Rock    ) = Draw
outcome (Paper   , Paper   ) = Draw
outcome (Scissors, Scissors) = Draw
outcome (_       , _       ) = Win

scoreOutcome :: Outcome -> Int
scoreOutcome Win  = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

scoreShape :: Shape -> Int
scoreShape Rock     = 1
scoreShape Paper    = 2
scoreShape Scissors = 3

score :: Game -> Int
score g@(opp, you) = (scoreOutcome (outcome g)) + (scoreShape you)

part1 :: String -> Int
part1 s = foldl (+) 0 (map score (map toGame (split s '\n')))

part2 :: String -> Int
part2 s = foldl (+)
                0
                (map Two.score 
                     (map Two.toGuideGame 
                          (map Two.toGuide 
                               (Two.split s '\n'))))
