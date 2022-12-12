data Shape = Rock | Paper | Scissors deriving Show
data Outcome = Win | Loss | Draw
type Game = (Shape, Shape)


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

toGame :: String -> Game
toGame (opp:_:you:_) = (toShape opp, toShape you)

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

main = do
    input <- readFile "inputs/02.txt"
    print (foldl (+) 0 (map score (map toGame (split input '\n'))))
