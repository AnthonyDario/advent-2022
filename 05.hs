import Data.Map ((!))
import qualified Data.Map as Map (Map, adjust, fromList, foldl)
import Util

type Crate = Char
type Stack = [Crate]
type Stacks = Map.Map Int Stack
type Instruction = Stacks -> Stacks

instance Show Instruction where show _ = "Instructions"

{- Parse the Stacks -}
stacks :: [String] -> Stacks
stacks (nums:cs) = fillStacks cs (Map.fromList [(i, []) | i <- [1..(getNum nums)]])

fillStacks :: [String] -> Stacks -> Stacks
fillStacks []     ss = ss 
fillStacks (l:ls) ss = fillStacks ls (readRow l 1 ss)

getNum :: String -> Int
getNum s = read (last (split s " ")) :: Int

readRow :: String -> Int -> Stacks -> Stacks
readRow []           i m = m
readRow (a:b:c:e:cs) i m = if a == ' ' 
                           then readRow cs (i + 1) m
                           else readRow cs (i + 1) (Map.adjust (\x -> b:x) i m)
readRow (a:b:c:cs)   i m = if a == ' ' 
                           then readRow cs (i + 1) m
                           else readRow cs (i + 1) (Map.adjust (\x -> b:x) i m)

{- Parse the instructions -}
instructions :: [String] -> [Instruction]
instructions []     = []
instructions (i:is) = (readIns i) : (instructions is)

{- For part 1
readIns :: String -> Instruction
readIns i = let ss = (split i " ")
                amount = read (ss !! 1) :: Int
                from   = read (ss !! 3) :: Int
                to     = read (ss !! 5) :: Int
            in (\stacks -> Map.adjust (\x -> drop amount x) 
                                      from
                                      (Map.adjust (\x -> (reverse (take amount (stacks ! from))) ++ x)
                                                   to 
                                                   stacks))
-}
readIns :: String -> Instruction
readIns i = let ss = (split i " ")
                amount = read (ss !! 1) :: Int
                from   = read (ss !! 3) :: Int
                to     = read (ss !! 5) :: Int
            in (\stacks -> Map.adjust (\x -> drop amount x) 
                                      from
                                      (Map.adjust (\x -> (take amount (stacks ! from)) ++ x)
                                                   to 
                                                   stacks))

{- Parse everything -}
parse :: String -> (Stacks, [Instruction])
parse s = let sp = split s "\n\n"
              stackString = sp !! 0
              instString  = sp !! 1
          in (stacks (reverse (split stackString "\n")), instructions (split instString ("\n")))

{- Execute the instructions -}
move :: Stacks -> [Instruction] -> Stacks
move stacks is = foldl (\s i -> i s) stacks is

{- Print the tops -}
tops :: Stacks -> String
tops s = Map.foldl (\a (c:cs) -> a ++ [c]) "" s

main = do
    input <- readFile "inputs/05.txt"

    {- Part 1 Figure out the top crates -}
    --print (let (ss, is) = parse input in ss)
    --print (let (ss, is) = parse input in move ss is)
    print (tops (let (ss, is) = parse input in move ss is))
