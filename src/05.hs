module Five where

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
instructions :: [String] -> Bool -> [Instruction]
instructions []     b = []
instructions (i:is) b = (if b then readIns i else readInsTwo i) : (instructions is b)

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
readInsTwo :: String -> Instruction
readInsTwo i = let ss = (split i " ")
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
          in (stacks (reverse (split stackString "\n")), instructions (split instString ("\n")) True)

parseTwo :: String -> (Stacks, [Instruction])
parseTwo s = let sp = split s "\n\n"
                 stackString = sp !! 0
                 instString  = sp !! 1
             in (stacks (reverse (split stackString "\n")), instructions (split instString ("\n")) False)

{- Execute the instructions -}
move :: Stacks -> [Instruction] -> Stacks
move stacks is = foldl (\s i -> i s) stacks is

{- Print the tops -}
tops :: Stacks -> String
tops s = Map.foldl (\a (c:cs) -> a ++ [c]) "" s

part1 :: String -> String
part1 s = let (ss, is) = parse s in tops (move ss is)

part2 :: String -> String
part2 s = tops (let (ss, is) = parseTwo s in move ss is)
