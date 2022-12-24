import qualified One
import qualified Two
import qualified Three
import qualified Four
import qualified Five
import qualified Six

main = do

    putStr ("\nDay 1\n--------------------\n")
    input1 <- readFile "inputs/01.txt"

    -- Part 1
    putStr ("Part 1: ")
    print (One.part1 input1)
    
    -- Part 2
    putStr ("Part 2: ")
    print (One.part2 input1)

    putStr ("\nDay 2\n--------------------\n")
    input2 <- readFile "inputs/02.txt"
    
    -- Part 1
    putStr ("Part 1: ")
    print (Two.part1 input2)

    -- Part 2
    putStr ("Part 2: ")
    print (Two.part2 input2)

    putStr ("\nDay 3\n--------------------\n")
    input3 <- readFile "inputs/03.txt"

    -- Part 1
    putStr ("Part 1: ")
    print (Three.part1 input3)
    
    -- Part 2
    putStr ("Part 2: ")
    print (Three.part2 input3)

    putStr ("\nDay 4\n--------------------\n")
    input4 <- readFile "inputs/04.txt"

    -- Part 1
    putStr ("Part 1: ")
    print (Four.part1 input4)
    
    -- Part 2
    putStr ("Part 2: ")
    print (Four.part2 input4)

    putStr ("\nDay 5\n--------------------\n")
    input5 <- readFile "inputs/05.txt"

    -- Part 1
    putStr ("Part 1: ")
    print (Five.part1 input5)
    
    -- Part 2
    putStr ("Part 2: ")
    print (Five.part2 input5)


    putStr ("\nDay 6\n--------------------\n")
    --input6 <- readFile "inputs/06-test.txt"
    input6 <- readFile "inputs/06.txt"

    -- Part 1
    putStr ("Part 1: ")
    print (Six.part1 input6)
    
    -- Part 2
    putStr ("Part 2: ")
    print (Six.part2 input6)
