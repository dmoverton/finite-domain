module Main where

import FD
import Sudoku
import Queens2
import SendMoreMoney

testPuzzle :: Puzzle
testPuzzle = [
    0, 0, 0, 0, 8, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 6, 5, 0, 7,
    4, 0, 2, 7, 0, 0, 0, 0, 0,
    0, 8, 0, 3, 0, 0, 1, 0, 0,
    0, 0, 3, 0, 0, 0, 8, 0, 0,
    0, 0, 5, 0, 0, 9, 0, 7, 0,
    0, 5, 0, 0, 0, 8, 0, 0, 6,
    3, 0, 1, 2, 0, 4, 0, 0, 0,
    0, 0, 6, 0, 1, 0, 0, 0, 0 ]

hard1 :: Puzzle
hard1 = [
    1, 0, 0, 0, 0, 0, 0, 0, 2,
    0, 9, 0, 4, 0, 0, 0, 5, 0,
    0, 0, 6, 0, 0, 0, 7, 0, 0,
    0, 5, 0, 9, 0, 3, 0, 0, 0,
    0, 0, 0, 0, 7, 0, 0, 0, 0,
    0, 0, 0, 8, 5, 0, 0, 4, 0,
    7, 0, 0, 0, 0, 0, 6, 0, 0,
    0, 3, 0, 0, 0, 9, 0, 8, 0,
    0, 0, 2, 0, 0, 0, 0, 0, 1 ]
 
hard2 :: Puzzle
hard2 = [
    0, 0, 1, 0, 0, 4, 0, 0, 0,
    0, 0, 0, 0, 6, 0, 3, 0, 5,
    0, 0, 0, 9, 0, 0, 0, 0, 0,
    8, 0, 0, 0, 0, 0, 7, 0, 3,
    0, 0, 0, 0, 0, 0, 0, 2, 8,
    5, 0, 0, 0, 7, 0, 6, 0, 0,
    3, 0, 0, 0, 8, 0, 0, 0, 6,
    0, 0, 9, 2, 0, 0, 0, 0, 0,
    0, 4, 0, 0, 0, 1, 0, 0, 0 ]
 


main = do
    putStrLn "Sudoku hard2"
    printSudoku hard2
    putStrLn "8 Queens"
    print $ runFD $ nQueens 8 >>= labelling
    -- putStrLn "send + more = money"
    -- print $ zip ['s', 'e', 'n', 'd', 'm', 'o', 'r', 'y'] sendMoreMoney


test = do
    putStrLn "test"
    print $ runFD $ do
        vars@[a, b, c] <- news 3 (1, 9)
        a #== b * c
        labelling vars
