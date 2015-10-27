module Queens (queens) where

import Control.Monad (zipWithM_)
import FD

queens n = runFD $ do
    vars <- newVars n (1, n)
    allDifferent vars
    diagonals vars
    labelling vars
    
diagonals :: [FDVar s] -> FD s ()
diagonals [] = return ()
diagonals (x:xs) = do
    zipWithM_ (diag x) xs [1..]
    diagonals xs

diag :: FDVar s -> FDVar s -> Int -> FD s ()
diag x y n = do
    y ./=. x .+. n
    y ./=. x .-. n
