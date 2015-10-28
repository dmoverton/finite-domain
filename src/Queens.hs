module Queens (queens) where

import Control.Monad (zipWithM_)
import FD

queens n = runFD $ do
    vars <- news n (1, n)
    allDifferent vars
    diagonals vars
    labelling vars
    
diagonals :: [FDExpr] -> FDConstraint
diagonals [] = return ()
diagonals (x:xs) = do
    zipWithM_ (diag x) xs [1..]
    diagonals xs

diag :: FDExpr -> FDExpr -> FDExpr -> FDConstraint
diag x y n = do
    y #\= x + n
    y #\= x - n
