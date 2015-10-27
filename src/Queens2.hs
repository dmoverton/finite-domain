module Queens (nQueens) where

import FD

nQueens :: Int -> FD s [FDVar s]
nQueens n = do
    qs <- newVars n (1, n)
    safeQueens qs
    return qs

safeQueens :: [FDVar s] -> FDConstraint s
safeQueens [] = return ()
safeQueens (q : qs) = do
    safeQueen qs q 1
    safeQueens qs

safeQueen :: [FDVar s] -> FDVar s -> Int -> FDConstraint s
safeQueen [] _ _ = return ()
safeQueen (q : qs) q0 d = do
   q0 ./=. q 
   q ./=. q0 .+. d
   q ./=. q0 .-. d
   safeQueen qs q0 (d + 1)
