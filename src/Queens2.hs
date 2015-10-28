module Queens2 (nQueens) where

import FD

nQueens :: Int -> FD [FDExpr]
nQueens n = do
    qs <- news n (1, n)
    safeQueens qs
    return qs

safeQueens :: [FDExpr] -> FDConstraint
safeQueens [] = return ()
safeQueens (q : qs) = do
    safeQueen qs q 1
    safeQueens qs

safeQueen :: [FDExpr] -> FDExpr -> FDExpr -> FDConstraint
safeQueen [] _ _ = return ()
safeQueen (q : qs) q0 d = do
   q0 #\= q 
   q #\= q0 + d
   q #\= q0 - d
   safeQueen qs q0 (d + 1)
