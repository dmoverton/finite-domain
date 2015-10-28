{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module FD (
    -- Types
    FD,           -- Monad for finite domain constraint solver
    FDConstraint,
    FDVar,        -- Finite domain solver variable

    -- Functions
    runFD,        -- Run the monad and return a list of solutions.
    newVar,       -- Create a new FDVar
    newVars,      -- Create multiple FDVars
    hasValue,     -- Constrain a FDVar to a specific value
    same,         -- Constrain two FDVars to be the same
    different,    -- Constrain two FDVars to be different
    allDifferent, -- Constrain a list of FDVars to be different
    labelling,    -- Backtracking search for all solutions
    solutions,
    dump,
    (#==),
    (#\=),
    (#<),
    (#*),
    (#+),
    (#-),
    Expr,
    ToExpr(..),
    ) where

import Prelude hiding (lookup)
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Control.Lens
import Data.Map.Lens
import Data.Maybe

import Domain

-- FD variables
newtype FDVar s = FDVar { _unwrapFDVar :: Int } deriving (Ord, Eq)

type VarSupply s = FDVar s

data VarInfo s = VarInfo
     { _delayedConstraints :: FDConstraint s, _domain :: Domain }

type VarMap s = Map (FDVar s) (VarInfo s)

data FDState s = FDState
     { _varSupply :: VarSupply s, _varMap :: VarMap s }

-- The FD monad
type FD s a = StateT (FDState s) [] a

type FDConstraint s = FD s ()

makeLenses ''FDVar

makeLenses ''FDState

makeLenses ''VarInfo

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: (forall s . FD s a) -> [a]
runFD fd = evalStateT fd initState

initState :: FDState s
initState = FDState { _varSupply = FDVar 0, _varMap = Map.empty }

initVarInfo :: VarInfo s
initVarInfo = VarInfo { _delayedConstraints = return (), _domain = maxDomain }

instance Monoid (VarInfo s) where
    mempty = initVarInfo
    mappend vi0 vi = vi0 & delayedConstraints %~ (>> vi ^. delayedConstraints)
                         & domain <>~ (vi ^. domain)

-- Get a new FDVar
newVar :: ToDomain a => a -> FD s (FDVar s)
newVar d = do
    v <- use varSupply
    varSupply . unwrapFDVar += 1
    let vi = initVarInfo & domain .~ toDomain d
    varMap . at v ?= vi
    return v

newVars :: ToDomain a => Int -> a -> FD s [FDVar s]
newVars n d = replicateM n (newVar d)

-- Lookup the current domain of a variable.
lookup :: FDVar s -> FD s Domain
lookup x =
    use $ varMap . ix x . domain

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar s -> Domain -> FDConstraint s
update x i = do
    vi <- use $ varMap . ix x
    varMap . ix x . domain .= i
    vi ^. delayedConstraints

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar s -> FDConstraint s -> FDConstraint s
addConstraint x constraint = do
    varMap . ix x . delayedConstraints %= (>> constraint)
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint s = FDVar s -> FDVar s -> FDConstraint s
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar s -> Int -> FDConstraint s
var `hasValue` val = do
    vals <- lookup var
    guard $ val `member` vals
    let i = singleton val
    when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FDConstraint s
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `intersection` yv
    guard $ not $ Domain.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FDConstraint s
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ not (isSingleton xv) || not (isSingleton yv) || xv /= yv
    when (isSingleton xv && xv `isSubsetOf` yv) $
        update y (yv `difference` xv)
    when (isSingleton yv && yv `isSubsetOf` xv) $
        update x (xv `difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FDConstraint s
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
lessThan :: FDVar s -> FDVar s -> FDConstraint s
lessThan = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = filterLessThan (findMax yv) xv
    let yv' = filterGreaterThan (findMin xv) yv
    guard $ not $ Domain.null xv'
    guard $ not $ Domain.null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- Get all solutions for a constraint without actually updating the
-- constraint store.
solutions :: FD s a -> FD s [a]
solutions constraint = do
    s <- get
    return $ evalStateT constraint s

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar s] -> FD s [Int]
labelling = mapM label where
    label var = do
        vals <- lookup var
        val <- lift $ elems vals
        var `hasValue` val
        return val

dump :: [FDVar s] -> FD s [Domain]
dump = mapM lookup

data Expr s
    = Int Int
    | Var (FDVar s)
    | Plus (Expr s) (Expr s)
    | Minus (Expr s) (Expr s)
    | Times (Expr s) (Expr s)
    | Negate (Expr s)
    | Abs (Expr s)

class ToExpr s a | a -> s where
    toExpr :: a -> Expr s

instance ToExpr s (FDVar s) where
    toExpr = Var

-- instance ToExpr s Int where
--    toExpr = Int

instance ToExpr s (Expr s) where
    toExpr = id

interpret :: Expr s -> FD s (FDVar s)
interpret (Var v) = return v
interpret (Int i) = newVar (i, i)
interpret (Plus e0 e1) = interpretBinary (+) e0 e1
interpret (Minus e0 e1) = interpretBinary (-) e0 e1
interpret (Times e0 e1) = interpretBinary (*) e0 e1
interpret (Negate e) = interpretUnary negate e
interpret (Abs e) = interpretUnary abs e

interpretBinary :: (Int -> Int -> Int) -> Expr s -> Expr s -> FD s (FDVar s)
interpretBinary op e0 e1 = do
    v0 <- interpret e0
    v1 <- interpret e1
    d0 <- lookup v0
    d1 <- lookup v1
    v <- newVar [n0 `op` n1 | n0 <- elems d0, n1 <- elems d1]
    let pc  = constrainBinary (\n n0 n1 -> n == n0 `op` n1) v v0 v1
        nc0 = constrainBinary (\n0 n n1 -> n == n0 `op` n1) v0 v v1
        nc1 = constrainBinary (\n1 n n0 -> n == n0 `op` n1) v1 v v1
    addConstraint v0 $ pc >> nc1
    addConstraint v1 $ pc >> nc0
    addConstraint v  $ nc0 >> nc1
    return v

constrainBinary :: (Int -> Int -> Int -> Bool) -> FDVar s -> FDVar s -> FDVar s -> FDConstraint s
constrainBinary pred v v0 v1 = do
    d <- lookup v
    d0 <- lookup v0
    d1 <- lookup v1
    let d' = toDomain [n | n <- elems d, n0 <- elems d0, n1 <- elems d1, pred n n0 n1]
    guard $ not $ Domain.null d'
    when (d' /= d) $ update v d'

interpretUnary :: (Int -> Int) -> Expr s -> FD s (FDVar s)
interpretUnary op e0 = do
    v0 <- interpret e0
    d0 <- lookup v0
    v <- newVar [op n0 | n0<- elems d0]
    addConstraint v0 $ constrainUnary (\n n0 -> n == op n0) v v0
    addConstraint v  $ constrainUnary (\n0 n -> n == op n0) v0 v
    return v

constrainUnary :: (Int -> Int -> Bool) -> FDVar s -> FDVar s -> FDConstraint s
constrainUnary pred v v0 = do
    d <- lookup v
    d0 <- lookup v0
    let d' = toDomain [n | n <- elems d, n0 <- elems d0, pred n n0]
    guard $ not $ Domain.null d'
    when (d' /= d) $ update v d'

infixl 6 #+
(#+) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
a #+ b = Plus (toExpr a) (toExpr b)

infixl 6 #-
(#-) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
a #- b = Minus (toExpr a) (toExpr b)

infixl 7 #*
(#*) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
a #* b = Times (toExpr a) (toExpr b)

infix 4 #==
(#==) :: (ToExpr s a, ToExpr s b) => a -> b -> FDConstraint s
a #== b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `same` v1

infix 4 #\=
(#\=) :: (ToExpr s a, ToExpr s b) => a -> b -> FDConstraint s
a #\= b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `different` v1

infix 4 #<
(#<) :: (ToExpr s a, ToExpr s b) => a -> b -> FDConstraint s
a #< b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `lessThan` v1
