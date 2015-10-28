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
newtype FDVar = FDVar { _unwrapFDVar :: Int } deriving (Ord, Eq)

type VarSupply = FDVar

data VarInfo = VarInfo { _delayedConstraints :: FDConstraint, _domain :: Domain }

type VarMap = Map FDVar VarInfo

data FDState = FDState { _varSupply :: VarSupply, _varMap :: VarMap }

-- The FD monad
type FD a = StateT FDState [] a

type FDConstraint = FD ()

makeLenses ''FDVar

makeLenses ''FDState

makeLenses ''VarInfo

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: FD a -> [a]
runFD fd = evalStateT fd initState

initState :: FDState
initState = FDState { _varSupply = FDVar 0, _varMap = Map.empty }

initVarInfo :: VarInfo
initVarInfo = VarInfo { _delayedConstraints = return (), _domain = maxDomain }

instance Monoid VarInfo where
    mempty = initVarInfo
    mappend vi0 vi = vi0 & delayedConstraints %~ (>> vi ^. delayedConstraints)
                         & domain <>~ (vi ^. domain)

-- Get a new FDVar
newVar :: ToDomain a => a -> FD FDVar
newVar d = do
    v <- use varSupply
    varSupply . unwrapFDVar += 1
    let vi = initVarInfo & domain .~ toDomain d
    varMap . at v ?= vi
    return v

newVars :: ToDomain a => Int -> a -> FD [FDVar]
newVars n d = replicateM n (newVar d)

-- Lookup the current domain of a variable.
lookup :: FDVar -> FD Domain
lookup x =
    use $ varMap . ix x . domain

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> Domain -> FDConstraint
update x i = do
    vi <- use $ varMap . ix x
    varMap . ix x . domain .= i
    vi ^. delayedConstraints

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar -> FDConstraint -> FDConstraint
addConstraint x constraint = do
    varMap . ix x . delayedConstraints %= (>> constraint)
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint = FDVar -> FDVar -> FDConstraint
addBinaryConstraint :: BinaryConstraint -> BinaryConstraint
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> FDConstraint
var `hasValue` val = do
    vals <- lookup var
    guard $ val `member` vals
    let i = singleton val
    when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> FDConstraint
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `intersection` yv
    guard $ not $ Domain.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar -> FDVar -> FDConstraint
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ not (isSingleton xv) || not (isSingleton yv) || xv /= yv
    when (isSingleton xv && xv `isSubsetOf` yv) $
        update y (yv `difference` xv)
    when (isSingleton yv && yv `isSubsetOf` xv) $
        update x (xv `difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar] -> FDConstraint
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
lessThan :: FDVar -> FDVar -> FDConstraint
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
solutions :: FD a -> FD [a]
solutions constraint = do
    s <- get
    return $ evalStateT constraint s

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar] -> FD [Int]
labelling = mapM label where
    label var = do
        vals <- lookup var
        val <- lift $ elems vals
        var `hasValue` val
        return val

dump :: [FDVar] -> FD [Domain]
dump = mapM lookup

data Expr
    = Int !Int
    | Var !FDVar
    | Plus !Expr !Expr
    | Minus !Expr !Expr
    | Times !Expr !Expr
    | Negate !Expr
    | Abs !Expr

new :: ToDomain a => a -> FD Expr
new d = newVar d <&> Var

news :: ToDomain a => Int -> a -> FD [Expr]
news n d = replicateM n $ new d

class ToExpr a where
    toExpr :: a -> Expr

instance ToExpr FDVar where
    toExpr = Var

instance ToExpr Int where
    toExpr = Int

instance ToExpr Expr where
    toExpr = id

interpret :: Expr -> FD FDVar
interpret (Var v) = return v
interpret (Int i) = newVar (i, i)
interpret (Plus e0 e1) = interpretBinary (+) e0 e1
interpret (Minus e0 e1) = interpretBinary (-) e0 e1
interpret (Times e0 e1) = interpretBinary (*) e0 e1
interpret (Negate e) = interpretUnary negate e
interpret (Abs e) = interpretUnary abs e

interpretBinary :: (Int -> Int -> Int) -> Expr -> Expr -> FD FDVar
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

constrainBinary :: (Int -> Int -> Int -> Bool) -> FDVar -> FDVar -> FDVar -> FDConstraint
constrainBinary pred v v0 v1 = do
    d <- lookup v
    d0 <- lookup v0
    d1 <- lookup v1
    let d' = toDomain [n | n <- elems d, n0 <- elems d0, n1 <- elems d1, pred n n0 n1]
    guard $ not $ Domain.null d'
    when (d' /= d) $ update v d'

interpretUnary :: (Int -> Int) -> Expr -> FD FDVar
interpretUnary op e0 = do
    v0 <- interpret e0
    d0 <- lookup v0
    v <- newVar [op n0 | n0<- elems d0]
    addConstraint v0 $ constrainUnary (\n n0 -> n == op n0) v v0
    addConstraint v  $ constrainUnary (\n0 n -> n == op n0) v0 v
    return v

constrainUnary :: (Int -> Int -> Bool) -> FDVar -> FDVar -> FDConstraint
constrainUnary pred v v0 = do
    d <- lookup v
    d0 <- lookup v0
    let d' = toDomain [n | n <- elems d, n0 <- elems d0, pred n n0]
    guard $ not $ Domain.null d'
    when (d' /= d) $ update v d'

infixl 6 #+
(#+) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a #+ b = Plus (toExpr a) (toExpr b)

infixl 6 #-
(#-) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a #- b = Minus (toExpr a) (toExpr b)

infixl 7 #*
(#*) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a #* b = Times (toExpr a) (toExpr b)

infix 4 #==
(#==) :: (ToExpr a, ToExpr b) => a -> b -> FDConstraint
a #== b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `same` v1

infix 4 #\=
(#\=) :: (ToExpr a, ToExpr b) => a -> b -> FDConstraint
a #\= b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `different` v1

infix 4 #<
(#<) :: (ToExpr a, ToExpr b) => a -> b -> FDConstraint
a #< b = do
    v0 <- interpret (toExpr a)
    v1 <- interpret (toExpr b)
    v0 `lessThan` v1
