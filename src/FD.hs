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
    (#<),        -- Constrain one FDVar to be less than another
    labelling,    -- Backtracking search for all solutions
    solutions,
    dump,
    (#==),
    (#\=),
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
     { _delayedConstraints :: FD s (), _domain :: Domain }

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
update :: FDVar s -> Domain -> FD s ()
update x i = do
    vi <- use $ varMap . ix x
    varMap . ix x . domain .= i
    vi ^. delayedConstraints

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar s -> FD s () -> FD s ()
addConstraint x constraint = do
    varMap . ix x . delayedConstraints %= (>> constraint)
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar s -> Int -> FD s ()
var `hasValue` val = do
    vals <- lookup var
    guard $ val `member` vals
    let i = singleton val
    when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FD s ()
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `intersection` yv
    guard $ not $ Domain.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FD s ()
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ not (isSingleton xv) || not (isSingleton yv) || xv /= yv
    when (isSingleton xv && xv `isSubsetOf` yv) $
        update y (yv `difference` xv)
    when (isSingleton yv && yv `isSubsetOf` xv) $
        update x (xv `difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FD s ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
infix 4 #<
(#<) :: FDVar s -> FDVar s -> FD s ()
(#<) = addBinaryConstraint $ \x y -> do
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

newtype Expr s = Expr { unExpr :: FD s (FDVar s) }

instance Num (Expr s) where
    (+) = (#+)
    (-) = (#-)
    (*) = (#*)
    abs = undefined
    signum = undefined
    fromInteger i = Expr $ newVar (n :: Int, n) where n = fromInteger i

class ToExpr s a | a -> s where
    toExpr :: a -> Expr s

instance ToExpr s (FDVar s) where
    toExpr = Expr . return

instance ToExpr s (Expr s) where
    toExpr = id

exprVar :: ToExpr s a => a -> FD s (FDVar s)
exprVar = unExpr . toExpr

-- Add constraint (z = x `op` y) for new var z
addArithmeticConstraint :: (ToExpr s a, ToExpr s b) =>
    (Domain -> Domain -> Domain) ->
    (Domain -> Domain -> Domain) ->
    (Domain -> Domain -> Domain) ->
    a -> b -> Expr s
addArithmeticConstraint getZDomain getXDomain getYDomain xexpr yexpr = Expr $ do
    x <- exprVar xexpr
    y <- exprVar yexpr
    xv <- lookup x
    yv <- lookup y
    z <- newVar (getZDomain xv yv)
    let zConstraint = constraint z x y getZDomain
        xConstraint = constraint x z y getXDomain
        yConstraint = constraint y z x getYDomain
    addConstraint z xConstraint
    addConstraint z yConstraint
    addConstraint x zConstraint
    addConstraint x yConstraint
    addConstraint y zConstraint
    addConstraint y xConstraint
    return z
    where
        constraint z x y getDomain = do
            xv <- lookup x
            yv <- lookup y
            zv <- lookup z
            let znew = zv `intersection` getDomain xv yv
            guard $ not $ Domain.null znew
            when (znew /= zv) $ update z znew

infixl 6 #+
(#+) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
(#+) = addArithmeticConstraint getDomainPlus getDomainMinus getDomainMinus

infixl 6 #-
(#-) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
(#-) = addArithmeticConstraint getDomainMinus getDomainPlus
    (flip getDomainMinus)

infixl 7 #*
(#*) :: (ToExpr s a, ToExpr s b) => a -> b -> Expr s
(#*) = addArithmeticConstraint getDomainMult getDomainDiv getDomainDiv

getDomainPlus :: Domain -> Domain -> Domain
getDomainPlus xs ys = toDomain (zl, zh) where
    zl = findMin xs + findMin ys
    zh = findMax xs + findMax ys

getDomainMinus :: Domain -> Domain -> Domain
getDomainMinus xs ys = toDomain (zl, zh) where
    zl = findMin xs - findMax ys
    zh = findMax xs - findMin ys

getDomainMult :: Domain -> Domain -> Domain
getDomainMult xs ys = toDomain (zl, zh) where
    zl = minimum products
    zh = maximum products
    products = [x * y |
        x <- [findMin xs, findMax xs],
        y <- [findMin ys, findMax ys]]

getDomainDiv :: Domain -> Domain -> Domain
getDomainDiv xs ys = toDomain (zl, zh) where
    zl = minimum quotients
    zh = maximum quotients
    quotients = [x `div` y |
        x <- [findMin xs, findMax xs],
        y <- [findMin ys, findMax ys]]

infix 4 #==
(#==) :: (ToExpr s a, ToExpr s b) => a -> b -> FD s ()
xexpr #== yexpr = do
    x <- exprVar xexpr
    y <- exprVar yexpr
    x `same` y

infix 4 #\=
(#\=) :: (ToExpr s a, ToExpr s b) => a -> b -> FD s ()
xexpr #\= yexpr = do
    x <- exprVar xexpr
    y <- exprVar yexpr
    x `different` y
