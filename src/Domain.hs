module Domain (
    Domain,
    ToDomain,
    toDomain,
    member,
    isSubsetOf,
    elems,
    intersection,
    difference,
    Domain.null,
    singleton,
    isSingleton,
    filterLessThan,
    filterGreaterThan,
    findMax,
    findMin,
    maxDomain
) where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

data Domain
    = Set IntSet
    | Range Int Int

-- Domain constructors
class ToDomain a where
    toDomain :: a -> Domain

instance ToDomain Domain where
    toDomain = id

instance ToDomain IntSet where
    toDomain = Set

instance Integral a => ToDomain [a] where
    toDomain = toDomain . IntSet.fromList . map fromIntegral

instance (Integral a, Integral b) => ToDomain (a, b) where
    toDomain (a, b) = Range (fromIntegral a) (fromIntegral b)

instance ToDomain () where
    toDomain () = Range minBound maxBound

--instance Integral a => ToDomain a where
--    toDomain a = toDomain (a, a)

-- Operations on Domains
instance Eq Domain where
    (Range xl xh) == (Range yl yh) = xl == yl && xh == yh
    xs == ys = elems xs == elems ys

instance Monoid Domain where
    mappend = intersection
    mempty = maxDomain

member :: Int -> Domain -> Bool
member n (Set xs) = n `IntSet.member` xs
member n (Range xl xh) = n >= xl && n <= xh

isSubsetOf :: Domain -> Domain -> Bool
isSubsetOf (Set xs) (Set ys) = xs `IntSet.isSubsetOf` ys
isSubsetOf (Range xl xh) (Range yl yh) = xl >= yl && xh <= yh
isSubsetOf (Set xs) yd@(Range _ _) =
    isSubsetOf (Range xl xh) yd where
        xl = IntSet.findMin xs
        xh = IntSet.findMax xs
isSubsetOf (Range xl xh) (Set ys) =
    all (`IntSet.member` ys) [xl..xh]

elems :: Domain -> [Int]
elems (Set xs) = IntSet.elems xs
elems (Range xl xh) = [xl..xh]

intersection :: Domain -> Domain -> Domain
intersection (Set xs) (Set ys) = Set (xs `IntSet.intersection` ys)
intersection (Range xl xh) (Range yl yh) = Range (max xl yl) (min xh yh)
intersection (Set xs) (Range yl yh) =
    Set $ IntSet.filter (\x -> x >= yl && x <= yh) xs
intersection x y = intersection y x

difference :: Domain -> Domain -> Domain
difference (Set xs) (Set ys) = Set (xs `IntSet.difference` ys)
difference xd@(Range xl xh) (Range yl yh)
    | yl > xh || yh < xl = xd
    | otherwise = Set $ IntSet.fromList [x | x <- [xl..xh], x < yl || x > yh]
difference (Set xs) (Range yl yh) =
    Set $ IntSet.filter (\x -> x < yl || x > yh) xs
difference (Range xl xh) (Set ys)
    | IntSet.findMin ys > xh || IntSet.findMax ys < xl = Range xl xh
    | otherwise = Set $
        IntSet.fromList [x | x <- [xl..xh], not (x `IntSet.member` ys)]

null :: Domain -> Bool
null (Set xs) = IntSet.null xs
null (Range xl xh) = xl > xh

singleton :: Int -> Domain
singleton x = Set (IntSet.singleton x)

isSingleton :: Domain -> Bool
isSingleton (Set xs) = case IntSet.elems xs of
    [_] -> True
    _   -> False
isSingleton (Range xl xh) = xl == xh

filterLessThan :: Int -> Domain -> Domain
filterLessThan n (Set xs) = Set $ IntSet.filter (< n) xs
filterLessThan n (Range xl xh) = Range xl (min (n-1) xh)

filterGreaterThan :: Int -> Domain -> Domain
filterGreaterThan n (Set xs) = Set $ IntSet.filter (> n) xs
filterGreaterThan n (Range xl xh) = Range (max (n+1) xl) xh

findMax :: Domain -> Int
findMax (Set xs) = IntSet.findMax xs
findMax (Range _ xh) = xh

findMin :: Domain -> Int
findMin (Set xs) = IntSet.findMin xs
findMin (Range xl _) = xl

maxDomain :: Domain
maxDomain = Range minBound maxBound
