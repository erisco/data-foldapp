-- | Module of variadic functions.
--
--   All types used are re-exported.
--
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.FoldApp.Function
  ( Alternative
  , Bool
  , FoldlApp
  , FoldrApp
  , IntMap
  , IntSet
  , Map
  , MonadPlus
  , Monoid
  , NonEmpty
  , Num
  , Ord
  , Ordering
  , Seq
  , Set
  , allOf
  , allOfBy
  , anyOf
  , anyOfBy
  , asumOf
  , dualAsumOf
  , dualFoldOf
  , dualMsumOf
  , firstOf
  , foldOf
  , intSetOf
  , lastOf
  , lazyIntMapOf
  , lazyMapOf
  , listOf
  , maxOf
  , maxOfBy
  , minOf
  , minOfBy
  , msumOf
  , nonEmptyOf
  , productOf
  , reverseNonEmptyOf
  , reverseOf
  , reverseSeqOf
  , seqOf
  , setOf
  , strictIntMapOf
  , strictMapOf
  , sumOf
  )
where

import Control.Applicative
  ( Alternative(empty, (<|>))
  )
--

import Control.Monad
  ( MonadPlus(mzero, mplus)
  )
--

import Data.Bool
  ( Bool(True, False)
  , (||)
  , (&&)
  )
--

import Data.FoldApp.Identity
  ( FoldlApp
  , FoldrApp
  , foldlApp
  , foldrApp
  )
--

import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as LazyIntMap
import qualified Data.IntMap.Strict as StrictIntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Map (Map)
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as StrictMap

import Data.Monoid
  ( Monoid(mempty, mappend)
  )
--

import Data.Ord
  ( Ord
  , Ordering(LT, EQ, GT)
  )
--

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Set(Set)
import qualified Data.Set as Set

import Prelude
  ( Num((+), (*))
  , (.)
  , flip
  , max
  , min
  , uncurry
  )
--

-- | True if all arguments are True, False otherwise.
--
allOf :: FoldrApp Bool Bool f => f
allOf = foldrApp (&&) True

-- | True if all arguments map to True, False otherwise.
--
allOfBy :: FoldlApp a Bool f => (a -> Bool) -> f
allOfBy f = foldlApp (\a x -> a && f x) True

-- | True if any arguments are True, False otherwise.
--
anyOf :: FoldrApp Bool Bool f => f
anyOf = foldrApp (||) False

-- | True if any arguments map to True, False otherwise.
--
anyOfBy :: FoldlApp a Bool f => (a -> Bool) -> f
anyOfBy f = foldlApp (\a x -> a || f x) False

-- | Concatentate all arguments with @\<|\>@.
--
asumOf :: forall a f g. (Alternative f, FoldrApp (f a) (f a) g) => g
asumOf = foldrApp (<|>) (empty :: f a)

-- | Concatenate all arguments with @\<|\>@ in reverse order.
--
dualAsumOf :: forall a f g. (Alternative f, FoldrApp (f a) (f a) g) => g
dualAsumOf = foldrApp (flip (<|>)) (empty :: f a)

-- | Concatenate all argments with @mappend@ in reverse order.
--
dualFoldOf :: forall a f. (Monoid a, FoldlApp a a f) => f
dualFoldOf = foldlApp (flip mappend) (mempty :: a)

-- | Concatenate all arguments with @mplus@ in reverse order.
--
dualMsumOf :: forall a m f. (MonadPlus m, FoldrApp (m a) (m a) f) => f
dualMsumOf = foldrApp (flip mplus) (mzero :: m a)

-- | Returns the first argument.
--
firstOf :: forall a f. FoldlApp a a f => a -> f
firstOf = foldlApp ((\x _ -> x) :: a -> a -> a)

-- | Concatenate all arguments with @mappend@.
--
foldOf :: forall a f. (Monoid a, FoldrApp a a f) => f
foldOf = foldrApp mappend (mempty :: a)

-- | Form an @IntSet@ from all arguments.
--
intSetOf :: forall f. FoldlApp IntSet.Key IntSet f => f
intSetOf = foldlApp (flip IntSet.insert) IntSet.empty

-- | Returns the last argument.
--
lastOf :: forall a f. FoldlApp a a f => a -> f
lastOf = foldlApp (\_ y -> y)

-- | Form a lazy @IntMap@ from all arguments.
--
lazyIntMapOf ::
  forall a f.
  FoldlApp (StrictIntMap.Key, a) (IntMap a) f =>
  f
lazyIntMapOf =
  foldlApp
  (flip (uncurry LazyIntMap.insert))
  (LazyIntMap.empty :: IntMap a)
--

-- | Form a lazy @Map@ from all arguments.
--
lazyMapOf :: forall k a f. (Ord k, FoldlApp (k, a) (Map k a) f) => f
lazyMapOf =
  foldlApp
  (flip (uncurry LazyMap.insert))
  (LazyMap.empty :: Map k a)
--

-- | Form a list from all arguments.
--
listOf :: forall a f. FoldrApp a [a] f => f
listOf = foldrApp (:) ([] :: [a])

-- | Return the largest argument.
--
maxOf :: forall a f. (Ord a, FoldlApp a a f) => a -> f
maxOf = foldlApp max

-- | Return the largest argument by the given comparator.
--
maxOfBy ::
  forall a f.
  (Ord a, FoldlApp a a f) =>
  (a -> a -> Ordering) -> a -> f
maxOfBy f = foldlApp (\a x -> case f a x of GT -> a; _ -> x)

-- | Return the smallest argument.
--
minOf :: forall a f. (Ord a, FoldlApp a a f) => a -> f
minOf = foldlApp min

-- | Return the smallest argument by the given comparator.
--
minOfBy ::
  forall a f.
  (Ord a, FoldlApp a a f) =>
  (a -> a -> Ordering) -> a -> f
minOfBy f = foldlApp (\a x -> case f a x of GT -> x; _ -> a)

-- | Concatentate all arguments with @mplus@.
--
msumOf :: forall a m f. (MonadPlus m, FoldrApp (m a) (m a) f) => f
msumOf = foldrApp mplus (mzero :: m a)

-- | Form a @NonEmpty@ list from all arguments.
--
nonEmptyOf :: FoldrApp a (NonEmpty a) f => a -> f
nonEmptyOf = foldrApp (\a (x:|xs) -> x:|(a:xs)) . (:|[])

-- | Return the product of all arguments.
--
productOf :: forall a f. (Num a, FoldlApp a a f) => f
productOf = foldlApp (*) (1 :: a)

-- | Form a @NonEmpty@ list in reverse order from all arguments.
--
reverseNonEmptyOf :: FoldlApp a (NonEmpty a) f => a -> f
reverseNonEmptyOf = foldlApp (\(x:|xs) a -> a:|(x:xs)) . (:|[])

-- | Form a list in reverse order from all arguments.
--
reverseOf :: forall a f. FoldlApp a [a] f => f
reverseOf = foldlApp (flip (:)) ([] :: [a])

-- | Form a @Seq@ from all arguments.
--
seqOf :: forall a f. FoldlApp a (Seq a) f => f
seqOf = foldlApp (Seq.|>) (Seq.empty :: Seq a)

-- | Form a @Seq@ in reverse order from all arguments.
--
reverseSeqOf :: forall a f. FoldlApp a (Seq a) f => f
reverseSeqOf = foldlApp (flip (Seq.<|)) (Seq.empty :: Seq a)

-- | Form a @Set@ from all arguments.
--
setOf :: forall a f. (Ord a, FoldlApp a (Set a) f) => f
setOf = foldlApp (flip Set.insert) (Set.empty :: Set a)

-- | Form a strict @IntMap@ from all arguments.
--
strictIntMapOf ::
  forall a f.
  FoldlApp (StrictIntMap.Key, a) (IntMap a) f =>
  f
strictIntMapOf =
  foldlApp
  (flip (uncurry StrictIntMap.insert))
  (StrictIntMap.empty :: IntMap a)
--

-- | Form a strict @Map@ from all arguments.
--
strictMapOf ::
  forall k a f.
  (Ord k, FoldlApp (k, a) (Map k a) f) =>
  f
strictMapOf =
  foldlApp
  (flip (uncurry StrictMap.insert))
  (StrictMap.empty :: Map k a)
--

-- | Return the sum of all arguments.
--
sumOf :: forall a f. (Num a, FoldlApp a a f) => f
sumOf = foldlApp (+) (0 :: a)
