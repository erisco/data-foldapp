-- | The most generic definitions for folding function applications.
--
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.FoldApp.Generic
  ( Converter(convert)
  , FoldlApp(foldlApp)
  , FoldrApp()
  , Monad
  , foldlMApp
  , foldrApp
  , foldrMApp
  )
where

import Control.Monad
  ( Monad((>>=), return)
  )
--

import Data.Kind
  ( Constraint
  )
--

import Prelude
  ( id
  , flip
  )
--

-- | Class of constraints which feature a function to convert a value of
--   one type to a value of another.
--
class Converter (conv :: * -> * -> Constraint) where
  convert :: conv a b => a -> b
--

instance Converter (~) where
  convert = id
--

-- | Constrain all parameters of f to be convertible to p and the return
--   of f to be r.
--
type family
  Infer (conv :: * -> * -> Constraint)
        (p    :: *                   )
        (r    :: *                   )
        (f    :: *                   )
        :: Constraint
  where
  Infer conv p r (a -> f) = (conv a p, Infer conv p r f)
  Infer _    _ r s        = r ~ s
--

-- | Class defining left-associative folds of function applications. No
--   other instances need be defined.
--
class
  ( Converter conv
  , Infer conv p r f
  ) =>
  FoldlApp (conv :: * -> * -> Constraint)
           (p    :: *                   )
           (r    :: *                   )
           (f    :: *                   )
  where
  -- | Left-associative fold of function applications.
  foldlApp :: (r -> p -> r) -> r -> f
--

instance (Converter conv, Infer conv p r r) => FoldlApp conv p r r where
  foldlApp _ r = r
--

instance
  ( Converter conv
  , Infer conv p r (x -> f)
  , FoldlApp conv p r f
  ) =>
  FoldlApp conv p r (x -> f)
  where
  foldlApp f r p = foldlApp @conv f (f r (convert @conv p))
--

-- | Monadic left-associative fold of function applications.
--
foldlMApp ::
  forall conv m p r f.
  (Monad m, FoldlApp conv p (m r) f) =>
  (r -> p -> m r) -> r -> f
foldlMApp f r = foldlApp @conv (\r' p -> r' >>= flip f p) (return r)

-- | Class defining right-associative folds of function applications. No
--   other instances need be defined.
--
class
  ( Converter conv
  , Infer conv p r f
  ) =>
  FoldrApp (conv :: * -> * -> Constraint)
           (p    :: *                   )
           (r    :: *                   )
           (f    :: *                   )
  where
  -- | Right-associative fold of function applications. This is an
  --   internal implementation; use 'foldrApp' instead.
  foldrAppImpl :: (p -> r -> r) -> (r -> r) -> r -> f
--

instance (Converter conv, Infer conv p r r) => FoldrApp conv p r r where
  foldrAppImpl _ g r = g r
--

instance
 ( Converter conv
 , Infer conv p r (x -> f)
 , FoldrApp conv p r f
 ) => FoldrApp conv p r (x -> f)
 where
  foldrAppImpl f g r p =
    foldrAppImpl @conv f (\r' -> g (f (convert @conv p) r')) r
--

-- | Right-associative fold of function applications.
--
foldrApp ::
  forall conv p r f.
  FoldrApp conv p r f =>
  (p -> r -> r) -> r -> f
foldrApp f = foldrAppImpl @conv f id

-- | Monadic right-associative fold of function applications.
--
foldrMApp ::
  forall conv m p r f.
  (Monad m, FoldrApp conv p (m r) f) =>
  (p -> r -> m r) -> r -> f
foldrMApp f r = foldrApp @conv (\p r' -> r' >>= f p) (return r)
