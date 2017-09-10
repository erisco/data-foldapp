-- | Specialised functions for folds of function applications. The
--   converter has been specialised to the identity converter.
--
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Data.FoldApp.Identity
  ( FoldlApp
  , FoldrApp
  , foldlApp
  , foldlMApp
  , foldrApp
  , foldrMApp
  , Monad
  )
where

import Control.Monad
  ( Monad
  )
--

import qualified Data.FoldApp.Generic as G

-- | @Data.FoldApp.FoldlApp@ with the identity converter chosen.
--
type FoldlApp p r f = G.FoldlApp (~) p r f

-- | @Data.FoldApp.FoldrAPp@ with the identity converter chosen.
--
type FoldrApp p r f = G.FoldrApp (~) p r f

-- | Left-associative fold of function applications.
--
foldlApp ::
  forall p r f.
  (FoldlApp p r f) =>
  (r -> p -> r) -> r -> f
foldlApp = G.foldlApp @(~)

-- | Monadic left-associative fold of function applications.
--
foldlMApp ::
  forall m p r f.
  (Monad m, FoldlApp p (m r) f) =>
  (r -> p -> m r) -> r -> f
foldlMApp = G.foldlMApp @(~)

-- | Right-associative fold of function applications.
--
foldrApp ::
  forall p r f.
  FoldrApp p r f =>
  (p -> r -> r) -> r -> f
foldrApp = G.foldrApp @(~)

-- | Monadic right-associative fold of function applications.
--
foldrMApp ::
  forall m p r f.
  (Monad m, FoldrApp p (m r) f) =>
  (p -> r -> m r) -> r -> f
foldrMApp = G.foldrMApp @(~)
