-- | This package provides a framework for constructing variadic
--   functions as folds over function applications.
--
--   For example, a variadic function @f@ may be reduced like this:
--
-- @
-- f a b c
-- ≡ foldlApp @(~) f' z a b c
-- ≡ f' (f' (f' z a) b) c
-- @
--
--   Both left and right associative folds are available.
--
--   This module re-exports "Data.FoldApp.Identity" which assumes the
--   identity conversion for arguments. "Data.FoldApp.Generic" provides
--   the generalised folds where a different converter may be given.
--   Conversion allows for folding over differently-typed arguments by
--   converting them to a common type.
--
--   "Data.FoldApp.Function" contains several variadic functions which
--   may be useful as examples or in your programs.
--
--   Folds cannot be defined to return functions. This is because the
--   parameters intended for the returned function become confused with
--   the parameters intended for folding. This weakness can be
--   circumvented by wrapping and unwrapping returned functions with
--   a newtype at the cost of inconvenience.
--
--   If a type inference problem arises, you possibly need to provide
--   an annotation for some arguments or an annotation for the return
--   type. For example, without any other typing context the following
--   is ambiguous …
--
-- @
-- listOf "hello" "sailor!"
-- @
--
--   … because it is not known how many more arguments should be
--   accepted. An annotation such as the following fixes this problem …
--
-- @
-- listOf "hello" "sailor!" :: String -> [String]
-- @
--
--   … saying one more @String@ argument must be given and a @[String]@
--   will be returned.
--
module Data.FoldApp
  ( module Data.FoldApp.Function
  , module Data.FoldApp.Identity
  )
where

import Data.FoldApp.Function

import Data.FoldApp.Identity
