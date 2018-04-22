{-# LANGUAGE LambdaCase #-}

module Prototype where

import Data.Functor.Foldable
import Control.Monad.Free

-- implementation of example 1 from wikipedia on l-system
-- https://en.wikipedia.org/wiki/L-system#Example_1:_Algae

data Grammar = A | B deriving (Show, Enum)

applyRules :: (a -> [a]) -> [a] -> [a]
applyRules f = cata $ \case
	Nil -> []
	Cons a as -> f a ++ as

iterateRules :: (a -> [a]) -> [a] -> [[a]]
iterateRules f = ana $ \case
	as -> Cons as (applyRules f as)

example1Rules :: Grammar -> [Grammar]
example1Rules A = [A, B]
example1Rules B = [A]

ex1 = iterateRules example1Rules [A]
fibs = fmap length ex1
