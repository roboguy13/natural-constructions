{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Data.Natural
  where

import           Data.Bifunctor

type Natural p f g = forall a. p (f a) (g a)

infixr ~>
type f ~> g = Natural (->)   f g
type f ** g = Natural (,)    f g
type f ++ g = Natural Either f g

-- | These '.' prefixed versions of things are used when composing natural
-- constructions together. For instance, if we want a two argument
-- (curried) natural transformation we could have
--
--     example :: (f ~> g .~> h) -> Int
--
-- This is necessary so that the forall'd variable gets "propagated", since
-- what we really want to end up with is (after expanding the type
-- synonyms):
--
--     example :: (forall a. f a -> g a -> h a) -> Int
--
-- The '.' prefixed versions (when combined either with the non-'.'
-- versions or with End) propagate the forall'd variable in this way.
--
-- This is how End could be used to accomplish the above example instead of
-- mixing in the non-'.' operators:
--
--     example :: End (f .~> g .~> h) -> Int
type (f .~> g) a = f a -> g a
infixr .~>

type (f .** g) a = (f a, g a)
type (f .++ g) a = Either (f a) (g a)

type End f = forall a. f a

natFst :: f ** g -> f a
natFst (fa, _) = fa

natSnd :: f ** g -> g a
natSnd (_, ga) = ga

natCurry :: ((f .** g) ~> h) -> (f ~> g .~> h)
natCurry f x y = f (x, y)

natUncurry :: (f ~> g .~> h) -> ((f .** g) ~> h)
natUncurry f (x, y) = f x y

internalApply :: ((f .~> g) .** f) ~> g
internalApply (tr, fa) = tr fa

apply :: ((f .~> g) ** f) -> g b
apply (tr, fa) = tr fa

-- These two work with both (**) and (++) because of the Bifunctor
-- constraint:
natFirst :: Bifunctor p => (f ~> f') -> Natural p f g -> Natural p f' g
natFirst tr p = first tr p

natSecond :: Bifunctor p => (g ~> g') -> Natural p f g -> Natural p f g'
natSecond tr p = second tr p
