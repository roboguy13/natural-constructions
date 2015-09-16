{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Data.Natural
  where

import           Data.Bifunctor

type Natural p f g = forall a. p (f a) (g a)
type BiNat   p f g = Bifunctor p => Natural p f g

type f ~> g = Natural (->)   f g
infixr ~>

-- Two examples of BiNats (not written with BiNat for a more clear analogy
-- to the definition of (~>)):
type f **  g = Natural (,)    f g
type f ++  g = Natural Either f g

-- | These '.' prefixed versions of things are used when chaining things
-- together. For instance, if we want a two argument (curried) natural
-- transformation we could have
--     example :: (f ~> g .~> h) -> Int
-- This is necessary so that the forall'd variable gets "propagated", since
-- what we really want to end up with is (after expanding the type
-- synonyms):
--     example :: (forall a. f a -> g a -> h a) -> Int
-- The '.' prefixed versions propagate the forall'd variable in this way.
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

apply :: ((f .~> g) .** f) ~> g
apply (tr, fa) = tr fa

natFirst :: (f ~> f') -> BiNat p f g -> BiNat p f' g
natFirst tr p = first tr p

natSecond :: (g ~> g') -> BiNat p f g -> BiNat p f g'
natSecond tr p = second tr p
