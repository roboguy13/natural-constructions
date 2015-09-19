{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-} {-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ConstraintKinds           #-}

module Data.Natural
  where

import           Data.Bifunctor
import           Data.Proxy

type Natural  p f g = forall a.           p (f a) (g a)
data Natural' p f g = forall a. Natural' (p (f a) (g a))

infixr ~>
type f ~> g = Natural (->)   f g
type f ** g = Natural (,)    f g
type f ++ g = Natural Either f g

type f :~> g = Natural' (->)   f g
type f :** g = Natural' (,)    f g
type f :++ g = Natural' Either f g

pattern fa :** ga = Natural' (fa, ga)

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

type End  f = forall a. f a
data End' f = forall a. End' (f a)

endApply :: (forall a. f a -> b) -> End' f -> b
endApply f (End' e) = f e

data Constr c a = c a => Constr (Proxy c)

constrApply :: (forall a. c a => g a -> b) -> Constr c :** g -> b
constrApply f (Constr Proxy :** ga) = f ga

natFst :: f :** g -> End' f
natFst (fa :** _) = End' fa

natSnd :: f :** g -> End' g
natSnd (_ :** ga) = End' ga

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

natFirst' :: Bifunctor p => (f ~> f') -> Natural' p f g -> Natural' p f' g
natFirst' tr (Natural' p) = Natural' $ first tr p

natSecond' :: Bifunctor p => (g ~> g') -> Natural' p f g -> Natural' p f g'
natSecond' tr (Natural' p) = Natural' $ second tr p
