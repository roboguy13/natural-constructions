{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

import           Data.Natural

--
-- Example usage:
-- ghci> endApply showVect . natSnd $ dfilter even (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
-- "2 : 4 : []"
--

data Nat = Zero | Succ Nat

-- | Natural number singletons
data NatS a where
  ZeroS :: NatS Zero
  SuccS :: NatS n -> NatS (Succ n)

data Vect a (n :: Nat) where
  Nil  :: Vect a Zero
  Cons :: a -> Vect a n -> Vect a (Succ n)

dfilter :: (e -> Bool) -> Vect e n -> NatS :** (Vect e)
dfilter _ Nil = ZeroS :** Nil
dfilter f (Cons x xs)
  | f x =
      case dfilter f xs of
        n :** xs' -> SuccS n :** (Cons x xs')
  | otherwise =
      dfilter f xs

-- | For testing/demonstration purposes
showVect :: Show e => Vect e n -> String
showVect Nil = "[]"
showVect (Cons x xs) = show x ++ " : " ++ showVect xs

