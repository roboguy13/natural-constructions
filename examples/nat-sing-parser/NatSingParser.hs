{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE PolyKinds      #-}

--
-- Example Usage:
--  ghci> endApply showNatS (natSnd (parseNatS "SSZ"))
--  "SSZ"
--

import           Data.Natural

data Nat = Zero | Succ Nat

data NatS (a :: Nat) where
  ZeroS :: NatS Zero
  SuccS :: NatS n -> NatS (Succ n)

parseNatS :: String -> (NatS :** NatS)
parseNatS "Z" = ZeroS :** ZeroS
parseNatS ('S':n) =
  case parseNatS n of
    _ :** m ->
      let r = SuccS m
      in
      r :** r

showNatS :: NatS a -> String
showNatS ZeroS     = "Z"
showNatS (SuccS n) = 'S' : showNatS n

