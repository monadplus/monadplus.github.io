---
title: Introduction to singletons in Haskell and Sigma types
description: This blog introduces the notion of singleton and sigma types with a couple of compeling examples...
categories:
 - haskell
tags:
 - haskell
 - type-level
 - singleton
 - sigma type
 - dependent types
---

The code is the post, just read the code and enjoy!

```haskell
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}
module Sing where

data Nat = Z | S Nat
  deriving Eq

instance Show Nat where
  show = show . go 0
    where
      go acc Z = acc
      go acc (S n) = go (acc + 1) n

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

toNat :: SNat n -> Nat
toNat SZ = Z
toNat (SS sn) = S (toNat sn)

-- This direction is tricky.
-- Because we don't know n (it can be a runtime value)

-- | Wrong implementation
fromNat :: (SNat n -> r) -> Nat -> r
fromNat f nat = f (fromNat' nat)
  where
    fromNat' :: Nat -> SNat n
    fromNat' Z = SZ
    fromNat' (S nat) = SS (fromNat' nat)
-- It will fail to compile because n is "existential" (from the compiler POV).
-- `n` can never be defined outisde the rank-2 scope.

fromNat :: (forall n. SNat n -> r) -> Nat -> r
fromNat f Z = f SZ
fromNat f (S n) = fromNat (\some -> f (SS some)) n

-- >>> let nat = S (S ( S( S Z)))
-- >>> testIso nat
-- True
testIso x = fromNat ((== x) . toNat) x

---------------------------------
---------------------------------

-- A more compeling example

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

vlength :: forall n. Vector n a -> SNat n
vlength VNil = SZ
vlength (VCons _ v) = SS (vlength v)

-- This is similar to fromNat, we don't know the length of the vector
-- when we filter it because the compiler can't statically know which
-- elements are going to pass the predicate (we need a universal quantification).
filterV :: (forall n. Vector n a -> r) -> (a -> Bool) -> Vector n a -> r
filterV f p VNil = f VNil
filterV f p (VCons a v) = filterV (\v' -> if p a then f (VCons a v') else f v') p v

-- >>> let vec = VCons 1 (VCons 2 ( VCons 3 VNil))
--
-- Although we are running it forall n, the following fails:
--
-- >>> filterV (vlength) (< 2) vec
--
--    • Couldn't match type ‘r’ with ‘SNat n1’
--        because type variable ‘n1’ would escape its scope
--
-- As I mentioned before, n can't scape its scope because
-- it has been existentialized from the compiler POV.
--
-- toNat removes the dependency on the forall n.
-- So we can finally know the length of the vector as it was a regular list.
--
-- >>> filterV (toNat . vlength) (< 2) vec
-- 1

-------------------------------
-------------------------------

-- Singleton & Sigma

type family Sing (x :: k) :: Type


data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- From Haskell's POV, types and kinds are the same.
--
-- In dependently typed languages, terms and types are the same. Not in Haskell (yet!).

-- | Bool <--> SBool
type instance Sing b = SBool b

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | Same for Nat <---> SNat
type instance Sing n = SNat n

-- In dependently-typed programming, we talk about the notion of a "Sigma type",
--   or "dependent pair". This is a single-constructor GADT that takes two
--   arguments: a singleton for some kind, and some type indexed by the type
--   represented by the singleton.

-- Don't panic, it is easier that it seems. Let's see some examples:

data Sigma (f :: k -> Type) where
  Sigma :: Sing k -> f k -> Sigma f

data Strings (n :: Nat) where
  SNil ::                        Strings  'Z
  (:>) :: String -> Strings n -> Strings ('S n)

-- | If we have a working sigma type, we should be able to package a @Strings
-- n@ and an @SNat n@ into @Sigma Strings@, existentialising the actual length.

example :: [Sigma Strings]
example = [ Sigma         SZ   SNil
          , Sigma     (SS SZ)  ("hi" :> SNil)
          , Sigma (SS (SS SZ)) ("hello" :> ("world" :> SNil))
          ]

data Vector (a :: Type) (n :: Nat) where
  VNil  ::                    Vector a  'Z
  VCons :: a -> Vector a n -> Vector a ('S n)

-- | It existentialized the indexed kind.
filterV :: (a -> Bool) -> Vector a n -> Sigma (Vector a)
filterV _ VNil = Sigma SZ VNil
filterV p (VCons a v)
  | p a = cons a (filterV p v)
  | otherwise = filterV p v
  where
    cons a (Sigma n v) = Sigma (SS n) (VCons a v)

-----------------------------------
-----------------------------------

-- A more compeling example of sigma types:

data Label = Client | Server

data ClientData
  = ClientData
      { name         :: String
      , favouriteInt :: Int
      }
  deriving Show

data ServerData
  = ServerData
      { favouriteBool     :: Bool
      , complimentaryUnit :: ()
      }
  deriving Show

data Communication (label :: Label) where
  ClientPkg :: ClientData -> Communication 'Client
  ServerPkg :: ServerData -> Communication 'Server

data SLabel (label :: Label) where
  SClient :: SLabel 'Client
  SServer :: SLabel 'Server

type instance Sing label = SLabel label

serverLog :: [Sigma Communication] -> [ServerData]
serverLog [] = []
serverLog (x:xs) = case x of
  Sigma SClient              _ -> serverLog xs
  Sigma SServer (ServerPkg pkg)-> pkg : (serverLog xs)
-- >>> sigmaComm1 = Sigma SClient (ClientPkg (ClientData "Arnau Abella" 1))
-- >>> sigmaComm2 = Sigma SServer (ServerPkg (ServerData True ()  ))
-- >>> serverLog [sigmaComm2, sigmaComm1, sigmaComm2]

-- Arguably, in this case, the Sigma type is an overkill.
--
-- We could have done the following:
data SomeCommunication where
  SomeComm :: Communication label -> SomeCommunication

serverLog' :: [SomeCommunication] -> [ServerData]
serverLog' [] = []
serverLog' (x:xs) = case x of
  SomeComm (ServerPkg serverData) -> serverData : serverLog' xs
  SomeComm (ClientPkg _         ) -> serverLog' xs
```

\*All credit to _Tom Harding_ as most of these examples are from him.

\**Also, a shout out to the book _"Thinking with Types"_ from _Sandy Maguire_ which explains this in detail (apart from many other type-level techniques).
