---
title: Cheatsheet - Haskell Extensions
description: This is a compilation of my notes about GHC language extensions...
categories:
 - haskell
tags:
 - haskell
 - ghc
---

This blog is not a replacement of the [official documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#language-options) about language extensions.

# AllowAmbiguousTypes

Each user-written type signature is subjected to an ambiguity check. The ambiguity check rejects functions that can never be called; for example:

```haskell
f :: C a => Int
```

The idea is there can be no legal calls to f because every call will give rise to an ambiguous constraint. Indeed, the only purpose of the ambiguity check is to report functions that cannot possibly be called. We could soundly omit the ambiguity check on type signatures entirely, at the expense of delaying ambiguity errors to call sites. Indeed, the language extension AllowAmbiguousTypes switches off the ambiguity check.

Getting rid of the ambiguity at the call site using TypeApplications:

```haskell
class D a b where
  h :: b
instance D Int Int where ...

main = print (h @Int @Int)
```

# GADTS

Enables type equality (a ~ b).

```haskell
data ShowList where
 ShowNil  :: ShowList
 ShowCons :: Show a => a -> ShowList -> ShowList
```

# ExistentialQuantification

```haskell
data Baz = forall a. Eq a => Baz1 a a
        | forall b. Show b => Baz2 b (b -> b)
```


# FlexibleContexts

```haskell
Constraints on instances must be of the form C (a, .., an) =>

This relaxes the rule allowing constrainst like:
                                g :: Eq [a] => ...
                                g :: Ord (T a ()) => ...
```


# FlexibleInstances (implies TypeSynonymInstances)

Allows the following:

- instances for type synonyms:

```haskell
type Point a = (a, a); instance C (Point a)
```

- instances for types that enforces equality between type variables

```haskell
instance C (Either e e) where..
```

- instances for types that aren't type variables:

```haskell
instance C (Maybe Int) where ...
instance SomeClass (Either e (Maybe a))
```

# KindSignatures

```haskell
class OfKindTypeToType (a :: Type -> Type)
```


# DataKinds

Promoted data constructors.

```haskell
data Nat = Z  | S Nat
          'Z | 'S Nat  <- Only exist on the type level, they don't have any value.
                          Useful as a phantom type.

-- Z  is a constructor of type Nat
-- 'Z is a type        of kind Nat

-- The type Nat has kind Type
-- A new kind appeared: Nat

-- >>> :t Z  = Nat, :t  S = Nat -> Nat
-- >>> :k 'Z = Nat, :k 'S = Nat -> Nat
```

# ConstraintKinds

Allows complex things on the left of a =>

```haskell
type family Typ a b :: Constraint
func :: Typ a b => a -> b -> b

type Stringy a = (Read a, Show a)
foo :: Stringy a => a -> (String, String -> a)

data HasConstraint (c :: Type -> Constraint) where
  Item :: c x => x -> HasConstraint c
```


# RankNTypes (implies ExplicitForAll)

```haskell
wrapAnyNew :: (forall x. x -> f x) -> (a, b) -> (f a, f b)

Rank1 = caller chooses the type parameter
Rank2 = callee choses the type parameter

unpack' :: (forall a. Show a => a -> r) -> Showable -> r
unpack' f (Showable x) = f x
```


# InstanceSigs

Allow writing type signatures on instance methods.

# DefaultSignatures

Allow default signatures in class. Useful for generic implementation.

# TypeFamilies

Indexed Type families (the set of types are called indices):

- data families
    - toplevel
    - associated data families (inside class)
- synonym families
    - open type synonym families (toplevel)
    - closed type synonym families (toplevel)
    - associated type families (inside class)

# TypeFamilyDependencies

```haskell
-- WRONG
type family Id a
type instance Id Int = Int
type instance Id Bool = Bool

id :: Id t -> Id t -- t is ambiguous
id x = x

-- OK
type family Id a = r | r -> a
type instance Id Int = Int
type instance Id Bool = Bool

id :: Id t -> Id t
id x = x
```

# TypeOperators

- Operator symbols become type constructors rather than type variables.

- Operator symbols in types can be written infix, both in definitions and uses:

```haskell
data a + b = Plus a b
type Foo = Int + Bool
```

# UndecidableInstances

Permit definition of instances which may lead to type-checker non-termination.

```haskell
-- Context assertion no smaller than head
instance C a => C a where ...
-- (C b b) has more occurrences of b than the head
instance C b b => Foo [b] where ...
```

__Termination is still ensured by having a fixed-depth recursion stack__. If you exceed the stack depth you get a sort of backtrace, and the opportunity to increase the stack depth with -freduction-depth=⟨n⟩.

# FunctionalDependencies

```haskell
class Elem e ce | ce -> e -- ce determines e
```

```haskell
-- WRONG
class Mult a b c where
  (*) :: a -> b -> c

instance Mult Matrix Vector Vector where
instance Mult Matrix Matrix Matrix where
instance Mult Matrix Matrix (Maybe Char) where -- Nothing prevent us from writing

m1, m2, m3 :: Matrix
(m1 * m2) * m3              -- KO; type of (m1*m2) is ambiguous
(m1 * m2) :: Matrix * m3    -- OK

-- KO
class Mult a b c | a b -> c where
  (*) :: a -> b -> c

m1, m2, m3 :: Matrix
(m1 * m2) * m3              -- OK
```

# PolyKinds (implies KindSignatures)

Possible kinds:

1.- (Type -> Type) -> Type -> Type
2.- ((Type -> Type) -> Type) -> (Type -> Type) -> Type

```haskell
data App f a = MkApp (f a)
```

With __PolyKinds__, GHC infers the kind: `forall k. (k -> Type) -> k -> Type`

```haskell
-- This needs PolyKinds
type family Id (x :: a) :: a where
  Id x = x
```


_Type family_ declarations have no right-hand side, but GHC must still infer a kind for `F`. Since there are no constraints, it could infer `F :: forall k1 k2. k1 -> k2`, but that seems _too polymorphic_.
So GHC defaults those entirely-unconstrained kind variables to `Type` and we get `F :: Type -> Type`.
You can still declare F to be _kind-polymorphic_ using kind signatures:

```haskell
type family F1 a                -- F1 :: Type -> Type
type family F2 (a :: k)         -- F2 :: forall k. k -> Type
type family F3 a :: k           -- F3 :: forall k. Type -> k
type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2
```

GHC supports kind-indexed type families:

```haskell
-- we actually can inspect the kind in our rules
type family Smuggler (x :: k) :: k where
  Smuggler (IO (Secrets, a)) = IO a
  Smuggler 0                 =    1
  Smuggler a                 =    a
```

# MultiParamTypeClasses

```haskell
class Collection c a where
    union :: c a -> c a -> c a
```

# RoleAnnotations

- nominal: must be identical (after type family reduction)
- representational: T Age representationally equal to T Int
- phantom

```haskell
data Simple a = MkSimple a          -- a has role representational

type family F
type instance F Int = Bool
type instance F Age = Char

data Complex a = MkComplex (F a)    -- a has role nominal

data Phant a = MkPhant Bool         -- a has role phantom
```

The type system ensures terms are usd correctly, the kind system ensures types are logical, the role system ensures coercions are safe.

Let's see an example:

```haskell
newtype Reverse =
  Reverse { _reversed :: String }

instance Ord Reverse where compare = ... -- compare from right to left
```

It is not safe to coerce from `Map Reverse v` to `Map String v` ?

Nope! But nothing prevents us from this ... except for the role system!

```haskell
type role Map nominal representational
```

k1 ~ k2 does not imply Map k1 v ~ Map k2 v

# ViewPatterns

`ViewPatterns` allow us to pattern match on the result of function applications.

Suppose you are using [Data.Sequence](https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html) instead a list.

The `Data.Seq` is implemented using the `Finger Tree` data structure which has a nice asymptotic cost for some operations like `last`, `append` from the right, `concat`, etc.

The only issue is that `Data.Seq` is not as ergonomic as a list because it is using a complex data structure underneath.

So the following is not possible using a seq.

```haskell
last :: Seq a -> Maybe a
last ?? = Nothing
last ?? = Just _
```

While we can’t pattern match directly on a Seq, we can view it as a list from the right by using viewr:

```haskell
data ViewR a = EmptyR | (Seq a) :> a
viewr :: Seq a -> ViewR a
```

Taking advantage of the extension `ViewPatterns`, we can pattern-match on the application of `viewr` and `viewl` and it behave like we were patter-matching on a list:

```
last :: Seq a -> Maybe a
last (viewr -> xs :> x) = Just x
last (viewr -> EmptyR) = Nothing
```

# OverlappingInstances

```
{-# OVERLAPPING #-}: between this and another, choose this.
{-# OVERLAPPABLE #-}: between this and another, choose the other.
{-# OVERLAPS #-}: only this is picked if it is more specific (if x is a valid substitution of y, but y isn't a valid substitution for x, then @x@ is more "specific")
{-# INCOHERENT #-}
```
