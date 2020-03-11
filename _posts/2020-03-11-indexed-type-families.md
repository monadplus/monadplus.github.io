---
title: Indexed Type Families
description: What are type families ? What are they used for ? In this blog, we are going to answer these questions with practical examples.
categories:
 - haskell
 - type level
tags:
 - haskell
 - type level
 - type families
 - data families
 - synonym families
---

"Indexed" because the set of types are called _indices_.

# Data Families

Two kinds:

* Top level
* Associated types: inside type class (better compiler warnings)

Unlike with ordinary data definitions, __the result kind of a data family does not need to be Type__: it can alternatively be a kind variable (with PolyKinds). __Data instances’ kinds must end in Type, however__.

```haskell
data family GMap k v
data family GMap k v :: Type -- same
data family GMap k :: Type -> Type -- same
data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

-- When ExplicitForAll is enabled, type or kinds variables used on the left hand side can be explictly bound
data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool
-- The compiler will warn the type variable on LHS is not used on RHS.
data instance forall (a :: k). F a = FOtherwise
```

Although, a data family is introduced with the keyword “data”, a data family instance can use either data or newtype. For example:

```haskell
    data family T a
    data    instance T Int  = T1 Int | T2 Bool
    newtype instance T Char = TC Bool -- does it make any difference?
```

A data instance can use GADT syntax for the data constructors, and indeed can define a GADT. For example:

```haskell
    data family G a b
    data instance G [a] b where
       G1 :: c -> G [Int] b
       G2 :: G [a] Bool
```

You can use a deriving clause on a data instance or newtype instance declaration.

The following is **not** possible:

```haskell
    data family T a
    data instance T Int  = A
    data instance T Char = B
    foo :: T a -> Int
    foo A = 1
    foo B = 2
```

Instead, you would have to write foo as a class operation, thus:

```haskell
class Foo a where
  foo :: T a -> Int
instance Foo Int where
  foo A = 1
instance Foo Char where
  foo B = 2
```

# Synonym families

Type families appear in three flavours:

* Open Families: toplevel
* Closed Families: toplevel
* Associated Type Synonyms: inside a class (better compiler warnings)

## Open Type Synonym Families

```haskell
-- Open type synonym family
type family Elem (c :: Type) :: Type
type family Elem c -- same

-- Type instance declaration
type instance Elem [e] = e
type instance Elem _ = Int -- Type arguments can be replaced with the underscore if the names dof the arguments don't matter.
```

We call the __number of parameters in a type family declaration, the family’s arity__, and all applications of a type family must be __fully saturated__ with respect to that arity.

__The right-hand side of a type instance must be a monotype (i.e., it may not include foralls)__ and after the expansion of all saturated vanilla type synonyms, no synonyms, except family synonyms may remain.

## Closed Type Synonym Families

```haskell
type family F a where
  F Int  = Double
  F Bool = Char
  F a    = String
```

A closed type family’s equations are tried in order, from top to bottom.

When ExplicitForAll is enabled, type or kind variables used on the left hand side of an equation can be explicitly bound:

```haskell
type family R a where
  forall t a. R (t a) = [a]
  forall a.   R a     = a
```

A closed type family may be declared with no equations: will never reduce and are not necessarily injective(one image per codomain element), and cannot be given any instances.

### Type family examples

```haskell
type family F a :: Type
type instance F [Int]   = Int               -- OK!
type instance F String  = Char              -- OK!
type instance F (F a)   = a                 -- WRONG: type parameter mentions a type family
type instance F (forall a. (a, b))  = b     -- WRONG: a forall type appears in a type parameter
type instance F Float = forall a.a          -- WRONG: right-hand side may not be a forall type (only for closed)
type family H a where                       -- OK!
  H Int  = Int
  H Bool = Bool
  H a    = String
type instance H Char = Char                 -- WRONG: cannot have instances of closed family
type family G a b :: Type -> Type
type instance G Int = (,)                   -- WRONG: must be two type parameters
type instance G Int a = (,) a               -- OK
type instance G Int Char Float = Double     -- WRONG: must be two type parameters
type instance G Int Char = Maybe            -- OK
```

### Compatibility and apartness of type family equations

1. The patterns of two distinct type family instances cannot overlap.

```haskell
type instance F Int = Bool
type instance F Int = Char
```

2. Two overlapping type family instances are allowed if the right-hand sides coincide in the region of overlap

```haskell
type instance F (a, Int) = [a]
type instance F (Int, b) = [b]   -- overlap permitted a ~ Int, b ~ Int, [a] ~ [b]

type instance G (a, Int)  = [a]
type instance G (Char, a) = [a]  -- ILLEGAL overlap, a1 ~ Char, a2 ~ Int, [Char] /= [Int]
```

For a polykinded type family, this is accepted:

```haskell
type family J a :: k
type instance J Int = Bool  -- Kind: Type
type instance J Int = Maybe -- Kind: Type -> Type
```

### Decidability of type synonym instances

You can use `UndecidableInstances` in combination with TypeFamilies.

More on https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeFamilies

### Wildcards on the LHS of data and type family instances

This is ok:

```haskell
data family F a b :: Type
data instance F Int _ = Int
```

## Associated data and type families

A data or type synonym family can be declared as part of a type class, thus:

```haskell
class GMapKey k where
  data family GMap k :: Type -> Type
  ...

class Collects ce where
  type family Elem ce :: Type
  ...
```

We (optionally) __may drop the “family” keyword__.

The parameters of the type must all be type variables, of course, and some (but not necessarily all) of then can be the class parameters:

```haskell
class C a b c where
  type T c a x :: Type
```

### Associated instances

```haskell
instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
  ...

instance Eq (Elem [e]) => Collects [e] where
  type instance Elem [e] = e
  ...
```

We (optionally) __may drop the instance keyword in the family instance__.

The associated instance type must be the same as the type given in the class instance head:

```haskell
class Collects ce where
  type Elem ce :: Type

instance Eq (Elem [e]) => Collects [e] where
  -- Choose one of the following alternatives:
  type Elem [e] = e       -- OK
  type Elem [x] = x       -- BAD; '[x]' is different to '[e]' from head
  type Elem x   = x       -- BAD; 'x' is different to '[e]'
  type Elem [Maybe x] = x -- BAD: '[Maybe x]' is different to '[e]'
```


### Associated type synonym defaults

This is ok:

```haskell
class IsBoolMap v where
  type Key v
  type instance Key v = Int

  lookupKey :: Key v -> v -> Maybe Bool

instance IsBoolMap [(Int, Bool)] where
  lookupKey = lookup
```

### Scoping of class parameters

```haskell
class C a b where
  data T a

instance C [c] d where
  data T [c] = MkT (c, d) -- WRONG!!  'd' is not in scope
```

PolyKinds is ok:

```haskell
class C k where
  type T :: k

instance C (Maybe a) where
  type T = (Nothing :: Maybe a)
```


###  Import and export

- The form T(..), where T is a data family, names the family T and all the in-scope constructors (whether in scope qualified or unqualified) that are data instances of T.

- The form C(..), where C is a class, names the class C and all its methods and associated types.

Family instances are implicitly exported, just like class instances. However, this applies only to the heads of instances, not to the data constructors an instance defines.

More on: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#import-and-export

### Type families and instance declarations

- Data type families may appear in an instance head:

```haskell
-- OK
data instance T Int = T1 Int | T2 Bool
instance Eq (T Int) where
  (T1 i) == (T1 j) = i==j
  (T2 i) == (T2 j) = i==j
  _      == _      = False
```

- Type synonym families may not appear (at all) in an instance head:

```haskell
-- WRONG
type family F a
type instance F Bool = Int

class C a

instance C Int
instance C (F a) -- Now a constraint (C (F Bool)) would match both instances.
```


# Example of Associated Data Families

```haskell
{-# LANGUAGE TypeFamilies #-}
module GenericMap (
  -- ^ Data type and instances
    GMapKey(..)
  , GMap(..)
  -- ^ Test it
  , main
  ) where

import           Data.Kind
import           Data.Char           (ord)
import qualified Data.Map            as Map
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IntMap -- Fast & furious
import           Prelude             hiding (lookup)

-- Let's make Map faster!

class GMapKey k where
  data GMap k v :: Type
  empty         :: GMap k v
  lookup        :: k -> GMap k v -> Maybe v
  insert        :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v = GMapInt (IntMap v)
  empty                  = GMapInt IntMap.empty
  lookup k (GMapInt m)   = IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (IntMap.insert k v m)

instance GMapKey Char where
  data GMap Char v = GMapChar (GMap Int v)
  empty                   = GMapChar empty
  lookup k (GMapChar m)   = lookup (ord k) m
  insert k v (GMapChar m) = GMapChar (insert (ord k) v m)

instance GMapKey () where
  data GMap () v = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup () (GMapUnit v)   = v
  insert () v (GMapUnit _) = GMapUnit $ Just v

instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v = GMapPair (GMap a (GMap b v))
  empty                         = GMapPair empty
  lookup (a, b) (GMapPair gm)   = lookup a gm >>= lookup b
  insert (a, b) v (GMapPair gm) = GMapPair $ case lookup a gm of
    Nothing  -> insert a (insert b v empty) gm
    Just gm2 -> insert a (insert b v gm2  ) gm

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
  empty                                   = GMapEither empty empty
  lookup (Left  a) (GMapEither gm1  _gm2) = lookup a gm1
  lookup (Right b) (GMapEither _gm1 gm2 ) = lookup b gm2
  insert (Left  a) v (GMapEither gm1 gm2) = GMapEither (insert a v gm1) gm2
  insert (Right a) v (GMapEither gm1 gm2) = GMapEither gm1 (insert a v gm2)

--------------------------------------------------------

myGMap :: GMap (Int, Either Char ()) String
myGMap =
   insert (5, Left 'c') "(5, Left 'c')"    $
   insert (4, Right ()) "(4, Right ())"    $
   insert (5, Right ()) "This is the one!" $
   insert (5, Right ()) "This is the two!" $
   insert (6, Right ()) "(6, Right ())"    $
   insert (5, Left 'a') "(5, Left 'a')"    $
   empty

main :: IO ()
main = do
  putStrLn $
    maybe "Couldn't find key!" id
      $ lookup (5, Right ()) myGMap
```

# Example of Associated Type Families

Functional dependencies:

```haskell
class Collects e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  toList :: ce -> [e]

instance Eq e => Collects e [e] where
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs)
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l

sumCollects :: (Collects e c1, Collects e c2) => c1 -> c2 -> c2
sumCollects c1 c2 = foldr insert c2 (toList c1)
```

Associated type synonyms:

```haskell
class Collects ce where
  type Elem ce
  empty  :: ce
  insert :: Elem ce -> ce -> ce
  member :: Elem ce -> ce -> Bool
  toList :: ce -> [Elem ce]

instance Eq e => Collects [e] where
  type Elem [e]   = e
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs)
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l

sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2
sumCollects c1 c2 = foldr insert c2 (toList c1)
```
