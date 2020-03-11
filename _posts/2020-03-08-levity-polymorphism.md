---
title: Levity polymorphism
description: How you ever read the signature of ($) ? Why is it so strange ? In this blog we are going to briefly explore levity-polymorphism and primitive types
categories:
 - haskell
 - ghc
tags:
 - haskell
 - ghc
 - levity polymorphism
 - primitive types
 - compilers
---

GHC is built on top of "primitive" types and operations that are not openly exposed to the average public. They are called "primitive" because they cannot be defined using only the Haskell specification. They are particular to GHC [1].

In GHC, there exist the following kind of types:

- Regular types: boxed and lifted.
- Primitive types: unboxed and unlifted (some of them need a pointer such as Array#).

And the kind system differentiate them this way [2]:

```haskell
TYPE :: RuntimeRep -> Type   -- highly magical, built into GHC

data RuntimeRep = LiftedRep     -- for things like `Int`
                | UnliftedRep   -- for things like `Array#`
                | IntRep        -- for `Int#`
                | TupleRep [RuntimeRep]  -- unboxed tuples, indexed by the representations of the elements
                | SumRep [RuntimeRep]    -- unboxed sums, indexed by the representations of the disjuncts
                | ...

type Type = TYPE LiftedRep    -- Type is just an ordinary type synonym
```

This is called _levity polymorphism_.

# Restrictions

```haskell
bad :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
              (a :: TYPE r1) (b :: TYPE r2).
       (a -> b) -> a -> b
bad f x = f x
```

This construction is incorrect because we don't know the size of bits of the given `x` because x's type is `TYPE r1` (levity-polymorphic) and `r1` is polymorphic so we can't know the size of the register to use for `x`.

GHC forbid such constructions via the rule:

> No variable may have a levity-polymorphic type

But we can still do things like:

```haskell
($) :: forall r (a :: Type) (b :: TYPE r). (a -> b) -> a -> b
f $ x = f x
```

Where `b` is levity-polymorphic but r is not. `($)` is the most general implementation possible.

And we can also encode `error` and `undefined`:

```haskell
undefined :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a
undefined = error "..."

error :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => String -> a
error = ... -- magic
```

# References

- [1] GHC's official documentation, Unboxed types and primitive operation, <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations>
- [2] GHC's official documentation, Levity-polymorphism, <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#levity-polymorphism>

