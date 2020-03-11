---
title: Tail Recursion
description: What is tail recursion ? Do I need to worry about it in Haskell ? How can I prevent a stack overflow ?
categories:
 - haskell
 - nix
 - tooling
tags:
 - haskell
 - ghc
 - nix
 - tooling
---

# Recursion, Tail Recursion and TCO

Def. A __recursive function__ is __tail recursive__ if the final result of the recursive call is the final result of the function itself

`foldl` is tail recursive:

```haskell
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
```

Calling a function uses stack space, so a recursive function may blow the stack space. Since in a __tail call__, the containing function is about to return, its environment can actually be discarded and the recursive call can be entered without creating a new stack frame. This trick is called __tail call elimination__ or tail call optimisation and allows tail-recursive functions to recur __indefinitely__.

The important concept to know in Haskell is __guarded recursion__ where any recursive calls occur within a data constructor. This allows the result of the function to be __consumed lazily__, since it can be evaluated up to the data constructor and the recursive call __delayed__ until needed.

`foldr` is guarded recursive: because the recursive call happens inside the :

```haskell
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

IMO, `foldr` is not a good example of guarded recursive:

```
>>> foldr (+) 0 [1..100000000]
*** Exception: stack overflow
```

Let's use a better example:

```
repeat :: a -> [a]
repeat x = let xs = x : xs in xs
```

Since the (:) constructor is __non-strict in its second argument__ this works and the list can be traversed, because you have a __finite weak-head normal form__ (WHNF). As long as the consumer (for example a list fold) only ever asks for the WHNF this works and runs in constant space.

# In Haskell

In Haskell, the function call model is a little different, function calls might __not__ use a _new stack frame_, so making a function tail-recursive typically isn't as big a deal—being productive, via guarded recursion, is more usually a concern.

# Sources

- "Tail Recursion", https://wiki.haskell.org/Tail_recursion
