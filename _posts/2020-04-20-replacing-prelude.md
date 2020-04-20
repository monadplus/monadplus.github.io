---
title: Replacing Prelude
description: Replacing Prelude from `base` by a custom one. This post explains several ways to do so.
categories:
 - haskell
tags:
 - haskell
 - prelude
---

# Replacing `Prelude`

`base` has some strenghts and weaknesses\*. As a library author you are usually forced to work with `base`. However, as an application developer you are free to use an alternative `Prelude`. Below, several ways to do so:

## Explicit way

On each module:

```
{-# LANGUAGE NoImplicitPrelude #-}

import CustomPrelude
```

## Implicit way

.cabal

```
library:
  build-depends: base-noprelude ^=> a.b.c.d
  other-modules: Prelude
```

Prelude.hs

```
module Prelude(module CustomPrelude) where
import CustomPrelude
```

## Cabal way

Requires `cabal` >= 2.4.0.

```
library:
  ...
  mixins:    base hiding (Prelude)
           , custom-prelude (CustomPrelude as Prelude)
```

\* Explained here: https://hackage.haskell.org/package/relude
