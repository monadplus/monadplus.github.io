---
title: Conflicts in parsing
description: What's a parsing conflict ? Let's explore some examples ...
categories:
 - compilers
tags:
 - parsing
---

# What's a parsing conflict ?

In a _parsing table_ (LL, LR, LALR), a **conflict** means that there are several items in a cell[1].

In LR and LALR, two kinds of conflicts may occur:

- **shift-reduce conflict** between shift and reduce actions.
- **reduce-reduce conflict** between two (or more) reduce actions.

The second type is more harmful, but easier to fix. Let's see some examples:

## Reduce-reduce conflict examples

- Any `Lit` can be reduced using both rules:

```
EVar.  Exp ::= Lit ;
ECons. Exp ::= Lit ;
```

The solution is to remove one rule and let the type-checker desambiguate between `EVar` and `ECons`.

- Another example are **implicit ambiguities**:

```
SExp.  Stm ::= Exp ;
SDecl. Stm ::= Decl ;
DTyp.  Decl ::= Typ ;
EId.   Exp ::= Lit ;
TId.   Typ ::= Lit ;
```

Notice that a `Stm` can be reduce using one of the following reduction-path:

```
Stm -> Exp -> Lit

Stm -> Decl -> Typ -> Lit
```

The solution is to redesign your language, for example, `DTyp` should only be valid in a function parameter lists, and not as statements.

## Shift-reduce conflict examples

- The **dangling else**:

```
SIf.     Stm ::= "if" "(" Exp ")" Stm ;
SIfElse. Stm ::= "if" "(" Exp ")" Stm "else" Stm ;
```

The problem arises when _if statements_ are nested:

```c
if (x > 0) if (y < 8) return y; else return x;
```

There are two possible actions:

```c
// shift
if (x > 0) { if (y < 8) return y; else return x; }

// reduce
if (x > 0) { if (y < 8) return y; } else return x;
```

The solution is to pick one action arbitrary (shift).

# Bibliography

- [1] A. Ranta, _"Implementing Programming Languages"_, College Publications.
