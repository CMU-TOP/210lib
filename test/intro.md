Introduction by Favonia
=======================

The goal of testing here is to show some possibly more efficient sequence
library matches a simple list implementation.  If this is true, then by
parametricity we can prove that no client code can tell the difference.
This is made possible by the SML types and module systems. [1]

More precisely, we want to show a mathematical correspondence between two
implementations.  This boils down to a relation for each pair of
corresponding types (and type constructors).  In this case the most
interesting pair is `Seq.seq` and `list`.  We say a sequence
`S` and a list `L` are related iff their "contents" are the same.
Values of other base types (`int`, `string`, ...) are related iff
they are equal.  The critical part is to show related inputs lead to related
outputs.  Once you have proved this, parametricity is at your hand.

How exactly the "contents" are defined depends on the actual code in the
sequence library.  A proper mathematical relation might requires some
manual inspection of the code.  However, here we want a testing system with
no human interference.  Therefore, we assume `%` and `equal` are
properly implemented, and use this relation instead:

> S and L are related iff `equal R (S, % L)` is `true`

where `R` is a (correct) equality testing function for the element type.
For example, let's take a look at `nth`.  It has the type

> `val nth : 'a seq -> int -> 'a`.

One can write a simple `nth'` for lists of type

> `val nth' : 'a list -> int -> 'a`.

We want to show that related inputs give the related outputs.  For instance,
`nth <1,2,3> 2` should be related to `nth [1,2,3] 2`.  Since the
return type `int` is a base type, "being related" is simply "being
equal".  This means they return the same integer, which should be `3`.

In addition to the assumption that `%` and `equal` are correct,
four compromises are made:

1. We only test the functions against random inputs.  It is undecidable
   whether two functions are equal.

2. In the case that there are extra preconditions on the inputs,
   we will only look at the inputs satisfying the preconditions.
   Bad clients (badly written student code, for example) can possibly
   distinguish different implementations by breaking the preconditions.

3. `splitMid` intentionally allows implementations to break
   the observational equivalence; related inputs in different
   implementations can lead to different outputs.  In other words,
   our goal is actually impossible if `splitMid` is taken into
   consideration.  A weaker "self-consistency" condition is used instead.

4. Additional assumptions:
   * `merge` is "stable".
   * `argmax` always returns the first one.

One final note is that in many cases we only test specific element types
such as `int` or `string` or only specially chosen inputs.
This is made possible by parametricity and the assumption that no (visible)
effects (including non-termination) are involved.  The theory would be more
complicated with effects.

[1] The parametricity for the full SML, as far as I know, has not been
    formally proved yet.  Only the parametricity for various sublanguages
    is done.  That said, people believe parametricity holds for SML.
