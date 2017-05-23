# Category Theory

## Functions and Epimorphisms
Functions are defined in mathematics as special kinds of relations. A relation is just a sub-set of pairs of elements.

If we were to talk about a set of pairs from Set theory, a set of pairs is called a **Cartesian product**. If we were to have two sets, `s1` and `s2`, and were to do a Cartesian product of the two (so all elements from `s1` paired with all possible elements from `s2`, the set of all pairs forms a Cartesian product), if we were to pick a sub-set of this Cartesian product, this would by definition be a **relation**.

So in this sense, relation does not have directionality. Functions however have directions (think arrows). An element from `s1` can have **relations** with many elements from `s2`, or vice versa.

```
s1 a ------------> s2 a
s1 b ------------> s2 a
s1 c ------------> s2 a

s1 a ------------> s2 a
s1 a ------------> s2 b
s1 a ------------> s2 c
```

However with functions, one argument for a function cannot be mapped to a bunch of elements. It's still ok for multiple arguments to be mapped to the same value for a function. Functions are not symmetrical in this way. There is **directionality**, which is a very important intuition about functions.

```
FUNCTIONS CAN DO:
s1 a ------------> s2 a
s1 b ------------> s2 a
s1 c ------------> s2 a

BUT CANNOT DO:
s1 a ------------> s2 a
s1 a ------------> s2 b
s1 a ------------> s2 c
```

All elements of `s1` must be mapped into something in `s2`. However, it's not true that all elements from `s2` have to have mappings from `s1`. In this sense, it could be a subset of `s2` that is being mapped.

If we were to use the correct names, if we were to have a function `f` where the above is a graph of `f`:

```
s1 ------------> s2
        f
```

`s1` would be called the **domain**. The **domain** of the function is the whole set from which we are starting.

`s2` would be called a **codomain**.

The subset of `s2` that was obtained from mapping `s1` is called the **image**.

If there is a function `f` that goes from `a` to `b`, there isnt' always a function that does the other way around (the inverse of the function).

```haskell
------------>
f :: a -> b

Note: Haskell Notation
```

This function is invertible if there is a function that goes from `b` to `a`. This is known as an **inverse function**.

```haskell
------------>
g :: b -> a
```

This would be the equivalent of saying `g ∘ f = id` and `f ∘ g = id`. A function that is invertible is automatically symmetrical, and is called **isomorphism**.

To define this in category theory, as opposed the just talking about sets (as a category can contain any objects, not just sets), you could look at this as:

```
a ------------> a
      id a

a ------------> b
        f

b ------------> a
        g

b ------------> b
      id b
```

We could then say `g ∘ f = id a` and `f ∘ g = id b` in categorical language, which is a definition of **isomorphism** in any category, not just sets.

Most functions that we deal with are not **isomorphisms**. One reason is because a function can collapse elements of a set. For example:

```
x1 ------------> y
x2 ------------> y
```

This could be something like `is even`, which might be a function that maps every `even` number into the boolean that is `true` and every `odd` number to a boolean that is `false`.

The other reason for a function not to be invertible is that it's **image** does not fill the whole **codomain**. For example there may be some functions outside of the **image** that don't correspond to anything in the **domain**, and therefore can't be inverted.

If a function does _not_ collapse things, this is called **injection**. This is where all the points in one set get 'transported' to this other set. For example:

```
x1 ------------> fx1
x2 ------------> fx2
```

If we were to look at the opposite of this, where if the image is equal to the whole codomain, then it's called **surjective**. If you have a function that is both **injective** and **surjective** at the same time, it's an **isomorphism**.

**injective** and **surjective** comes from Latin, but in category theory, this is often referred to by the Greek terminology. So instead of **sur**, we would refer to it as **epi**, so something **surjective** would be **epic**. Likewise, **injective** would be **monic**.

An **injective** morphism is also known as a **monomorphism**, and a **surjective** morphism is also known as an **epimorphism**.

If we were to think of a category with types and functions.
