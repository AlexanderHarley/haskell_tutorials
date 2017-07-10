# Kleisli Category [3.2]

We haven't looked at many examples of **partial orders** or **preorders** yet, so lets define a relation on sets. This will be a relation on inclusion - what it means to be a subset of another set. But what kind of relation is it?

Every Set is a subset of itself, which gives us identity (or in terms of _order_, we'd call it **reflexivity**, meaning an object is in relation with itself).

```
a ⊆ a
```

The other thing we need to check for in an order is composition:

If `a` is a subset of `b`, and `b` is a subset of `c`, then automatically `a` must be a subset of `c`. So we have this composition and associativity that we need in a category.

So it's definitely a preorder:

```
        ≤                ≤
a -------------> b -------------> c
```

But is it a partial order?

### a ⊆ b ∧

_Note: `∧` means `and`_
