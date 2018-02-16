# Algebraic Data Types [5.2]

A **Monoid** would be something that has **multiplication**, that is **associative** and has a **unit**. But now we are talking about types - is there something like an algebra for types? Is the **Product** in types behaving like **multiplication**?

It's not true of all **Monoids**, but the **Product** of numbers is usually symmetric.

If we were to take two pairs, `(a, b)` and `(b, a)`, these are not the same pair. If a function were to take `(Int, Bool)` as a parameter, it would not type check correctly if the parameter were to be `(Bool, Int)`.

However, they contain the same information, it's just encoded slightly differently. They are actually **isomorphic**, and the **isomorphism** between these two types is called **swap**:

```haskell
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)
```

So it is not really symmetric, but it _is_ symmetric up to **isomorphism**. A **Monoid** doesn't have to be symmetric it's just an interesting thing.

The next part is that a **Monoid Product** should be **associative**, so is this example associative?

```haskell
((a, b), c) ~ (a, (b, c))
```

The answer would be **no**, as if we have a function expecting the type `((a, b), c)`, we could call it with a function of type `(a, (b, c))`. But again, these two types contain the same information that is rearranged. And this rearrangement can be written as an **isomorphism**. For example:

```haskell
assoc ((a, b), c) = (a (b, c))
```

**swap** is **isomorphic**, it has an inverse (which is **swap**). **assoc** is **isomorphic** too, although it's inverse morphism is not itself. So this "multiplication of types" is **associative**.

There's one more thing - does it have a **unit**/**identity** of multiplication? What would be the type that if paired with any other type would return the same type?

```haskell
(a, ()) ~ a
```

Again, this is not the same as `a`, but it is **isomorphic** to `a`:

```haskell
munit (x, ()) = x
munit_inverse x = (x, ())
```

Or alternatively:

```haskell
munit = fst
```

This shows the right identity ("multiplication on the right"), but we could also similarly prove the left identity as we know it is symmetric up to an **isomorphism** - a **Module Isomorphism**. This is a **Monoid**.

With the idea of multiplication, we could say that these two are similarly equivalent:

- `(a, ()) ~ a`
- `a * 1 = a`

Where the unit `()` is represented as `1`.
