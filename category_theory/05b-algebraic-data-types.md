# Algebraic Data Types [5.2]

A **Monoid** would be something that has **multiplication**, that is **associative** and has a **unit**.

If we were to take two pairs, `(a, b)` and `(b, a)`, these are not the same pair. If a function were to take `(Int, Bool)` as a parameter, it would not type check correctly if the parameter were to be `(Bool, Int)`.

However, they contain the same information, it's just encoded slightly differently. They are actually **isomorphic**, and the isomorphism between these two types is called **swap**:

```haskell
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)
```

So it is not really symmetric, but it _is_ symmetric up to **isomorphism**. It's not necessary to show that this is a **Monoid** as a **Monoid** doesn't have to be symmetric it's just an interesting thing.
