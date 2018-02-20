# Algebraic Data Types [5.2]

## Monoid in respect to Product

A **Monoid** would be something that has **multiplication**, that is **associative** and has a **unit**. But now we are talking about types - is there something like an algebra for types? Is the **Product** in types behaving like **multiplication**?

It's not true of all **Monoids**, but the **Product** of numbers is usually symmetric.

If we were to take two pairs, `(a, b)` and `(b, a)`, these are not the same pair. If a function were to take `(Int, Bool)` as a parameter, it would not type check correctly if the parameter were to be `(Bool, Int)`.

However, they contain the same information, it's just encoded slightly differently. They are actually **isomorphic**, and the **isomorphism** between these two types is called **swap**:

```haskell
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p)
```

So it is not really symmetric, but it _is_ symmetric _up to_ **isomorphism**. A **Monoid** doesn't have to be symmetric it's just an interesting thing.

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

Where the unit `()` is represented as `1`, and `=` meaning `up to isomorphism`. Similarly we could say the same for the standard **Monoid**:

- `((a, b), c) ~ (a, (b, c))`
- `(a * b) * c = a * (b * c)`

The fact that it's **associative** _up to isomorphism_ is very important as it means we can remove some of the parenthesis and say:

- `(a, b, c)`

## Monoid in respect to Coproduct

Similarly the **Coproduct** (or **sum type**) is symmetric _up to isomorphism_:

```haskell
Either a b ~ Either b a
```

`Either a b` is **isomorphic** to `Either b a`. The **isomorphism** would pattern match on `Left` and `Right` and return `Right` and `Left`.

Similarly it's also **associative** _up to isomorphism_. If you wanted to associate a bunch of `Either` you could write a data structure such as:

```haskell
data Triple a b c = Left a | Right c | Middle b
```

You can do things like this because the **sum type** is **associative**, as _up to isomorphism_ it doesn't matter how things are ordered.

Now does the **sum type** also have a **Unit**/**Identity**? The answer is **Yes**:

```haskell
Either a Void ~ a
```

`Either a Void` is **isomorphic** to `a`. Because there are no elements of `Void`, you can never construct a `Right`. So effectively `Either a Void` give us the right identity and similarly `Either Void a` gives us the left identity.

If `Either` is like a `+` then `Void` is like `0`:

- `Either a Void ~ a`
- `a + 0 = a`

So the **sum type** is also a **Monoid**.

## Combining the two

So we have these two **Monoids**, with the **Product** (or **Pair**) representing the multiplication `*` in our algebra, and the **Coproduct** (or **sum type**) representing the addition `+`.

```haskell
-- Product
(a, ()) ~ a
a * 1 = a

-- Coproduct
Either a Void ~ a
a + 0 = a
```

So if we have `a + 0 = a`, in algebra `a * 0 = 0` is also true, as anything multiplied by 0 equals 0. Is this true of our example? If `0` is `Void` and `*` represents a **Product** (or **pair**):

```haskell
(a, Void) ~ Void
```

Since we can never call this function (as we can never generate a `Void`), the function is the equivalent of `Void`. Therefore this isomorphism is true.

Another thing we know from algebra is the **Distributive Law**:

```
a * (b + c) = a * b + a * c
```
