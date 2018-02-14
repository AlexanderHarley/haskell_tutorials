# Products [4.2]

## Terminal Objects Continued

Continuing with **Terminal Objects**, we talked before about _incoming_ arrows to the **Terminal Object**, but we didn't talk about _outgoing_ arrows.

So the _incoming_ arrows to the **Terminal Object** have to be unique, but there can be outgoing arrows (and there usually are). These _outgoing_ arrows help us define generalised elements in other objects. Every arrow from a **Terminal Object** to another object is a definition of a generalised element in the other object. Because we are talking about category theory we can't see the elements, but this is as close as we can come to a _generlised_ element.

## Reversing Categories

When we were talking about Terminal Objects, we mentioned that the Initial Object is the same but with the arrows reversed.

It turns out that this trick of reversing arrows has a much deeper meaning:

**Every construction in category theory has it's opposite construction that's done by reversing it's arrows.**

So if we have the definition of a Terminal Object, we get the definition of an Initial Object for free by repeating the same construction but with inverting the arrows.

For any category you can always create another category that is the same as the original category, but with the arrows reversed.

So if we have category `C` with a morphism between `a` and `b`, there is the opposite category `C op` (normally called _C opp_) which has the same objects, but with the arrows reversed:

```
C
        f
a ------------> b


C op
       f op
b ------------> a
```

Lets make the category a little bigger:

```
C
        f              g
a ------------> b ------------> c

              g ∘ f
a ----------------------------> c


C op
       g op           f op
c ------------> b ------------> a

            (g ∘ f)op
c ----------------------------> a
```

### (g ∘ f)op = f op ∘ g op

## Cartesian Products

Looking at Sets (let's say Set `a` and Set `b`), when you take an element from one set and an element from another set you can form a pair, for example: `(e1, e2)`. The Set of all these pairs is called the **Cartesian Product**.

The **Cartesian Product** has some properties that can be expressed in terms of arrows. For every **Cartesian Product** of two sets, there are these two special functions called **projections**. In Haskell, these are `fst` and `snd`.

```
       fst
c ------------> a

       snd
c ------------> b
```

Since we are talking about categories, we can't look into each object to see which one is our **Cartesian Product**. This is where **Universal Construction** comes in.

Let's say we have two objects, `c` and `c'` (c prime), with projections `p`, `q`, `p'` and `q'`:

```
         p
c  ------------> a

         q
c  ------------> b

         p'
c' ------------> a

         q'
c' ------------> b
```

We are saying that `c` is better than `c'` if there is a morphism `m` from `c'` to `c`. This is similar to what we did with ranking for the **Terminal** and **Initial** objects. Similarly, we impose the condition that this arrow `m` is unique.

However, this is still not enough as we haven't talked about these **projections**. We need to not only say that `c` is better because of this morphism, but that the **projections** are also better.

The conditions for the projections are:

### p ∘ m = p'
### q ∘ m = q'

So `c` is better than `c'` if there is a unique morphism `m`, such that `p ∘ m = p'` and `q ∘ m = q'`. This morphism `m` is special as it factorises the two projections.

Let's look at this from a programming example:

If we were to take the type of a pair, in Haskell this would be `(a, b)`. We then have these two functions:

```haskell
fst (a, _) = a
snd (_, b) = b
```

This is the real product - the _good_ product. But what if we were to have some _bad_ products. Let's look at an example:

If `a = Int` and `b = Bool`, we might have a really bad candidate which is just `Int`. This doesn't look like a product of `Int` and `Bool` at all. The product of `Int` and `Bool` should look like `(Int, Bool)`.

But the `Int` has projections too. For example, there is:

```haskell
p' :: Int -> Int
p' = id
```

and also:

```haskell
q' :: Int -> Bool
q' = True
```

On the surface, this looks like a good candidate for our **Cartesian Product**, as it meets the rules of our pattern in our **Universal Construction**. So how do we say that `(Int, Bool)` is better than `Int`.

```haskell
m :: Int -> (Int, Bool)
m x = (x, True)
```

This morphism is _bad_ in that it shrinks stuff. This is how we know that the `q'` is bad - there is **non-surjective** badness that is distilled in this morphism from `q'`. Meaning that it completely misses pairs in the form of `(x, False)`.

Let's try a richer candidate: `(Int, Int, Bool)`.

```haskell
p' :: (Int, Int, Bool) -> Int
p' (x, _, _) = x

q' :: (Int, Int, Bool) -> Bool
q' (_, _, b) = b

m :: (Int, Int, Bool) -> (Int, Bool)
m (x, y, b) = (x, b)
```

So this is a good projection, no information is lost here. It takes an `Int` and returns an `Int`, and it takes `Bool` and returns a `Bool`. However, this is too big to be a product. It has redundant information in the form of the middle `Int`. So this candidate isn't any good either as it is **non-injective** - it is shrinking the information.

All these candidates have some flaw, whether it be that they are **non-surjective**, **non-injective** etc. (except from our _good_ candidate), and these flaws can always be distilled into `m` morphism.

## Categorical Product

Now we can define the product using arrows and objects - we don't need to talk about Sets. We call this a **Categorical Product**.

A **Categorical Product** of two objects (`a` and `b`), with a third object `c` that has two projections (`p` and `q`):

```haskell
p :: c -> a
q :: c -> b
```

This has the universal property, meaning for any other object with two projections, there is a unique morphism `m` that goes from `c'` to `c`:

```haskell
p' :: c' -> a
q' :: c' -> b
```

```haskell
m  :: c' -> c
p' :: p ∘ m
q' :: q ∘ m
```

Not every category has products, and even if it has maybe it doesn't have it for every pair of objects. But if we want to imitate **Set category**, we want to have categories that have **Products**, **Terminal Objects**, **Initial Objects**, and also **Coproducts** (which are the opposite of **Products** to be looked at later).
