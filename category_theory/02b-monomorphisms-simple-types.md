# Monomorphisms and Simple Types [2.2]

## Monomorphisms

![monomorphism-02](https://user-images.githubusercontent.com/13851085/27336910-df1a518a-55c8-11e7-9fad-5ac4c1c38047.jpg)

An example of something that **isn't** a **monomorphism** (also called a **non-injective** function) would be something like `f`. A **non-injective** function will just map two different elements (`x1`, `x2`) of one set (`a`) into the same element of another set (`b`). `f` is **not** a **monomorphism**.

If we were to look at what **is** a **monomorphism**:

If you compose `g1` with `f`, or `g2` with `f`, you will get the same result. This is a **monomorphism**.

_Note: This is very similar to Epimorphisms, but rather than looking at post-composition, we look at pre-composition._

<div style="font-size: 22px; margin-bottom: 15px;">
f ∘ g<sup>1</sup> = f ∘ g<sup>2</sup>
</div>

This is because they only differ on the two points: `x1` and `x2`. They are equal on everything else but they differ on the way they map `z`. You will get the same result as both `x1` and `x2` are mapped into the same `y`.

<div style="font-size: 22px; margin-bottom: 15px;">
∀c . ∀g1 , g2 :: c -> a <br/>
f ∘ g<sup>1</sup> = f ∘ g<sup>2</sup> ⇒ g<sup>1</sup> = g<sup>2</sup>
</div>

> Epimorphisms and Monomorphisms are defined for every category.

_Note: However you can get categories that don't contain any Monomorphisms or Epimorphisms._

## Simple Types

Lets look at Sets, as they are models for types.

### Empty Set

The simplest Set in Set Theory is the **Empty Set**. While the Empty Set doesn't correspond to a type in imperative programming languages, you will find it in `Haskell` (well, kind of, it technically has the "bottom" element).

In Haskell, Empty Set corresponds to the type called **void**. Void has no constructor, so there is no way of constructing it.

So if you were to have a function of type:
```haskell
f :: Void -> Int
```

This function can exist, but can never be called, as **void** can never be supplied as an argument.

Every object in our category has to have an identity. So an identity for void has to be:

```haskell
id^Void :: Void -> Void
```

This function exists, but in vacuum. It exists, but can't be called.

This function has a name in Haskell. It's called the `absurd` function.

```haskell
absurd :: Void -> a
```

In logic, **Void** corresponds to **False**. You cannot construct falsity from something. You cannot prove something that is false.

<div style="font-size: 22px; margin-bottom: 15px;">
Void &harr; False
</div>

However, if you assume False, then from False follows anything. That's why absurd exists. `Void -> a`, anything can follow, it is a polymorphic function.

### Singleton Set

Next is a one element set, a **Singleton Set**. In Haskell it's called `Unit`, but the notation for it is just an empty tuple, `()`. It has just one element, and this element can be constructed from anything.

The element of the Singleton Unit type is also a tuple:

```haskell
() :: ()
```

From the point of logic again, **()** corresponds to **True**. You can always prove that it's True.

```haskell
unit :: a -> ()
```

This function can take something out of thin air, disregard it's type and create a tuple. This function can always be called.

_Note: These are being defined talking about Sets, but will later be defined in categorical terms._

If we were to look at some other functions that use Unit:

```haskell
one :: () -> Int
two :: () -> Int
true :: () -> True
false :: () -> False
```

There are many functions like this. We have as many functions as we have for elements in a Set. This gives us a sneaky way of defining what an element of a Set is without talking about elements, as we are just talking about functions (which are Morphisms).

So we can say, in Set we have this family of morphisms from Unit (if we can define Unit in terms of a category), so a family of functions from Unit corresponds to picking elements of Sets.

Essentially this is a morphism from any Singleton Set to any other Set (or Type, Object etc.).

_Note: An object like () does not always exist in every category_
