# Category Theory - Basics

## What is a category?
A category can be thought of as a `bunch` of objects. It is also sometimes referred to as a `class` of objects.

It also contains `morphisms` (also referred to as `arrows`) that goes between two objects.

- **Abstraction**
- **Composition**
- **Identity**

### Abstraction

The objects are primitives with no properties and no internal structure. The morphisms are also primitives with no properties, other than that they have a beginning and an end. The reason for having objects is to mark the ends of morpshisms (or arrows).

```
a -----> b
    f
```

There can be multiple (or infinitely many) arrows per object.
```
a ------> b
     f
a ------> b
     g
a ------> b
     h
```

You can also have multiple (or infinitely many) arrows that go to the same object.
```
a ----> a
    f

a ----> b
    g

b ----> b
    h
```

### Composition

If you have objects `a`, `b` and `c` with arrow `f` connecting `a` and `b`, and arrow `g` connecting `b` and `c`, then there must always exist an arrow that is a composition of the functions `f` and `g`.

```
a -----> b -----> c
    f        g

a --------------> c
       g ∘ f
```

In this example, that would be `g ∘ f` (or `g after f`). Going to `a` to `b` using `f` and going to `b` to `c` using `g`, is identical to going to `a` to `c` using `g ∘ f`.

There might be mulitple arrows going from `a` to `c`, but `g ∘ f` is a composition of `f` and `g` and must exist.

Composition is kind of like a giant multiplication table. You have to define all possible combinations of how you can compose arrows going between objects. The whole information about the category is in this 'multiplication' table (composition table). Different composition tables will give you different categories.

### Identity

For every object there is an identity arrow.

```
a ---------> a
     id a
```

```
a ---------> b --------> b
      f          id b
```

If you were to compose arrow `f` with arrow `id b`, you would get back arrow `f`. In a sense this can be thought of as an identity. In the multiplication example, in terms of composition, `id` can be thought of as a 1. **id<sub>b</sub> ∘ f = f**

**id<sub>b</sub> ∘ f** is a morphism, **f** is a morphism, and they are both equal.

Similarly in this example:

```
a ---------> a ---------> b
    id a           g
```

**g ∘ id<sub>b</sub> = g**. `g` after `id b` is equal to `g`.
