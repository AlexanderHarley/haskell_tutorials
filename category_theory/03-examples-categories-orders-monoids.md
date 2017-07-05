# Examples of categories, orders, monoids [3.1]

We've looked a lot at categories of Sets, but lets look at what other categories there are.

Just like we started with the simplest possible Set, let's look at the simplest possible Category.

## Empty Category (∅)

By itself, this category is useless. However, the value with this category is context, much in the same way how 0 by itself is useless. But when you start adding the 0 to other numbers (e.g. 10, 100) etc. it then becomes useful.

The context of this category would be if we had a category of all categories. If we did have such a category, this would be an important thing.

## One-Object Category

This category has to have at least 1 arrow, as we know that each object in a category must have an identity arrow. This category is also useless by itself, but becomes significant when its combined with other categories.

## Two-Object Category

This becomes interesting as now we have 2 objects there are multiple combinations of arrows that we could have. Both objects have an identity arrow, this is a must, but there can also be no arrows between the two objects, or one or multiple arrows one or both ways.

It's possible to turn any graph into a category by adding additional arrows. Identity arrows must be added, but also certain other composed arrows depending on the graph.

![category-theory-graph-to-category](https://user-images.githubusercontent.com/13851085/27643723-b8af839c-5c19-11e7-9f65-0476869ad7cc.png)

_The red arrows have been added to the graph to turn it into a category. There have been identity arrows added and also composed arrows._

As the graph increases in size, there will need to be more composed arrows added to the category. This won't exponentially increase however due to associativity in the category cancelling out some of the arrows.

This construction in which you keep adding stuff to satisfy certain axioms is a very common construction in category theory, and is called **free** construction. This is because we are freely adding stuff - we are not imposing any additional constraints other than those that are required from the definition of a category. So we are created a **free category** from the graph above.

## Orders and Thin Categories

In a category that's an **order**, an arrow represents something other than a function or something abstract - they represent a relation.

If we were to have two objects (`a`, and `b`), with an arrow going from `a` to `b` if `a` is less than or equal to `b`:

```
        ≤
a -------------> b
```

This arrow doesn't have any other meaning other than `a` is less than or equal to `b`. We could also say this as `a` comes before `b` in some kind of order.

There are certain conditions an order has to fulfil to be an order. There are different types or order: **preorder**, **partial order** and **total order**.

Total order is familiar to programmers in something like a `sort()` function, where the elements must have a total order.

### Preorder

The simplest form of order is **preorder**, which is something less than an order, a _pre_ order. A preorder is a relation like the one above, that satisfies just the minimum of conditions, which is that it has to be composable:

```
        ≤                ≤
a -------------> b -------------> c

                 ≤
a ------------------------------> c
```

If `a ≤ b` and `b ≤ c`, then `a ≤ c`. This is known as **transivity**. This means that preorder is also associative.

As we are a talking about **preorder** as a category, each object must also have an identity:

```
       ≤
a -----------> a

       ≤
b -----------> b

       ≤
c -----------> c
```

This would mean that for every object, it is less than or equal to itself. This is called **reflexivity**. This relation has to be **reflexive**, which is why we have the `and equal to` part, as the object cannot be less than itself.

If there is a relation between two objects there is an arrow, if there isn't then there is no arrow. But there aren't mulitple arrows, it's a binary choice. Either the object is less than equal to, or it is not. In **total order** you can say that between _any_ 2 objects there is an arrow. But in general in **preorder** or even **partial order** this is not true.

A category that has this property (only 0 or 1 arrows between two objects) is called a **thin category**. Every thin category corresponds to a preorder, and every preorder corresponds to a thin category, so there is a 1-to-1 correspondance. In this case, like in many other cases, Category Theory hints that there is something special about a particular theory. So if we consider all possible types of orders, Category Theory tells us that **preorder** is the most basic. This is because its _just_ a category, it's a thin category, but it's still a category.

### Partial Order

In **partial order**, you can't have loops of arrows. For example,, if you have an arrow from `a` to `b`, you can't then have an arrow from `b` to `a`.

Because of this, in a **partial order** some objects are not comparable.

### Total Order

**Total order** has an arrow between any two objects, so you can compare any two objects.


## Homsets

A **homset** is the Set of arrows between any two objects.

```
homset : C(a, b)
         C(a, a)
```

In terms of **homsets**, a **thin category** (also known as a **posetal category**) is one in which every homset is either an empty set or a singleton set (_or to put it another way, a **thin category** is a category whose **homsets** each contain at most one morphism_).

### Monomorphisms and Epimorphisms in Thin Categories

In a thin category, something which is a monomorphism and an epimorphism does not have to be invertible.

When we looked at the category of Sets, the monomorphism was known as an **injective** function, and the epimorphism was known as a **surjective** function. If something is injective and surjective, then it is reversible, which is known as a **bijective** function (or a **bijection**).

This isn't true in every category however, you can have an epimorphism that is a monomorphism, but is not invertible.

```

 monomorphism              epimorphism
      h                         g
x ---------> a ---------> b ---------> c
      h′                        g′
x ---------> a            b ---------> c
```

_Note: The `′` symbol is the **prime**  and is commonly used in mathematics to generate more variable names for things which are similar without resorting to subscripts. It can also be used to denote units (e.g. 9′8″ would normally mean something like 9 foot 8 inches or 9 minutes 8 seconds) The prime symbol is not related to prime numbers._

In a **thin category**, these things are automatically satisfied because there aren't any pairs like these.

So every arrow in a **preorder** is a monomorphism and is an epimorphism at the same time. But it's not necessarily invertible. In fact if it is **partial order**, it is definitely not invertible, because there cannot be an arrow going backwards.

The most general category can be thought of as a preorder, but one that is a "thick" category. When you have an order you think of it as a relation between `a` and `b`. If there is no arrow, there is no relation, it is black and white.

But when you have a "thick" category, if you have no arrows there is no relation, but if you have multiple arrows then they are in a relation, and each of these arrows sort of represents a different proof of the relation between these two objects.

## Monoid

Looking again at the **One-Object Category**, we must have an identity:

```
    id
m ------> m
```

But we can also have multiple morphisms going from `m` to itself.

```
    id
m ------> m

m ------> m
m ------> m
m ------> m
```

Because this is a category, we can compose these morphisms. In a category with multiple objects, some will be composable as some of the morphisms will begin and finish at the same objects, but because all the morphisms in this category go to and from the same object, they are all composable. For that reason, any two arrows are composable in this category.

So we have an identity arrow, and a bunch of other arrows, with composition defined for them, and this composition works for any pair of arrows. This category is known as a **Monoid**.

The other condition that can be imposed in associativity.

**(a \* b) \* c = a \* (b \* c)**

**∃e ∀a . e \* a = a \* e = a**

There is a pre-existing definition of a **Monoid** from Sets that may be more familiar, which existed before categories were invented.

Some examples of **Monoids** could be:

- **Natural Numbers with multiplication** - This is a Monoid as it has a unit, `e`, which is `1`. Anything multiplied by 1 is the same. `10 * 1 = 10`. It's associative, so it's a Monoid.

- **Natural numbers with addition** - The same as above, but this time the unit, `e`, is `0`. And `0` on the left or the right doesn't change anything, e.g. `50 + 0 = 50`, `0 + 50 = 50`. Is it associative? Yes.

- **String concatenation** - If you concatenate two `string`s, you get a `string`. It's a binary operator. Does it have a unit? Yes, an **empty `string`**. If you append an empty string or prepend an empty string, you don't change anything. `'Hello World!' + '' = 'Hello World!'`. This is a good example of a Monoid because it's not symmetrical. With addition an multiplication they are symetrical - you can change the order. With `string`s you can't - if you append two `string`s, the result will be different if you append them in the opposite order. This is a nice example of a Monoid.

Another example would be generally appending `lists`, this forms a **Monoid**. This is why in `Haskell`, `String`s are lists of `Char`s, and form a **Monoid**.

### Monoids in Category Theory

So looking at **Monoids** in Category Theory, are they equivalent with the **Monoids** in Set Theory, and vice versa? The short answer is, Yes.

Lets consider this Monoid, which we'll call `M`:

```
    id
m ------> m

m ------> m
m ------> m
```

There is only one homset, `M(m, m)`. This is a Set of arrows that goes from `m` to `m`. There is only one homset, as there is only object. This is a sort of reflexive homset.

This homset is a Set, so we have a Category and a Set - This Category defines a Set. You take any 2 elements in this Set and they correspond to arrows, but there is a 3rd element in this Set.
