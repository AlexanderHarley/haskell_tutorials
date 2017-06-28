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

## Homsets

A **homset** is the Set of arrows between any two objects.

```
homset : C(a, b)
         C(a, a)
```

In terms of **homsets**, a **thin category** (also known as a **posetal category**) is one in which every homset is either an empty set or a singleton set (_or to put it another way, a **thin category** is a category whose **homsets** each contain at most one morphism_).
