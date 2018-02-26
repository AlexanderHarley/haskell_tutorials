# Functors [6.1]

## Introduction

**Functors** are very important, so much so that you can consider all the previous sections as a build up to **Functors**.

Mathematicians will tell you that it's really **Natural Transformations** that are important, but you need **Functors** in order to define **Natural Transformations**.

Mathematically speaking, **Functors** are quite a simple idea. If you take two categories, a **Functor** is just mapping from one category to another. So why are **Functors** so important?

When we were talking about **Products** and **Coproducts** before, we were using quite loose language when it came to looking for a pattern. Since a category is just a definition of structure abstracted to bare essentials, if you say you want to recognise a certain structure in a category that means you should define your pattern _as a category_. If it's a small pattern, then it's just a small category (for example, keep in mind the Singleton category with just one object). So being able to recognise one category inside another category is the definition of pattern recognition. We will revisit this when we look at the more general **Limits** and **Colimits** where things are defined using these patterns, where you'll see that **Product** and **Coproduct** are just examples of **Limits** and **Colimits**.

So this is what's important, _being able to define mappings between categories_. We can think of one category as being the pattern (or **model**), and we map it into another category and we recognise a match for this **model** (or embed this **model**) in another category.

## Functors in Small Categories

For simplicity, to get an idea of **Functors** for this example let's use a small categories (a small category is one in which objects form a **Set**).

If you want to map one category into another category, the first thing you have to map is objects. Since objects form a **Set** in the small category, it's just a mapping of **Sets** which is a function.

There's one thing we haven't mentioned about functions. They are kind of primitive or trivial. What we're really interested in, in mathematics and generally, is mappings that preserve structure. It so happens that functions are mappings between **Sets**, and by definition a **Set** has _no structure_. It just has a bunch of elements and that's it - there is no order. This is one of the reasons why **Sets** can be so difficult to implement in programming, as hardware by it's very nature is organised and **Sets** are disorganised. Some people implement **Sets** as trees (like a binary tree), but to implement a binary tree you need to be able to compare elements, so in that circumstance a **Set** can only be implemented when you can compare elements. Alternatively it could be implemented as a list, but then lists have an order, in which case you then have to say that all these lists that only differ by order represent a **Set**, and so on.

However, from the point of view of mathematics **Sets** work perfectly, they have no structure. Since we primarily look at functions, we rarely look at mappings that preserve structure. But in the rest of mathematics these structure preserving mappings are very important, because we are dealing with structured things such as **Monoids**, **Algebra**, **Geometrical Figures** and so on.

So if a category embodies structure, is there a category that embodies the lack of structure? For example, if we wanted to implement a **Set** as a category, how would we do that since even arrows have structure as they compose.

In this circumstance it would have to be a category with no arrows (other than identity morphisms as they are required), so we could have a category such as:

```
      id a
a ------------> a

      id b
b ------------> b

      id c
c ------------> c

      id d
d ------------> d
```

containing a bunch of objects with identity morphisms, but no arrows between them. This category has a name, it's called a **Discrete Category**.

- `∀x . homC(x, x) = {id x}`
