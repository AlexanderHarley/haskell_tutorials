# Terminal and Initial Objects [4.1]

## Kleisli Categories Continued

It is important to fully understand Kleisli Categories, as they are an important part to understanding **Monads**. It is this kind of weird construction where you have to hold two categories in your head at the same time.

The construction is like this:

```
C

a ------------> b
```

You start with one category (lets call it `C`), and based on this category you're trying to build another category (the Kleisli Category). The objects in the Kleisli Category are exactly the same as in the starting category:

```
Kleisli

a ------------> b
```

However, the arrows are **not** the same. For every object, the arrow gives some other object. We looked at one particular case before where for every type, we assign to it a new type which is a pair of the same type and a `String`.

```
a ------------> (a, String)
```

We could also represent this as:

```
a ------------> m a
```

`m` is a **mapping** that maps objects to objects, or in this case, types to types. We will later learn that this type of mapping is called a **Functor**. We just wanted to look at the Kleisli Category before Functors because there are all these interesting categories that are non-trivial, and yet they are not exactly the category of types and pure functions.

So if in category `C` there is an arrow between `a` and `b`, and `a` and `m b`, in the Kleisli category the arrow between `a` to `b` is equal to the arrow between `a` and `m b` in category `C`.


```
C
        x
a ------------> b
        y
a ------------> m b

( m b = (b, String) )


Kleisli
        z
a ------------> b
```

### y = z

So `y` will be an _embellished_ arrow so to speak, and this embellished arrow in category `C` is a direct arrow in the `Kleisli` category.

So it's like we're implementing a new category in terms of the first category. In this example, implementing a new function in in terms of a function from the other category. For programmers you could think of `C` as the implementation of the `Kleisli` category. `z` is an arrow, and `y` is how it's implemented.

But how do we know the `Kleisli` category is a category? We have to have composition of these arrows.

If we were to add another object, `c` to the above categories:

```
C
        x               u
a ------------> b ------------> c
        y
a ------------> m b
                        w
                b ------------> m c

( m b = (b, String) )
( m c = (c, String) )


Kleisli
        z               v
a ------------> b ------------> c
```

So in category `C` if we have an arrow between `a` -> `m b` (`y`), and `b` -> `m c` (`w`), in this category these arrows do not compose because the end of `y` is the not the same as the start of `w`. So if we can't compose them in category `C`, how do we compose them in the `Kleisli` category?

If we go back to the previous lecture with the C++ `compose()` example, in this case where it's `(b, String)` and `(c, String)`, we can do this clever trick where we call function `y` first and get the `(b, String)` pair, and then we split this pair into `b` and `String` and pass this `b` to the object `b`, then call `w` and can concatenate the `String` from `(b, String)` with the `String` from `(c, String`) and return:

```haskell
(c, s1 ++ s2)
```

In general for any kind of mapping, this isn't true. We were just lucky in this circumstance that there was a composition,  and that we could define a clever way of composing these things. If we find a clever way of composing the implementation, we can say this is how we compose the arrows in this `Kleisli` category.

In order for the `Kleisli` category to be a category we also need identity.

But if we were to implement identity in category `C`, it would have to go from `a -> m a` (or `a -> (a, String)`). It has to be a unit (or identity) with regards to the new special kind of composition, and in order to make it a unit with respect to this composition, we have to pick a `String` that is an empty `String`, so that the concatenation with an empty `String` will return the original `String`.

To satisfy all the axioms of category theory we have to also make sure we can compose them in any order (associativity). It is associative because the concatentation of `String`s is associative.

So in this case we had to prove all these axioms: We had to come up with a way of composing the arrows, we had to figure out what the unit (or identity) is, and we had to show that it's associative. Once we do that, we can say that we have a `Kleisli` category. And if this is a `Kleisli` category, then the **mapping** of each arrow (such as from `a -> (a, String)` and `b -> (c, String)` etc.), is called a **Monad**. This is one of the many definitions of a **Monad**, but this is a clean definition of a **Monad** for programmers. We will look further at **Monads** later, but it is important to understand this categorical construction.

## Set Theory vs Category of Sets

So we talked about Sets before - There is this **category of Sets** and there is also **Set Theory**, and these are two views which are very useful to have.

One view is that Sets are things which have elements. Using these elements we can define things like a function maps elements to elements, or we can define an empty Set, which has no elements etc. So a lot of these things can be defined in terms of elements.

And then we have this **category of Sets**, where we don't know that Sets contain elements - we can't talk about elements, the only thing we can talk about is these arrows. In **Set Theory** we know that we can have functions between Sets. Everytime we have a function from a Set to another Set, this will be our arrow in our **category of Sets**.

Everytime we have a function between two sets, we have an arrow. If we have just one function between two sets, there will be just one arrow. If there are multiple functions, there will be multiple arrows.

And we also know how to compose functions - the result of one function is the argument to another function. So we can look at Sets and see how these functions compose, forgetting about the elements, and just say that these two functions compose this way.

But the thing is, we have forgotten everything about elements but in Set Theory there is an **Empty Set**. This is a Set that has no elements, but how can we define an **Empty Set** in a category without knowing about elements? The same for a **Singleton Set** and a **Cartesian Product**? We have to redine everything in terms of arrows and their composition.

## Universal Construction

In category theory, we use **Universal Construction** to pick a particular kind of object, arrow, or pattern, and say we used something like this before when looking at theories (**Set Theory** for example), but here is a **Universal Construction** that gives you the same thing.

Since we cannot look inside an object, the only thing we can do is define properties of an object in terms of it's relation to other objects. The relations between these objects are the arrows. Everything must be defined in terms of these arrows.

In general, if we want to identify a specific object with a unique property, we'll have to define it in respect to every other object in the category - the whole _'universe'_ of objects. This is the same as when talking about Epimorphisms and Monomorphisms before, they are also defined in terms of the _'universe'_. We do a very similar thing with **Universal Constructions**.

The general method of **Universal Constructions** can be thought of as being similar to **Google**. The first step is to define a pattern. This pattern is some combination of objects and arrows. It is almost then like _googling_ the category with that pattern - Show me everything in category `Y` with pattern `X` construction, for example.

This can result in many _'hits'_, perhaps infinitely many. So we need some way of ordering or _ranking_ these results, similar to **Google**. Maybe all of them are not comparable (e.g. **Partial Orders**), but you can at least compare some of the objects to say that one is better than the other. This does not guarantee that there will be something at the top of this _ranking_, but if there is then this defines the object that you are looking for.

### A Simple Example of Universal Construction

Lets look at a **Singleton Set**. This is a set that has one element, but we cannot talk about elements, so how does this Set relate to other Sets? We have to look at arrows to and from other Sets.

There is one property of the **Singleton Set** that is interesting - **it has an arrow coming from every other Set**.

```haskell
a -> ()
```

From **any Set** (or any Type), there is an arrow from any Set to a **Singleton Set** (or Unit). There is even an arrow from the **Empty Set** (or Void) to the **Singleton Set**. This is a sort of universal property of the Singleton Set.

However, the **category of Sets** is very rich in arrows, and the Singleton Set isn't the only one with this property.
