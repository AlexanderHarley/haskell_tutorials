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

So we talked about Sets before - There is this **category of Sets** and there is also **Set Theory**, and these are two views which are very useful to have.

One view is that Sets are things which have elements. Using these elements we can define things like a function maps elements to elements, or we can define an empty Set, which has no elements etc. So a lot of these things can be defined in terms of elements.

And then we have this **category of Sets**, where we don't know that Sets contain elements - we can't talk about elements, the only thing we can talk about is these arrows. In **Set Theory** we know that we can have functions between Sets. Everytime we have a function from a Set to another Set, this will be our arrow in our **category of Sets**.
