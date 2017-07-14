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

`m` is a mapping that maps objects to objects, or in this case, types to types. We will later learn that this type of mapping is called a **Functor**. We just wanted to look at the Kleisli Category before Functors because there are all these interesting categories that are non-trivial, and yet they are not exactly the category of types and pure functions.

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

So in category `C` if we have an arrow between `a` and `m b` (`y`), and `b` and `m c` (`w`), in this category these arrows do not compose because the end of `y` is the not the same as the start of `w`. So if we can't compose them in category `C`, how do we compose them in the `Kleisli` category?
