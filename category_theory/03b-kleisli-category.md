# Kleisli Category [3.2]

## Subsets

We haven't looked at many examples of **partial orders** or **preorders** yet, so lets define a relation on sets. This will be a relation on inclusion - what it means to be a subset of another set. But what kind of relation is it?

Every Set is a subset of itself, which gives us identity (or in terms of _order_, we'd call it **reflexivity**, meaning an object is in relation with itself).

```
a ⊆ a
```

_Note: `a ⊆ b` means `a` is a subset of some of all elements of `b`._

The other thing we need to check for in an order is composition:

If `a` is a subset of `b`, and `b` is a subset of `c`, then automatically `a` must be a subset of `c`. So we have this composition and associativity that we need in a category.

So it's definitely a **preorder**:

```
        ⊆                ⊆
a -------------> b -------------> c
```

But is it a **partial order**?

### a ⊆ b ∧ b ⊆ a ⇒ a = b

_Note: `∧` means `and`.
`⇒` means if the left side is true, then the right side is also true._

If `a` is a subset of `b`, and `b` is a subset of `a`, then `a = b`. So therefore it is also a **partial order**. This is the quality that **partial order** has over **preorder**, and is known as **anti-symmetry**. What this means is that the only way the two sets could be a subset of each other is if they both contain the same elements, and therefore are equal.

## Pure Functions

Lets think of a theoretical programming scenario, where we have a C++ application with a bunch of functions.

```c++
bool negate(bool x) {
    return !x;
}

/* more functions */
```

One day our manager comes along and says that there's a new requirement where every function must now be logged in the application, and it must be implemented in the simplest way possible. He suggests implementing a global log for each function. Although a contrived example as this would normally be logged in a database instead of appending a string (and abstracted to a seperate function), this more generally refers to any mutable state in the application.

```c++
string log = "";

bool negate(bool x) {
    log += "not!";
    return !x;
}

/* more functions */
```

Their suggestion of simplicity was based on that it required  relatively few amount of lines of code to implement, but there is a kind of _hidden_ complexity, whereby all our functions are now reliant on this `log` through this kind of long distance interaction, even though the functions have nothing directly to do with the `log`. If `log` ever gets removed, all your functions are broken.

The complexity has increased even though the seamingly _simplistic_ path has been taken.

What if we were to implement the same feature, but with **pure functions**?

```c++
pair<bool, string>
negate(bool x, string log) {
    return make_pair(!x, log + "not!");
}
```

This is one way of implementing the `log` in a purely functional way, but it violates the principle of seperation of concerns. A better way would be to define a new way of composing functions:

```c++
function<pair<c, string>(a)>
compose(functon<pair<b, string>(a)> f, function<pair<c, string>(b)> g) {
    return [f,g](a x) {
        auto p1 = f(x);
        auto p2 = g(p1.first);
        return make_pair(p2.first, p1.second + p2.second);
    };
}

pair<bool, string>
negate(bool x) {
    return make_pair(!x, "not!");
}
```

Because string concatenation is associative, if we take 3 such functions and compose them, the order in which the logs are concatenated doesn't matter.

What if we were to represent the identity?

```c++
pair<a, string>
id(a x) {
    return make_pair(a, '')
}
```

## Kleisli Category

We have a binary operator that is associative (+) and has a unit (''). This will work for any Monoid, it doesn't have to be just strings. It is quite hard to abstract in C++, but in Haskell the definition would include a Monoid. We want to impose as fewer conditions as possible; the only condition that we have to impose is that it is a Monoid if we want the composition to form a category.

So we have composition, identity and assocativity. The objects in this category (`a`, `b`, `c`) are types, but arrows in this category are not regular functions. The arrow from `a` to `b` is not a function from `a` to `b`, it is a function from `a` to a pair of `b` and `string`.

```haskell
a -> (b, String)
```

An arrow from `a` to `b` is a function that is embellished. We know how to compose these functions and we know what the identity is; we have a category. This category has a name, it's called a **Kleisli Category**. These functions that are embellished are known as **Kleisli arrows**. Kleisli arrows can be defined for a lot of embellishments, not just this example paired with a string.

This is a view of something called a **Monad**. These arrows are composable because this embellishment is a **Monad**.
