# Category Theory

## Functions and Epimorphisms
Functions are defined in mathematics as special kinds of relations. A relation is just a sub-set of pairs of elements.

If we were to talk about a set of pairs from Set theory, a set of pairs is called a **Cartesian product**. If we were to have two sets, `s1` and `s2`, and were to do a Cartesian product of the two (so all elements from `s1` paired with all possible elements from `s2`, the set of all pairs forms a Cartesian product), if we were to pick a sub-set of this Cartesian product, this would by definition be a **relation**.

So in this sense, relation does not have directionality. Functions however have directions (think arrows). An element from `s1` can have relations with many elements from `s2`, or vice versa.

```
s1 a ------------> s2 a
s1 b ------------> s2 a
s1 c ------------> s2 a

s1 a ------------> s2 a
s1 a ------------> s2 b
s1 a ------------> s2 c
```

If we were to think of a category with types and functions.
