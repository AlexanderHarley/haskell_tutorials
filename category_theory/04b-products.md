# Products [4.2]

## Terminal Objects Continued

Continuing with **Terminal Objects**, we talked before about _incoming_ arrows to the **Terminal Object**, but we didn't talk about _outgoing_ arrows.

So the _incoming_ arrows to the **Terminal Object** have to be unique, but there can be outgoing arrows (and there usually are). These _outgoing_ arrows help us define generalised elements in other objects. Every arrow from a **Terminal Object** to another object is a definition of a generalised element in the other object. Because we are talking about category theory we can't see the elements, but this is as close as we can come to a _generlised_ element.

## Terminal Objects vs Initial Objects

When we were talking about Terminal Objects, we mentioned that the Initial Object is the same but with the arrows reversed.

It turns out that this trick of reversing arrows has a much deeper meaning:

**Every construction in category theory has it's opposite construction that's done by reversing it's arrows.**

So if we have the definition of a Terminal Object, we get the definition of an Initial Object for free by repeating the same construction but with inverting the arrows.

For any category you can always create another category that is the same as the original category, but with the arrows reversed.

So if we have category `C` with a morphism between `a` and `b`, there is the opposite category `C op` (normally called _C opp_) which has the same objects, but with the arrows reversed:

```
C
        f
a ------------> b

C op
       f op
a ------------> b
```
