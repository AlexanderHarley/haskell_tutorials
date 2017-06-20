## Monomorphisms [2.2]

![monomorphism-02](https://user-images.githubusercontent.com/13851085/27336910-df1a518a-55c8-11e7-9fad-5ac4c1c38047.jpg)

An example of something that **isn't** a **monomorphism** (also called a **non-injective** function) would be something like `f`. A **non-injective** function will just map two different elements (`x1`, `x2`) of one set (`a`) into the same element of another set (`b`). `f` is **not** a **monomorphism**.

If we were to look at what **is** a **monomorphism**:

If you compose `g1` with `f`, or `g2` with `f`, you will get the same result. This is a **monomorphism**.

_Note: This is very similar to Epimorphisms, but rather than looking at post-composition, we look at pre-composition._

<div style="font-size: 22px; margin-bottom: 15px;">
f ∘ g<sup>1</sup> = f ∘ g<sup>2</sup>
</div>

This is because they only differ on the two points: `x1` and `x2`. They are equal on everything else but they differ on the way they map `z`. You will get the same result as both `x1` and `x2` are mapped into the same `y`.

<div style="font-size: 22px; margin-bottom: 15px;">
∀c . ∀g1 , g2 :: c -> a <br/>
f ∘ g<sup>1</sup> = f ∘ g<sup>2</sup> ⇒ g<sup>1</sup> = g<sup>2</sup>
</div>

> Epimorphisms and Monomorphisms are defined for every category.

_Note: However you can get categories that don't contain any Monomorphisms or Epimorphisms._
