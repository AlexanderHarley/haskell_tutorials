# Coproducts and Sum Types [5.1]

When you take a **Product** and reverse the arrows you create a **Coproduct**. This is standard nomenclature where you take something and reverse the arrows it's often called **Co-**, for example, **Monad** and **Comonad**, **Monoid** and **Comonoid**, **Limit** and **Colimit** and so on.

So we take the **Product** which we defined previously:

![product](https://user-images.githubusercontent.com/13851085/36213662-30bc48ee-119f-11e8-9d74-253254aef8fd.png)

But instead of having **projections** we will have what are called **injections**. A **Coproduct** will match this pattern:

![coproduct](https://user-images.githubusercontent.com/13851085/36213873-c110541c-119f-11e8-862f-d260fca13550.png)

Similar to the **Product** there are lots of `c`'s that have two injections, so we need to again use **Universal Construction** to find to _best_ one:

![coproduct2](https://user-images.githubusercontent.com/13851085/36214064-5251fa98-11a0-11e8-8e90-4225292bde73.png)

```haskell
m  :: c -> c'
i' :: m ∘ i
j' :: m ∘ j
```

This is the definition of the **Coproduct**.

So in programming the **Product** gives us the **pair**, or a two-length tuple, for example, `(a, b)`, but the **Coproduct** is slightly harder to recognise.

If we were to look at the **Coproduct** in Set theory, there is something called the **union** of two Sets, where you take all elements from one Set and all elements from another Set to create a union Set.

But what happens if the elements overlap, or if `a` and `b` are the same set for example? So the **union** or a set with itself would be just the same Set, as there are no duplicate elements.

However, you can also _tag_ each of the elements, which mean that even though it is the same element, one is from Set `a` and one is from Set `b`, resulting in essentially a duplicated Set. This is called a **discriminated union**.

So is the **Coproduct** a **union** or a **discriminated union**? The answer is it is a **discriminated union**.

It is possible to map a **discriminated union** to a **union** by mapping all duplicate elements to a single element, however it's not possible to map a **union** into a **discriminated union**.

In terms of types, the **discriminated union** is called a **tagged union** or **variant**. It means that you have a data type (let's say a union of `Int` and `Bool`), it is something that contains _either_ an `Int` or a `Bool`. This is different to a **pair** which requires _both_ to construct it.

A simple example of this is an enumeration. Let's look at an example in Haskell:

The canonical way of describing a **Product** is using a **pair**, and it is built into the language.

The canonical definition of a  a **union** (also called a **sum type**) is `Either`.

```haskell
data Either a b = Left a | Right b
```

Similarly to how we have two **projections** in the **Product** which in Haskell are the equivalent of `fst` and `snd`, we have two **injections** in the **Coproduct** which are the equivalent to the data constructors `Left` and `Right`.
