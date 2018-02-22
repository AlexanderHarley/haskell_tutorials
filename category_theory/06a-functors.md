# Functors [6.1]

**Functors** are very important, so much so that you can consider all the previous sections as a build up to **Functors**.

Mathematicians will tell you that it's really **Natural Transformations** that are important, but you need **Functors** in order to define **Natural Transformations**.

Mathematically speaking, **Functors** are quite a simple idea. If you take two categories, a **Functor** is just mapping from one category to another. So why are **Functors** so important?

When we were talking about **Products** and **Coproducts** before, we were using quite loose language when it came to looking for a pattern. Since a category is just a definion of structure abstracted to bare essentials, if you say you want to recognise a certain structure in a category that means you should define your pattern _as a category_. If it's a small pattern, then it's just a small category (for example, keep in mind the Singleton category with just one object). So being able to recognise one category inside another category is the definition of pattern recognition. We will revisit this when we look at the more general **Limits** and **Colimits** where things are defined using these patterns, where you'll see that **Product** and **Coproduct** are just examples of **Limits** and **Colimits**.

So this is what's important, _being able to define mappings between categories_. We can think of one category as being the pattern (or **model**), and we map it into another category and we recognise a match for this **model** (or embed this **model**) in another category.
