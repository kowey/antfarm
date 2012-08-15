Antfarm is a referring expression generator for use in generating
dictionary definitions, such as this fragment below (referring
expressions highlighted):

“A **sister chromatid** is detached from **another sister chromatid**
resulting in **two chromosomes** at **a kinetochore**”

Referring expression generation is a subtask in natural language
geeration.  The basic goal is to find a way of referring to objects that
both helps the other part to know which object you are referring to, and
which at the same time makes for reasonably fluent expressions.  Antfarm
is not a general purpose referring expression generator; it was written
to support our particular use case of generating dictionary definitions.
So instead of identifying salient properties of an object in some shared
world (eg. colour, size, etc); we instead rely on identifying the object
by its type, its number, and tracking how often it's been mentioned.

Antfarm is meant to be used in conjunction with other natural language
generation components.  We, for example, combine it with the surface
realiser [GenI](http://projects.haskell.org/GenI) to produce complete
sentences which have referring expressions in them.

## Antfarm in action

It is meant to be used as a library, but we included a small
demonstrator program to help show what it's supposed to do

A referring expression can have any number of like objects

    $ antfarm a1
    an ant

    $ antfarm a1 a2
    two ants

It can also contain different kinds of objects

    $ antfarm a1 b1
    an ant and a box

    $ antfarm a1 a2 b1
    two ants and a box

We can also keep track of whether or not you have referred to something
before, so if you have more than one referring expression…

    $ antfarm a1
    an ant

    $ antfarm a1, a1
    an ant, the ant

    $ antfarm a1 a2, a1 a2
    two ants, the ants

But if you refer to a different object(s)…

    $ antfarm a1, a2
    an ant, another ant

    $ antfarm a1, a2 a3
    an ant, another two ants

You can also provide cardinality constraints instead of referring to
specific objects

    $ antfarm 'a >= 3'
    at least three ants

    $ antfarm 'a = 3'
    exactly three ants

Or provide both constraints and instances of objects

    $ antfarm 'a <= 3'
    at most three ants

    $ antfarm a1 'a <= 3'
    between one and three ants

    $ antfarm a1 'a <= 3', a1 'a <= 3'
    between one and three ants, the same ants

Or just refer to the class of objects itself (eg. “ants like sugar”)

    $ antfarm a
    ants

Or talk about apart different individual objects in the order you
mentioned them

    $ antfarm a42 b59, a8, a90, a42, a90 b59
    an ant and a box, another ant, a third ant, the first ant, the third ant and the box

Or (recursively) provide examples of objects

    $ antfarm 'A >= 2 (M = 2 o1)'
    at least two animals (exactly two mammals, an owl)

    $ antfarm 'A >= 2 (M = 2 (d1 c2) o1)'
    at least two animals (exactly two mammals (a dog, a cat), an owl)
