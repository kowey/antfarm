Antfarm is a referring expression generator for use in generating
definitons.

In a definition, you create miniature worlds with a couple of things
in them, sometimes mentioning the things a few times.

TODO:EXAMPLE of a definition

It is meant to be used as a library, but we include a small demonstrator
program to help show what it's supposed to do

    $ antfarm a1
    an ant

    $ antfarm a1, a2
    an ant, another ant

    $ antfarm a1 a2
    two ants

    $ antfarm a1 b1
    an ant and a box

    $ antfarm a1 b1, a2, b2
    ant an and a box, another ant, another box

    $ antfarm a1, b1, a1
    an ant, a box, the ant
