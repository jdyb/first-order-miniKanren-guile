# miniKanren with a first-order representation (on Guile)

This implementation of miniKanren decouples the search strategy from the representation of the search space.  This makes it possible to use a variety of strategies, perform program transformations (even while a program is running), and implement tools such as a debugger.

Learn more from the [2019 miniKanren Workshop paper](http://minikanren.org/workshop/2019/minikanren19-final2.pdf).

# Guile translation

This repository contains a small friendly fork with patches for running the
software on Guile (3.0.1). The aim is to mostly track the upstream
project and provide patches to make it run on Guile.

Why? Because the translator (me) mostly uses Guile at this time.

Upstream is located here: [first-order-miniKanren](https://github.com/gregr/first-order-miniKanren).

# License (MIT)

Please see LICENSE for original license text from the upstream project. 

