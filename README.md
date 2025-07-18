# Metrical Hierarchies

This repository contains the Lisp code and audio examples from my [bachelor thesis](https://leon-focker.github.io/metrical-hierarchies/Leon%20Focker%20-%20Algorithmische%20Ans%C3%A4tze%20zur%20integrativen%20Gestaltung%20von%20Rhythmus%20mittels%20metrischer%20Strukturen.pdf) on the parametric design of rhythms using metrical structure. It provides tools for algorithmically determining the metrical weight of beats based on various metrical models.

[Click here](https://leon-focker.github.io/metrical-hierarchies/audio/) to listen to the audio examples in the browser.

## Install

Clone [this repository](https://github.com/Leon-Focker/metrical-hierarchies) or download the source code. Then either:

**Option 1:** Place the folder in your [Quicklisp local-projects directory](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html), for example:
```
~/quicklisp/local-projects/
```

**Option 2:** Add the path manually in your Lisp session:
```
(push "/full/path/to/metrical-hierarchies/" asdf:*central-registry*)
```

Then load the system with:
```
(ql:quickload :metrical-hierarchies)
```