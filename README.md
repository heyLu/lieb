# lieb

Because I can, and because it's fun, you should [try it][scm48hours]!

## Try it

    # for development, recompiles every time
    $ make repl

    # when you're more done
    $ make lieb
    $ ./lb
    lieb> (define answer 42)
    42
    lieb> (= 583 answer)
    #f
    lieb> (load "std.scm")
    ; whatever the value of the last expr in std.scm is
    lieb> (filter odd? (range 10))
    (1 3 5 7 9)

## What's missing?

* macros (where to start?)
* clojurey syntax (I like it more)
* speed (why does `(filter odd? (range 10000))` take so long?)
* comments
* more fun!
    - some kind of binding, possibly manual?
    - macros (to make more stuff implementable in lieb itself, such as
      case and cond)

[scm48hours]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
