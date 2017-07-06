OCamli
======
OCamli is a proof-of-concept step-by-step interpreter for OCaml programs intended for teaching purposes and, eventually, as a debugger. It works by reading an OCaml program into an abstract syntax tree, then interpreting the AST one reduction at a time, optionally displaying each reduction and underlining the reducible expression. For example:

```
$ ocamli -e "1 + 2 * 3" -show-all
   1 + 2 * 3
=> 1 + 6
=> 7
```

With just `-show` instead:

```
$ ocamli -e "1 + 2 * 3" -show-all
7
```

Running the classic factorial program, using a set of options to elide parts of the output (there are too many options, and the interface will be improved):

```
$ ocamli examples/factorial.ml -show-all -side-lets -remove-rec factorial -no-if-bool -no-arith
    factorial 4
n = 4 =>  if n = 1 then 1 else n * factorial (n - 1)
n = 4 =>* n * factorial (n - 1)
n = 4 =>  4 * factorial (n - 1)
      =>  4 * factorial (4 - 1)
      =>  4 * factorial 3
n = 3 =>  4 * (if n = 1 then 1 else n * factorial (n - 1))
n = 3 =>* 4 * (n * factorial (n - 1))
n = 3 =>  4 * (3 * factorial (n - 1))
      =>  4 * (3 * factorial (3 - 1))
      =>  4 * (3 * factorial 2)
n = 2 =>  4 * (3 * (if n = 1 then 1 else n * factorial (n - 1)))
n = 2 =>* 4 * (3 * (n * factorial (n - 1)))
n = 2 =>  4 * (3 * (2 * factorial (n - 1)))
      =>  4 * (3 * (2 * factorial (2 - 1)))
      =>  4 * (3 * (2 * factorial 1))
n = 1 =>  4 * (3 * (2 * (if n = 1 then 1 else n * factorial (n - 1))))
      =>* 4 * (3 * (2 * 1))
      =>* 24



```
(Mutable example)

(I/O example)

(standard library example)

"I want to see the last few steps before `if true`":


```
ocamli programs/factorial.ml -show-all -search "if true" -upto 3
=>  3 * (2 * factorial (2 - 1))
=>  3 * (2 * factorial 1)
=>  3 * (2 * (let n = 1 in if n = 1 then 1 else n * factorial (n - 1)))
=>  3 * (2 * (let n = 1 in if true then 1 else n * factorial (n - 1)))
```

Alternatively, we can annotate the node to be shown in the source with `[@show]`:

```
if n = 1 then 1 else n * (factorial (n - 1) [@show])
```

So when we run it:

```
$ ocamli programs/factorial.ml -show-annot
factorial (3 - 1)
factorial (2 - 1)
```

Examples
--------

The directory *examples* contains a number of little programs written for development.

The directory *OCaml from the Very Beginning* contains all the examples and exercises from my beginner's OCaml textbook.


Paper
-----
The eventual use of `ocamli` as a debugger is sketched in the position paper ["Visualizing the Evaluation of Functional Programs for Debugging"](http://www.cs.le.ac.uk/people/jw642/visfunc.pdf), given at SLATE'17 in June. 

Command line options
--------------------

Implementation
--------------

The `ocamli` interpreter requires no patches to the OCaml compiler; it is implemented entirely using `compiler-libs`.

The OCaml program is lexed and parsed by `compiler-libs`, typechecked, then the parse tree is converted to a simpler, more direct datatype, called `Tinyocaml`.

The `Tinyocaml` representation is then interpreted step-by-step, optionally printing each stage out. We use a custom prettyprinter, but hope to eventually contribute back to the standard one.

Keeping `ocamli` in sync with OCaml will involve updating it to reflect any changes in the `compiler-libs` API and adding any new features added to the OCaml languages. So, we will have `ocamli` 4.05 for OCaml 4.05 and so on.

The `ocamli` facilities for conversing with C code (such as OCaml `%external` declarations) are based on reading and writing OCaml heap values. The interpreter and the interpreted program and the compiled parts live in the same process with the same OCaml runtime.



Use with C programs
-------------------

PPX_eval
-----------

Status
------
The `ocamli` interpreter is a proof-of-concept. Do not expect it to run your larger programs!

It supports just enough of the language to load (and run the module initialisation of) most of the OCaml Standard Library. This is quite a large number of constructs, though, including functors, first class modules and so on.

The `ocamli` interpreter can run almost all the programs in the *OCaml from the Very Beginning* textbook. The examples are included in the download.

The `ocamli` interpreter currently makes no guarantee of computational complexity, even when the steps of evaluation are not shown. The extent to which such a guarantee can be given is an open research question.


Future
------

In short, make it more than a proof-of-concept:

* Extend it to the full language
* Better pretty-printing
* Make it fast enough for general use
* Easy invocation as a debugger regardless of build system or source structure\columnbreak
* Better tools for searching and eliding
* Allow interpretation of just one module -- other modules run at full speed
* An interactive interface

