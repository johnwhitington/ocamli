OCamli
======
OCamli is a proof-of-concept step-by-step interpreter for OCaml programs intended for teaching purposes and, eventually, as a debugger. It works by reading an OCaml program into an abstract syntax tree, then interpreting the AST one reduction at a time, optionally displaying each reduction and underlining the reducible expression. For example:

![showall](https://user-images.githubusercontent.com/1702581/27963402-de08f0f0-632c-11e7-8263-43e5f086819a.png)

With just `-show` instead:

![show](https://user-images.githubusercontent.com/1702581/27963403-de0a29e8-632c-11e7-90b1-854e0f81840b.png)


Running the classic factorial program, using a set of options to elide parts of the output (there are too many options, and the interface will be improved):

![factorial](https://user-images.githubusercontent.com/1702581/27963400-ddeae8f8-632c-11e7-8ac1-9b5ef7493a4b.png)


**Mutable code**

Some of `ocamli`'s output is a little low-level for now, but it's important to emulate what OCaml itself does by making the actual calls to primitives like `%setfield0`:

![mutable](https://user-images.githubusercontent.com/1702581/27963398-dde74a22-632c-11e7-94a6-58a898e121ba.png)



**Input/Output**

Sometimes the approach of printing everything gets far too much. We will need methods for hiding this internal detail by default:

![printint](https://user-images.githubusercontent.com/1702581/27963397-dde723c6-632c-11e7-982c-8ac81dd9014e.png)


**Standard Library Functions**

You should be able to use some or most of the Standard Library functions. Presently `Printf` and `Scanf` are not loaded due to bugs.

![listmap](https://user-images.githubusercontent.com/1702581/27963401-ddeb93f2-632c-11e7-9ccf-c22331236026.png)


**Searching**


"I want to see the last few steps before `if true`":

![search](https://user-images.githubusercontent.com/1702581/27963399-ddea4c04-632c-11e7-85a1-3d2e8bb872af.png)



Examples
--------

The directory *examples* contains a number of little programs written for development.

The directory *OCaml from the Very Beginning* contains all the examples and exercises from the beginner's OCaml [textbook](http://www.ocaml-book.com/).


Paper
-----
The eventual use of `ocamli` as a debugger is sketched in the position paper ["Visualizing the Evaluation of Functional Programs for Debugging"](http://www.cs.le.ac.uk/people/jw642/visfunc.pdf), given at SLATE'17 in June. 

Selected command line options
--------------------

`eval <filename | -e program> [-- arg1 arg2 ...]`

**Loading and runnning programs:**
 
 * -e \<string> Evaluate the program text given
 * -show  Print the final result of the program
 * -show-all  Print steps of evaluation
 * -e-name \<string>  Set the module name for the next -e instance
 * -no-stdlib  Don't load the standard library (for speed)


Multiple files and `-e` options may be given, and will be treated as zero or more modules followed by one main program.

**Searching:**

*  -search \<search-term> Show only matching evaluation steps
*  -regexp  Search terms are regular expressions rather than the built-in system
*  -invert-search  Invert the search, showing non-matching steps
*  -highlight Highlight the matching part of each matched step.
*  -n \<integer> Show only <x> results
*  -until \<search-term>  show only until this matches a printed step
*  -after \<search-term> show only after this matches a printed step
*  -until-any \<search-term> show only until this matches any step
*  -after-any \<search-term> show only after this matches any step
*  -invert-after  invert the after condition
*  -invert-until  invert the until condition
*  -stop  stop computation after final search results
*  -repeat  allow the after...until result to be repeated.
*  -upto \<n> show n lines up to each result line
*  -times \<n> Do many times

**Interaction:**

*  -prompt  Require enter after each step but last
*  -step /<float> Wait a number of seconds after each step but last  

**Elision:**

*  -remove-rec \<string> Do not print the given recursive function
*  -remove-rec-all  Do not print any recursive functions
*  -show-pervasives  Show Pervasives such as :=
*  -fast-curry  Apply all curried arguments at once. 
*  -show-stdlib-init  Show initialisation of standard library
*  -no-arith  Ellide simple arithmetic
*  -no-if-bool Don't show if false, if true stage
*  -no-var-lookup Don't show stage immediately after variable lookup 
*  -side-lets Show value-lets at the side
 
**Configuration:**

*  -pp <prettyprinter> Set the prettyprinter "normal" or "simple" (default)
*  -width <integer> Set the output width
*  -top  Do nothing, exit cleanly (for top level) 
*  -dtiny  Show Tinyocaml representation
*  -dpp  Show the pretty-printed program
*  -debug  Debug (for OCAMLRUNPARAM=b)
*  -no-syntax  Don't use syntax highlighting
*  -no-typecheck  Don't typecheck
*  -no-collect  Don't collect unused lets
*  -otherlibs <path> Location of OCaml otherlibs (Unix, Bigarray, Str)

Implementation
--------------

The `ocamli` interpreter requires no patches to the OCaml compiler; it is implemented entirely using `compiler-libs`.

The OCaml program is lexed and parsed by `compiler-libs`, typechecked, then the parse tree is converted to a simpler, more direct datatype, called `Tinyocaml`.

The `Tinyocaml` representation is then interpreted step-by-step, optionally printing each stage out. We use a custom prettyprinter, but hope to eventually contribute back to the standard one.

Keeping `ocamli` in sync with OCaml will involve updating it to reflect any changes in the `compiler-libs` API and adding any new features added to the OCaml languages. So, we will have `ocamli` 4.05 for OCaml 4.05 and so on.

The `ocamli` facilities for conversing with C code (such as OCaml `%external` declarations) are based on reading and writing OCaml heap values. The interpreter and the interpreted program and the compiled parts live in the same process with the same OCaml runtime.



Use at runtime
-------------------

The interpreter can be used at runtime, and the resulting value bought back into the caller:


```
# let x : int list * int list =
  Tinyocaml.to_ocaml_value\\
    (Runeval.eval_string "List.split [(1, 2); (3, 4)]");;
val x : int list * int list = ([1; 3], [2; 4])
```

Support for this is very rudimentary at the moment.

PPX_eval
-----------

Writing 

```
let compiler_command = [%compiletimestr "Sys.argv.(0)"])
```

in a normal compiled OCaml program with `PPX_eval` generates

```
let compiler_command = "ocamlopt"
```

Status
------
The `ocamli` interpreter is a proof-of-concept. Do not expect it to run your larger programs!

It supports just enough of the language to load (and run the module initialisation of) most of the OCaml Standard Library. This is quite a large number of constructs, though, including functors, first class modules and so on.

The `ocamli` interpreter can run almost all the programs in the *OCaml from the Very Beginning* [textbook](http://www.ocaml-book.com/). The examples are included in the download.

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

