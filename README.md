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

**Mutable code**

Some of `ocamli`'s output is a little low-level for now, but it's important to emulate what OCaml itself does by making the actual calls to primitives like `%setfield0`:

```
    let x = ref 0 in (:= x (! x + 1)); (! x)
=>  let x = let x = 0 in <<%makemutable>> in (:= x (! x + 1)); (! x)
=>* let x = {contents = 0} in (:= x (! x + 1)); (! x)
=>  let x = {contents = 0} in (:= {contents = 0} (! x + 1)); (! x)
=>  let x = {contents = 0} in ((let x = {contents = 0} in fun y -> <<%setfield0>>) (! x + 1)); (! x)
=>* let x = {contents = 0} in ((fun y -> let x = {contents = 0} in <<%setfield0>>) (! {contents = 0} + 1)); (! x)
=>  let x = {contents = 0} in ((fun y -> let x = {contents = 0} in <<%setfield0>>) ((let x = {contents = 0} in <<%field0>>) + 1)); (! x)
=>* let x = {contents = 0} in ((fun y -> let x = {contents = 0} in <<%setfield0>>) (0 + 1)); (! x)
=>  let x = {contents = 0} in ((fun y -> let x = {contents = 0} in <<%setfield0>>) 1); (! x)
=>  let x = {contents = 0} in (let y = 1 in let x = {contents = 0} in <<%setfield0>>); (! x)
=>* let x = {contents = 1} in (); (! x)
=>  let x = {contents = 1} in ! x
=>  ! {contents = 1}
=>  let x = {contents = 1} in <<%field0>>
=>* 1
```

**Input/Output**

Sometimes the approach of printing everything gets far too much. We will need methods for hiding this internal detail by default:

```
$ ocamli -e "print_int 42" -show-all
    print_int 42
=>  let string_length a = <<%string_length>> in let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in let unsafe_output_string a b c d = <<caml_ml_output>> in let output_string oc s = unsafe_output_string oc s 0 (string_length s) in let i = 42 in output_string <out_channel> (string_of_int i)
=>  let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in let i = 42 in (let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in fun s -> unsafe_output_string oc s 0 (string_length s)) (string_of_int i)
=>  let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in let i = 42 in (let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in fun s -> let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (string_of_int i)
=>  let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in let i = 42 in (let string_length a = <<%string_length>> in fun s -> let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (string_of_int i)
=>  let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in let i = 42 in (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (string_of_int i)
=>  let format_int a b = <<caml_format_int>> in let string_of_int n = format_int "%d" n in (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (string_of_int 42)
=>  (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (let format_int a b = <<caml_format_int>> in let n = 42 in format_int "%d" n)
=>  (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (let n = 42 in (let a = "%d" in fun b -> <<caml_format_int>>) n)
=>* (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) ((fun b -> let a = "%d" in <<caml_format_int>>) 42)
=>  (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) (let b = 42 in let a = "%d" in <<caml_format_int>>)
=>* (fun s -> let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)) "42"
=>  let s = "42" in let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in let oc = <out_channel> in unsafe_output_string oc s 0 (string_length s)
=>  let s = "42" in let string_length a = <<%string_length>> in let unsafe_output_string a b c d = <<caml_ml_output>> in unsafe_output_string <out_channel> s 0 (string_length s)
=>  let s = "42" in let string_length a = <<%string_length>> in (let a = <out_channel> in fun b c d -> <<caml_ml_output>>) s 0 (string_length s)
=>* let s = "42" in let string_length a = <<%string_length>> in (fun b -> let a = <out_channel> in fun c d -> <<caml_ml_output>>) "42" 0 (string_length s)
=>  let s = "42" in let string_length a = <<%string_length>> in (let b = "42" in let a = <out_channel> in fun c d -> <<caml_ml_output>>) 0 (string_length s)
=>* let s = "42" in let string_length a = <<%string_length>> in (let c = 0 in let b = "42" in let a = <out_channel> in fun d -> <<caml_ml_output>>) (string_length s)
=>* let string_length a = <<%string_length>> in (fun d -> let c = 0 in let b = "42" in let a = <out_channel> in <<caml_ml_output>>) (string_length "42")
=>  (fun d -> let c = 0 in let b = "42" in let a = <out_channel> in <<caml_ml_output>>) (let a = "42" in <<%string_length>>)
=>* (fun d -> let c = 0 in let b = "42" in let a = <out_channel> in <<caml_ml_output>>) 2
=>  let d = 2 in let c = 0 in let b = "42" in let a = <out_channel> in <<caml_ml_output>>
42=>* ()
```

**Standard Library Functions**

You should be able to use some or most of the Standard Library functions. Presently `Printf` and `Scanf` are not loaded due to bugs.

```
$ ocamli -e 'List.(map (fun x -> x * 2) (filter (fun x -> x > 2) [1; 2; 3; 4])))' -show
List.([6; 8])
```

**Searching**


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

Selected command line options
--------------------

`eval <filename | -e program> [-- arg1 arg2 ...]`

**Loading and runnning programs:**
 
 * -e  Evaluate the program text given
 * -show  Print the final result of the program
 * -show-all  Print steps of evaluation
 * -e-name  Set the module name for the next -e instance
 * -no-stdlib  Don't load the standard library (for speed)


Multiple files and `-e` options may be given, and will be treated as zero or more modules followed by one main program.

**Searching:**

*  -search  Show only matching evaluation steps
*  -regexp  Search terms are regular expressions rather than the built-in system
*  -invert-search  Invert the search, showing non-matching steps
*  -highlight Highlight the matching part of each matched step.
*  -n  Show only <x> results
*  -until  show only until this matches a printed step
*  -after  show only after this matches a printed step
*  -until-any  show only until this matches any step
*  -after-any  show only after this matches any step
*  -invert-after  invert the after condition
*  -invert-until  invert the until condition
*  -stop  stop computation after final search results
*  -repeat  allow the after...until result to be repeated.
*  -upto  show n lines up to each result line
*  -times  Do many times

**Interaction:**

*  -prompt  Require enter after each step but last
*  -step  Wait a number of seconds after each step but last  

**Elision:**

*  -remove-rec  Do not print the given recursive function
*  -remove-rec-all  Do not print any recursive functions
*  -show-pervasives  Show Pervasives such as :=
*  -fast-curry  Apply all curried arguments at once. 
*  -show-stdlib-init  Show initialisation of standard library
*  -no-arith  Ellide simple arithmetic
*  -no-if-bool Don't show if false, if true stage
*  -no-var-lookup Don't show stage immediately after variable lookup 
*  -side-lets Show value-lets at the side
 
**Configuration:**

*  -pp  Set the prettyprinter
*  -width  Set the output width
*  -top  Do nothing, exit cleanly (for top level) 
*  -dtiny  Show Tinyocaml representation
*  -dpp  Show the pretty-printed program
*  -debug  Debug (for OCAMLRUNPARAM=b)
*  -no-syntax  Don't use syntax highlighting
*  -no-typecheck  Don't typecheck
*  -no-collect  Don't collect unused lets
*  -otherlibs  Location of OCaml otherlibs

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

