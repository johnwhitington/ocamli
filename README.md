Tiny OCaml
==========

1. A program consists of a single top-level structure item. For example, this is
a program:

1 + 2 * 3 (OCaml will parse this as "let _ = 1 + 2 * 3")

So is this:

let x = 1 in
  let y = 2 in
    x + y

But this is not:

let x = 1

let y = 2 in
  x + y

2. Supported expressions

A. Integers

...,(-1),0,1,...

B. Booleans

true false

C. Operators on integers

+ - * /

D. Comparisons on integers

< <= = >= <>

E. Boolean operators

&& ||

F. Conditional

if e then e1 else e2

G. let binding

let var = e1 in e2

H. Function application

f x

I. Recursive let binding

let rec f a [b c...] = e1 in e2

3. Example program:

let rec factorial x =
  if x = 1 then 1 else x * factorial (x - 1)
in
  factorial 4

