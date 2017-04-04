Chapter 16 Examples and Exercises
=================================

The files textstat.ml, textstat.mli and stats.ml contain the text statistics
example as a stand-alone program, extended as the exercise at the end of the
chapter demands.

The file gregor.txt is suitable input. (Note for Windows users: this file may
not appear to have newlines in it when opened up in a Windows text editor. Do
not worry about this.)

The files q2.ml, q3.ml, and q4.ml are for the other questions.

To build these examples:

ocamlc q2.ml -o reverse

ocamlc q3.ml -o bigloop

ocamlopt q3.ml -o bigloop

ocamlc q4.ml -o search

ocamlc textstat.mli textstat.ml stats.ml -o stats

(The name following -o in each example is the name of the executable program)


