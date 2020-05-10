# README

Here's a sample parser for depython. You'll need to modify 3 files to complete your assignment:

* lexer.mll
* parser.mly
* syntax.ml

To test your parser, you can use the parse function defined in main.

I broke the Makefile, so you should probably run 
```
$ make clean
$ make
```
after every change to your files.

To parse a string, use the parse function:
```
$ ocaml
        OCaml version 4.05.0

# parse "123";;
- : Syntax.prog = Module [Expr (Num 123)]
```
