Evan Haralampoudis, evanhara
Natalie Rosenbaum, natros
Thomas Urey, turey

For our project, we created a command-line interface for explaining a subset of the Ocaml execution model through a small step execution model. We are all CIS 1200 students and through our time as TAs, we have seen that two of the most challenging topics for students are recursion and HOFS (transform/fold). Becuase of this, our goal was to allow students of CIS 1200 to be able to input simple OCaml expressions and see a step-by-step execution of their expression. 

The main components of this project are OCamlParser, OCamlPrettyPrinter, OCamlStepper,
OCamlSyntax, and OCamlTypes. We would recommend to start with the syntax, then read
the parser and pretty printer files, finally followed by OCamlTypes and then the stepper.
This should give an overview of our syntax and how we construct our ASTs and then
how we ultimately step and evaluate them. As you can see in our import statements
we did not rely on many libraries. Mainly the applicative, base, and pretty print libraries.

In order to run the main program, build using the cabal file (cabal build) and then run our Main.hs file 
in the app directory by running ./run.sh. From there, enter top level lets and then the expression 
you want to step!