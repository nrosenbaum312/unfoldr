Evan Haralampoudis, evanhara
Natalie Rosenbaum, natros
Thomas Urey, turey

The main components of this project are OCamlParser, OCamlPrettyPrinter, OCamlStepper,
OCamlSyntax, and OCamlTypes. We would recommend to start with the syntax, then read
the parser and pretty printer files, finally followed by OCamlTypes and then the stepper.
This should give an overview of our syntax and how we construct our ASTs and then
how we ultimately step and evaluate them. As you can see in our import statements
we did not rely on many libraries. Mainly the applicative, base, and pretty print libraries.

In order to run the main program, build using the cabal file and then run our Main.hs file 
in the app directory, 