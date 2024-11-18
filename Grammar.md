(* Keywords *)
KEYWORD ::= "begin" | "else" | "end" | "false" | "fun" | "function" | "not"
          | "if" | "in" | "let" | "rec" | "match" | "mod" | "then" | "true"

(* Identifiers and Literals *)
IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*
INTEGER_LITERAL ::= [0-9]+
BOOLEAN_LITERAL ::= "true" | "false"

(* Types and Type Annotations *)
TYPE ::= PRIMITIVE_TYPE | LIST_TYPE | TUPLE_TYPE | FUNCTION_TYPE | GENERIC_TYPE | "(" TYPE ")"
PRIMITIVE_TYPE ::= "int" | "bool"
LIST_TYPE ::= TYPE "list"
TUPLE_TYPE ::= "(" TYPE {"," TYPE} ")"
FUNCTION_TYPE ::= TYPE "->" TYPE
GENERIC_TYPE ::= "'" IDENTIFIER

TYPE_ANNOTATION ::= ":" TYPE

(* Expressions *)
EXPR ::= BOP_EXPR | UOP_EXPR | LIST_EXPR | TUPLE_EXPR | FUNCTION_EXPR 
       | IF_EXPR | MATCH_EXPR | LET_EXPR | IDENTIFIER | "(" EXPR ")" | "begin" EXPR "end"

BOP_EXPR ::= EXPR BOP EXPR
UOP_EXPR ::= UOP EXPR
LIST_EXPR ::= "[" [EXPR {";" EXPR}] "]"
TUPLE_EXPR ::= "(" EXPR {"," EXPR} ")"
BOP ::=
    "+" | "-" | "*" | "mod" | "||" | "&&" | ">" | "<" | ">=" | "<=" | "=" | "::" | "@"
UOP ::= 
    "-" | "not"

(* Functions *)
FUNCTION_EXPR ::= "fun" PARAMETER_LIST "->" EXPR
PARAMETER_LIST ::= IDENTIFIER {IDENTIFIER}  (* No patterns or destructuring *)

(* Let Expressions *)
LET_EXPR ::= "let" ["rec"] IDENTIFIER [PARAMETER_LIST] [TYPE_ANNOTATION] "=" EXPR ("in" EXPR)?

(* Top-Level Declarations *)
TOP_LEVEL_DECL ::= "let" ["rec"] IDENTIFIER [PARAMETER_LIST] [TYPE_ANNOTATION] "=" EXPR

(* If Expressions *)
IF_EXPR ::= "if" EXPR "then" EXPR "else" EXPR

(* Match Expressions *)
MATCH_EXPR ::= "match" EXPR "with" MATCH_CASE {MATCH_CASE}
MATCH_CASE ::= "|" PATTERN "->" EXPR

(* Patterns *)
PATTERN ::= CONSTANT_PATTERN | LIST_PATTERN | TUPLE_PATTERN | IDENTIFIER
CONSTANT_PATTERN ::= INTEGER_LITERAL | BOOLEAN_LITERAL
LIST_PATTERN ::= "[]" | IDENTIFIER "::" PATTERN | "[" PATTERN {";" PATTERN} "]"
TUPLE_PATTERN ::= "(" PATTERN {"," PATTERN} ")"

(* Program *)
PROGRAM ::= {TOP_LEVEL_DECL ";"}+ | {EXPR ";"}+