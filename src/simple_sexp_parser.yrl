Nonterminals sexp list tuple elements element.

Terminals string number symbol '(' ')' '[' ']' cons.

Rootsymbol sexp.

sexp -> string : '$1'.
sexp -> number : '$1'.
sexp -> symbol : '$1'.

sexp -> list : '$1'.
list -> '(' ')' : [].
list -> '(' elements ')' : '$2'.

sexp -> tuple : '$1'.
tuple -> '[' ']' : {}.
tuple -> '[' elements ']' : list_to_tuple('$2').

elements -> sexp : ['$1'].
elements -> sexp elements : ['$1' | '$2'].
