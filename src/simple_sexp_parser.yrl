Nonterminals sexp list tuple elements.

Terminals string number symbol '(' ')' '[' ']' cons.

Rootsymbol sexp.

sexp -> string : element(3, '$1').
sexp -> number : element(3, '$1').
sexp -> symbol : element(3, '$1').

sexp -> list : '$1'.
list -> '(' ')' : [].
list -> '(' elements ')' : '$2'.

sexp -> tuple : '$1'.
tuple -> '[' ']' : {}.
tuple -> '[' elements ']' : list_to_tuple('$2').

elements -> sexp : ['$1'].
elements -> sexp elements : ['$1' | '$2'].

sexp -> '(' sexp cons sexp ')' : ['$2' | '$4'].
