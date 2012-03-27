Nonterminals E uminus.
Terminals '+' '-' '*' '/' '(' ')' integer float.
Rootsymbol E.
Left 100 '+'.
Left 100 '-'.
Left 200 '*'.
Left 200 '/'.
Unary 300 uminus.
E -> uminus : '$1'.
E -> E '+' E : ['$2', '$1', '$3'].
E -> E '*' E : ['$2', '$1', '$3'].
E -> E '-' E : ['$2', '$1', '$3'].
E -> E '/' E : ['$2', '$1', '$3'].
E -> '(' E ')' : '$2'.
E -> integer : '$1'.
E -> float : '$1'.
uminus -> '-' E : ['$1', '$2'].
