:- multifile generate/2.
:- multifile shrink/3.

% same options as atom

generate(prob_ast_string(Options),String) :-
    generate(atom(Options),Atom) , 
    String = b(string(Atom),string,[]).

shrink(prob_ast_string(_),b(string(Atom),string,Info),Shrunken) :- 
    shrink(atom([]),Atom,ShrunkenValue) , 
    Shrunken = b(string(ShrunkenValue),string,Info).