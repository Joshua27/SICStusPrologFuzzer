:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

% Options:
% size:Length, alph only a-z

generate(atom(Options),Value) :-
    % given or random size
    (member(size:Size,Options)
    ->  Size >= 0
    ;   random(1,20,Size)) , 
    % only alphabetic with no capital letters, else random chars with special signs
    (member(alph,Options)
    ->  generate(list(between(97,122),[size:Size]),Codes)
    ;   generate(list(between(33,127),[size:Size]),Codes)) ,
    atom_codes(Value,Codes).

shrink(atom(_),Value,Shrunken) :- 
    atom_codes(Value,CodeList) ,
    shrink(list,CodeList,ShrunkenList) ,  
    ShrunkenList \= [] , 
    atom_codes(Shrunken,ShrunkenList).     