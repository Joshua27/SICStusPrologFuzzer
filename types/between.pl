:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

generate(between(A,B),Value) :-
    C is B + 1 ,
    random(A,C,Value).

shrink(between(A,B),Value,Shrunken) :- 
    shrink(integer(_),Value,Shrunken) , 
    Shrunken >= A , Shrunken =< B.