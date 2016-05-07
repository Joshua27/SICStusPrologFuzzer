:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

% Options:
% positive, negative

generate(rational(Options),Value) :-
    member(positive,Options) ,
    delete(Options,positive,NOptions) ,
    random(0,2,R) ,
    (R = 0 ->
        generate(integer([positive|NOptions]),Z) ,
        generate(integer([positive|NOptions]),N) ,
        N \= 0
    ;   generate(integer([negative|NOptions]),Z) ,
        generate(integer([negative|NOptions]),N) ,
        N \= 0) ,
    Value = Z/N.

generate(rational(Options),Value) :-
    member(negative,Options) ,
    delete(Options,negative,NOptions) ,
    random(0,2,R) ,
    (R = 0 ->
        generate(integer([positive|NOptions]),Z) ,
        generate(integer([negative|NOptions]),N) ,
        N \= 0
    ;   generate(integer([negative|NOptions]),Z) ,
        generate(integer([positive|NOptions]),N) ,
        N \= 0) ,
    Value = Z/N.

generate(rational(Options),Value) :- 
    random_member(Pol,[positive,negative]) ,
    generate(rational([Pol|Options]),Value).

shrink(rational(_),Val1/Val2,ShrunkenVal1/ShrunkenVal2) :- 
    shrink(integer([]),Val1,ShrunkenVal1) , 
    shrink(integer([]),Val2,ShrunkenVal2) , 
    ShrunkenVal2 \= 0.

shrink(rational(_),Value,Value).