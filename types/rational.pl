:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

% Options:
% positive, negative

generate(rational(Options),Value) :-
    member(positive,Options) ,
    delete(Options,positive,NOptions) ,
    random(0,2,R) ,
    generate_rational_aux(positive,NOptions,R,Z,N) ,
    Value = Z/N.

generate(rational(Options),Value) :-
    member(negative,Options) ,
    delete(Options,negative,NOptions) ,
    random(0,2,R) ,
    generate_rational_aux(negative,NOptions,R,Z,N) ,
    Value = Z/N.

generate(rational(Options),Value) :- 
    random_member(Pol,[positive,negative]) ,
    generate(rational([Pol|Options]),Value).

generate_rational_aux(positive,Options,0,Z,N) :- 
    generate(integer([positive|Options]),Z) ,
    generate(integer([positive|Options]),N) ,
    N \= 0.
generate_rational_aux(positive,Options,1,Z,N) :- 
    generate(integer([negative|Options]),Z) ,
    generate(integer([negative|Options]),N) ,
    N \= 0.
generate_rational_aux(negative,Options,0,Z,N) :- 
    generate(integer([positive|Options]),Z) ,
    generate(integer([negative|Options]),N) ,
    N \= 0.
generate_rational_aux(negative,Options,1,Z,N) :- 
    generate(integer([negative|Options]),Z) ,
    generate(integer([positive|Options]),N) ,
    N \= 0.

shrink(rational(_),Val1/Val2,ShrunkenVal1/ShrunkenVal2) :- 
    shrink(integer([]),Val1,ShrunkenVal1) , 
    shrink(integer([]),Val2,ShrunkenVal2) , 
    ShrunkenVal2 \= 0.

shrink(rational(_),Value,Value).