:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% all options of integer or float can be used

generate(number(Options),Value) :-
    Types = [integer(Options),float(Options)] ,
    random_member(Type,Types) ,
    generate(Type,Value).