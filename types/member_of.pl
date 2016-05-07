:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% random member of a list of values
generate(member(L),Value) :-
    random_member(Value,L).