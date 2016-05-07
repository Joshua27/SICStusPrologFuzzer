:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

generate(boolean(_),Value) :-
    random_member(Value,[true,false]).

shrink(boolean(_),Boolean,Boolean).