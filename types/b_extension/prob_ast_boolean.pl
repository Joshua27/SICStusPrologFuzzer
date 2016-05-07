:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

generate(prob_ast_boolean(_),Value) :-
    random_member(Val,[boolean_true,boolean_false]) ,
    Value = b(Val,boolean,[]).

shrink(prob_ast_boolean(_),Boolean,Boolean). 