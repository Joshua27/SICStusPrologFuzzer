:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

% same options as atom

generate(prob_value_string(Options),string(Value)) :-
    generate(atom(Options),Value).

shrink(prob_value_string(_),string(Value),string(Shrunken)) :-
    shrink(atom(_),Value,Shrunken).
