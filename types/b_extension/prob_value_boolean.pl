:- multifile generate/2.
:- multifile shrink/3.

% no options for boolean
generate(prob_value_boolean(_),Boolean) :-
    random_member(Boolean,[pred_true,pred_false]).

shrink(prob_value_boolean(_),Boolean,Boolean).