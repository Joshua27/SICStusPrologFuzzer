:- multifile generate/2.
:- multifile shrink/3.

% Options:
% small, positive, negative, nozero, between(A,B) or none

generate(prob_value_integer(Options),int(Value)) :- 
    generate(integer(Options),Value).

shrink(prob_value_integer(_),int(Value),int(Shrunken)) :- 
    shrink(integer(_),Value,Shrunken).