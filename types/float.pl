:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/1]).

% Options:
%   small, positive, negative, nozero

generate(float(Options),Value) :- 
    generate(integer(Options),Integer) ,
    random(R),
    Value is Integer*R.

shrink(float(_),Value,Shrunken) :- 
    shrink(integer(_),Value,Shrunken).