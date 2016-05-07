:- multifile generate/2.
:- multifile shrink/3.

% fixed value with no random generation or shrinking
generate(fixed(Value),Value).

shrink(fixed(Value),Value,Value).