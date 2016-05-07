:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% all options of any type can be used

% in practice a maximum recursive depth of two is enough for sets and sequences 
generate(prob_value_any(Options),Value) :-
    (member(noset,Options)
    ->  generate(ground_type(Options),Type)
    ;   random_member(Type,[integer(Options),string(Options),boolean(Options),
                            set(Options),set(set(Options)),seq(Options),seq(seq(Options))])) , 
    gen_type(Type,value,NType) , 
    generate(NType,Value).

shrink(Type,int(Value),int(Shrunken)) :- 
    Type =.. [prob_value_any|_] , 
    shrink(integer(_),Value,Shrunken).

shrink(Type,string(Value),string(Shrunken)) :- 
    Type =.. [prob_value_any|_] , 
    shrink(atom(_),Value,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_value_any|_] , 
    (is_list(Value) ; Value = avl_set(_)) , 
    shrink(prob_value_set,Value,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_value_any|_] , 
    shrink(prob_value_boolean(_),Value,Shrunken).