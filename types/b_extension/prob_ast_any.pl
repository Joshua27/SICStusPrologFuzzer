:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% all options of any given type can be used for generation

generate(prob_ast_any(Options),Value) :- 
    generate(ground_type,GroundType) ,
    (member(noset,Options)
    ->  generate(ground_type(Options),Type)
    ;   random_member(Type,[integer(Options),string(Options),boolean(Options),
                            set([any|Options]),seq([any|Options]),           % any set/seq or expression
                            set(set(GroundType)),seq(seq(GroundType))])) ,   % max depth is 2 by now
    gen_type(Type,ast,NType) , 
    generate(NType,Value).
    
% generate random type for an ast leave
generate(ground_type,Type) :- 
    random_member(Type,[integer([]),string([]),boolean([])]).
generate(ground_type(Options),Type) :- 
    random_member(Type,[integer(Options),string(Options),boolean(Options)]).