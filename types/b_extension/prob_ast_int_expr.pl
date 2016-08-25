:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3,random_member/2]).

generate(prob_ast_int_expr,Value) :- 
    generate(prob_ast_int_expr([]),Value).

% Options:
% not-well-defined 
% id to enable random generation of identifier
% all options of any type are accepted and will be applied when valid

% Expr :: integer   
generate(prob_ast_int_expr(Options),Value) :-
    % double the chance of generating recursive expression
    % (i.e. add,minus,multiplication,div,unary_minus) to increase depth,
    % because modulo and power_of also just generate ground integer nodes for well-definedness
    random_member(Expr,[add,minus,multiplication,div,unary_minus,
                        modulo,power_of,max,min,max_int,min_int,
                        add,minus,multiplication,div,unary_minus]) , 
    generate(prob_ast_int_expr(Expr,Options),Value).

generate(prob_ast_int_expr(card,Options),b(card(Set),integer,[])) :-
    random_member(Type,[integer([]),string([]),boolean([])]) ,
    (member(id,Options)
    ->  generate(id_or_ast(set(Type)),Set)
    ;   generate(prob_ast_set(Type,Options),Set)). 

% also generate empty list for no well-definedness
generate(prob_ast_int_expr(Expr,Options),b(NewPred,integer,[])) :-
    member(not-well-defined,Options) , 
    member(Expr,[max,min]) ,
    random(0,9,R) ,
    (R > 3
    ->  generate(prob_ast_set(integer([small]),Options),Set)
    ;   generate(prob_ast_set(empty([]),Options),Set)) ,
    NewPred =.. [Expr,Set].
generate(prob_ast_int_expr(Expr,Options),b(NewPred,integer,[])) :- 
    member(Expr,[max,min]) ,
    generate(prob_ast_set(integer([small]),Options),Set) , % no expression 
    NewPred =.. [Expr,Set].

generate(prob_ast_int_expr(Expr,_),b(Expr,integer,[])) :- 
    member(Expr,[max_int,min_int]).

generate(prob_ast_int_expr(Expr,Options),b(NewPred,integer,[])) :- 
    member(Expr,[add,minus,multiplication]) ,
    Type = integer([small,random|Options]) ,
    generate_aux(Options,Type,Type,A,B) ,
    NewPred =.. [Expr,A,B]. 
    
generate(prob_ast_int_expr(Expr,Options),b(NewPred,integer,[])) :- 
    member(not-well-defined,Options) ,
    member(Expr,[modulo,power_of,div]) ,
    % generate expressions (not well defined for zero, negative values or float)
    TypeA = integer([small,random,not-well-defined|Options]) ,
    TypeB = integer([small,not-well-defined|Options]) , 
    generate_aux(Options,TypeA,TypeB,A,B) ,
    NewPred =.. [Expr,A,B].
% use really small numbers to receive well-definedness for
% integer expressions in predicates 
generate(prob_ast_int_expr(power_of,Options),b(NewPred,integer,[])) :- 
    Type = integer([between(0,7)|Options]) ,
    % disabled generation of identifier
    /*(member(id,Options) -> 
        generate(id_or_ast(Type),A) , 
        generate(id_or_ast(Type),B) 
    ;*/ gen_type(Type,ast,NType) ,
        generate(NType,A) ,
        generate(NType,B),
    NewPred =.. [power_of,A,B].
generate(prob_ast_int_expr(modulo,Options),b(NewPred,integer,[])) :- 
    TypeA = integer([small,positive|Options]) ,
    TypeB = integer([small,positive,nozero|Options]) ,
    generate_aux(Options,TypeA,TypeB,A,B) ,
    NewPred =.. [modulo,A,B].
generate(prob_ast_int_expr(div,Options),b(div(A,B),integer,[])) :- 
    TypeA = integer([small,random|Options]) ,
    TypeB = integer([small,positive,nozero|Options]) ,
    generate_aux(Options,TypeA,TypeB,A,B).    

generate(prob_ast_int_expr(unary_minus,Options),b(unary_minus(Expr),integer,[])) :- 
    Type = integer([small,random|Options]) ,
    (member(id,Options)
    ->  generate(id_or_ast(Type),Expr) 
    ;   gen_type(Type,ast,NType) ,
        generate(NType,Expr)).

generate(prob_ast_int_expr(size),b(size(Seq),integer,[])) :- 
    generate(ground_type,Type) ,
    generate(prob_ast_seq(Type),Seq).

% generate two nodes for given types and options
generate_aux(Options,TypeA,TypeB,NodeA,NodeB) :- 
    member(id,Options) , ! , 
    generate(id_or_ast(TypeA),NodeA) , 
    generate(id_or_ast(TypeB),NodeB).
generate_aux(_,TypeA,TypeB,NodeA,NodeB) :- 
    gen_type(TypeA,ast,NTypeA) , generate(NTypeA,NodeA) , 
    gen_type(TypeB,ast,NTypeB) , generate(NTypeB,NodeB).

shrink(Type,Expression,Shrunken) :- 
    Type =.. [prob_ast_int_expr|_] ,
    minimize_int_expr(Expression,Shrunken).
shrink(Type,Expression,Shrunken) :- 
    Type =.. [prob_ast_int_expr|_] ,
    get_inner_expr(Expression,Shrunken).