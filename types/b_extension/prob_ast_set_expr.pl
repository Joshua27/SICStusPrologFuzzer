:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

generate(prob_ast_set_expr,Value) :- 
    generate(prob_ast_set_expr([]),Value).

% Options:
% id to enable random generation of identifier
% all expressions are well-defined unless option not-well-defined-values is given

% Expr :: set(_)
generate(prob_ast_set_expr(Options),Value) :- 
    is_list(Options) , 
    random_member(Expr,[interval,union,intersection,set_subtraction,bool_set,string_set,
                        general_union,general_intersection,empty_set]) ,
    generate(prob_ast_set_expr(Expr,Options),Value).

generate(prob_ast_set_expr(interval,Options),b(Interval,set(integer),[])) :-
    ( member(posInterval,Options)
    ->  generate(prob_ast_integer([between(0,15)|Options]),Node1)
    ; member(negInterval,Options)
    ->  generate(prob_ast_integer([between(-15,0)|Options]),Node1)  
    ;   generate(prob_ast_integer([between(-15,15)|Options]),Node1)) , 
    Node1 = b(integer(Value1),_,_) , 
    (member(maxInterval:S,Options)
    ->  % interval with given maximum size
        random(0,S,R) , 
        Value2 is Value1 + R , 
        Node2 = b(integer(Value2),integer,[])
    ;   generate(prob_ast_integer([small|Options]),Node2) , 
        Node2 = b(integer(Value2),_,_)) ,
    % get value from ast node
    (Value1 =< Value2
    ->  Interval = interval(Node1,Node2)
    ;   Interval = interval(Node2,Node1)).

generate(prob_ast_set_expr(empty_set,_),b(set_extension([]),set(empty),[])).

generate(prob_ast_set_expr(bool_set,Options),Value) :-
    (member(id,Options)
    ->  generate(id_or_ast(set(boolean([]))),Value)
    ;   generate(prob_ast_set(boolean([]),Options),Value)).

generate(prob_ast_set_expr(string_set,Options),Value) :- 
    (member(id,Options)
    ->  generate(id_or_ast(set(string([]))),Value)
    ;   generate(prob_ast_set(string([]),Options),Value)).

generate(prob_ast_set_expr(Expr,Options),b(NewPred,set(Type),[])) :-  
    member(Expr,[union,intersection,set_subtraction]) ,
    generate(prob_ast_set(Options),Set1) , 
    Set1 = b(_,set(Type),_) , 
    inner_type(Type,Inner,Outter) ,
    NewInner =.. [Inner,[]] , % no options
    surround_type(NewInner,Outter,NType) , ! ,
    (member(id,Options) , Type \= empty ->
        generate(id_or_ast(set(NType)),Set2) 
    ;   generate(prob_ast_set(NType,Options),Set2)) ,
    NewPred =.. [Expr,Set1,Set2].

generate(prob_ast_set_expr(Expr,Options),b(NewPred,Type,[])) :-  
    member(Expr,[general_union,general_intersection]) ,
    generate(ground_type,SetType) ,
    (member(id,Options)
    ->  generate(id_or_ast(set(SetType)),Set)
    ;   generate(prob_ast_set(SetType,Options),Set)) ,
    Set = b(_,Type,_) ,
    SetOfSet = b(set_extension([Set]),set(Type),[]) ,
    NewPred =.. [Expr,SetOfSet].

shrink(Type,Expression,Shrunken) :- 
    Type =.. [prob_ast_set_expr|_] ,
    minimize_set_expr(Expression,Shrunken).

shrink(Type,Value,Shrunken) :-
    Type =.. [prob_ast_set_expr|_] , 
    % defined in prob_ast_minimize_int_expr.pl
    get_inner_expr(Value,Shrunken).