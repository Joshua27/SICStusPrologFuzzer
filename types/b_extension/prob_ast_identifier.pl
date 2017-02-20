:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists),[nth1/4, is_list/1]).
:- use_module(library(random)).

generate(prob_ast_identifier(Type),b(identifier(Name),Type,[])) :- 
    Type = record(_) , ! , 
    generate(atom([size:5,alph]),Name).

generate(prob_ast_identifier(Type),b(identifier(Name),NType,[])) :-  
    generate(atom([size:5,alph]),Name) , 
    inner_type(Type,Inner,Outter) , 
    generate_id_or_ast_aux2(Type,Inner,Outter,NType).

% either generate an identifier or an expression
generate(id_or_ast(Type),Value) :-  
    random(0,4,R) , 
    R =< 2 , ! , 
    % remove options for type of identifier
    % argument Type is like set(integer([small]))
    inner_type(Type,Inner,Outter) ,
    generate_id_or_ast_aux2(Type,Inner,Outter,NType) , 
    generate(prob_ast_identifier(NType),Value).
    
generate(id_or_ast(Type),Value) :- 
    inner_type(Type,Inner,Outter) , 
    generate_id_or_ast_aux(Type,Inner,Outter,NType) , 
    generate(NType,Value).

% generate constraints for a list of identifier nodes
generate(prob_ast_id_constraints(IDs),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDs,[]),ConstraintPred).

generate(prob_ast_id_constraints(b(Name,integer,Info),Options),ConstraintPred) :- 
    generate(prob_ast_set_expr(interval,Options),Interval) , 
    ConstraintPred = b(member(b(Name,integer,Info),Interval),pred,[]).

generate(prob_ast_id_constraints(b(Name,set(InnerType),Info),_),ConstraintPred) :- 
    random_member(Pred,[equal,less,greater,greater_equal,less_equal]) , 
    generate(prob_ast_integer([between(1,10)]),Integer) , 
    Identifier = b(Name,set(InnerType),Info) , 
    NewNode =.. [Pred,b(card(Identifier),integer,[]),Integer] , 
    get_type_constraint(Identifier,InnerType,TypeConstraint) , 
    ConstraintPred = b(conjunct(b(NewNode,pred,[]),TypeConstraint),pred,[]).

generate(prob_ast_id_constraints(b(Name,seq(InnerType),Info),_),ConstraintPred) :- 
    random_member(Pred,[equal,less,greater,greater_equal,less_equal]) , 
    generate(prob_ast_integer([between(1,10)]),Integer) , 
    NewNode =.. [Pred,b(size(b(Name,seq(InnerType),Info)),integer,[]),Integer] , 
    ConstraintPred = b(NewNode,pred,[]).

generate(prob_ast_id_constraints([IDNode],Options),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDNode,Options),ConstraintPred).

generate(prob_ast_id_constraints([IDNode1,IDNode2|R],Options),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDNode1,Options),Constraint1) , 
    generate(prob_ast_id_constraints(IDNode2,Options),Constraint2) , 
    generate_identifier_constraint_aux(Constraint1,Constraint2,R,Options,ConstraintPred).
% skip boolean, string
generate(prob_ast_id_constraints(_,_),b(truth,pred,[])).

generate_identifier_constraint_aux(Constraint1,Constraint2,R,Options,ConstraintPred) :- 
    R \= [] , 
    generate(prob_ast_id_constraints(R,Options),RConstraints) , 
    ConstraintPred = b(conjunct(b(conjunct(Constraint1,Constraint2),pred,[]),RConstraints),pred,[]).
generate_identifier_constraint_aux(Constraint1,Constraint2,[],_,ConstraintPred) :- 
    ConstraintPred = b(conjunct(Constraint1,Constraint2),pred,[]).

get_type_constraint(Identifier,InnerType,TypeConstraint) :- 
    get_set_type(InnerType,SetType) , ! , 
    TypeConstraint = b(subset(Identifier,SetType),pred,[]).
get_type_constraint(_,_,b(truth,pred,[])).

get_set_type(InnerType,SetType) :- 
    ground(InnerType) , ! , 
    get_set_type_aux(0,InnerType,SetType).
get_set_type(_,SetType) :- 
    random(0,3,R) , 
    get_set_type_aux(R,_,SetType).
get_set_type_aux(0,integer,b(integer_set('INTEGER'),set(integer),[])).
get_set_type_aux(1,integer,b(integer_set('NATURAL'),set(integer),[])).
get_set_type_aux(2,integer,b(integer_set('NATURAL1'),set(integer),[])).
get_set_type_aux_(set(Type),b(pow_subset(Node),set(Type),[])) :- 
    get_set_type(Type,Node).

generate_id_or_ast_aux(_,InnerType,OutterType,NType) :- 
    \+is_list(InnerType) , 
    NewInner =.. [InnerType,[]] , 
    surround_type(NewInner,OutterType,Temp) , 
    gen_type(Temp,ast,NType).
generate_id_or_ast_aux(Type,InnerType,_,NType) :- 
    is_list(InnerType) , 
    gen_type(Type,ast,NType).

generate_id_or_ast_aux2(_,InnerType,OutterType,NType) :- 
    is_list(InnerType) , 
    length(OutterType,L) , 
    nth1(L,OutterType,NewInner,NewOutter) , 
    surround_type(NewInner,NewOutter,NType) .
generate_id_or_ast_aux2(_,InnerType,OutterType,NType) :- 
    compound(InnerType) , 
    % split inner to get rid of options
    InnerType =.. [NewInner|_] , 
    surround_type(NewInner,OutterType,NType).
generate_id_or_ast_aux2(Type,InnerType,_,Type) :- 
    \+is_list(InnerType).

% don't shrink identifier
shrink(prob_ast_identifier(_),Value,Value).