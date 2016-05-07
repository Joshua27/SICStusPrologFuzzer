:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists),[nth1/4]).

generate(prob_ast_identifier(Type),b(identifier(Name),Type,[])) :- 
    Type = record(_) , ! , 
    generate(atom([size:5,alph]),Name).

generate(prob_ast_identifier(Type),b(identifier(Name),NType,[])) :-  
    generate(atom([size:5,alph]),Name) , 
    inner_type(Type,Inner,Outter) , 
    ( is_list(Inner)
    ->  % drop options    
        length(Outter,L) , 
        nth1(L,Outter,NewInner,NewOutter) , 
        surround_type(NewInner,NewOutter,NType)
    ; compound(Inner)
    ->  % split inner to get rid of options
        Inner =.. [NewInner|_] , 
        NewOutter = Outter , 
        surround_type(NewInner,NewOutter,NType)
    ;   NType = Type).

% either generate an identifier or an expression
generate(id_or_ast(Type),Value) :-  
    random(0,4,R) , 
    R =< 2 , ! , 
    % remove options for type of identifier
    % argument Type is like set(integer([small]))
    inner_type(Type,Inner,Outter) ,
    (is_list(Inner)
    ->  length(Outter,L) , 
        nth1(L,Outter,NewInner,NewOutter) , 
        surround_type(NewInner,NewOutter,NType) 
    ;   NType = Type) , 
    generate(prob_ast_identifier(NType),Value).
    
generate(id_or_ast(Type),Value) :- 
    inner_type(Type,Inner,Outter) , 
    (\+is_list(Inner)
    ->  NewInner =.. [Inner,[]] , 
        surround_type(NewInner,Outter,Temp) , 
        gen_type(Temp,ast,NType) 
    ;   gen_type(Type,ast,NType)) , 
    generate(NType,Value).

% generate constraints for a list of identifier nodes

generate(prob_ast_id_constraints(IDs),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDs,[]),ConstraintPred).

generate(prob_ast_id_constraints(b(Name,integer,Info),Options),ConstraintPred) :- 
    generate(prob_ast_set_expr(interval,Options),Interval) , 
    ConstraintPred = b(member(b(Name,integer,Info),Interval),pred,[]).

generate(prob_ast_id_constraints(b(Name,set(Type),Info),_),ConstraintPred) :- 
    random_member(Pred,[equal,less,greater,greater_equal,less_equal]) , 
    generate(prob_ast_integer([between(1,10)]),Integer) , 
    NewNode =.. [Pred,b(card(b(Name,set(Type),Info)),integer,[]),Integer] , 
    ConstraintPred = b(NewNode,pred,[]).

generate(prob_ast_id_constraints(b(Name,seq(Type),Info),_),ConstraintPred) :- 
    random_member(Pred,[equal,less,greater,greater_equal,less_equal]) , 
    generate(prob_ast_integer([between(1,10)]),Integer) , 
    NewNode =.. [Pred,b(size(b(Name,seq(Type),Info)),integer,[]),Integer] , 
    ConstraintPred = b(NewNode,pred,[]).

generate(prob_ast_id_constraints([IDNode],Options),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDNode,Options),ConstraintPred).

generate(prob_ast_id_constraints([IDNode1,IDNode2|R],Options),ConstraintPred) :- 
    generate(prob_ast_id_constraints(IDNode1,Options),Constraint1) , 
    generate(prob_ast_id_constraints(IDNode2,Options),Constraint2) , 
    (R \= []
    ->  generate(prob_ast_id_constraints(R,Options),RConstraints) , 
        ConstraintPred = b(conjunct(b(conjunct(Constraint1,Constraint2),pred,[]),RConstraints),pred,[])
    ;   ConstraintPred = b(conjunct(Constraint1,Constraint2),pred,[])).
% skip boolean, string
generate(prob_ast_id_constraints(_,_),b(truth,pred,[])).

% don't shrink identifier
shrink(prob_ast_identifier(_),Value,Value).