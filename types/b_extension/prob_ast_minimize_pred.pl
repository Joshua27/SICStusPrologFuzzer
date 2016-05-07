% minimize ProB AST predicates for shrinking stage

:- use_module(library(lists)).
:- use_module(library(random),[random_member/2]).

minimize_pred(b(truth,pred,Info),b(truth,pred,Info)).

minimize_pred(b(falsity,pred,Info),b(falsity,pred,Info)).

% one argument predicates
minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [Type,Arg] , 
    member(Type,[negation,finite]) ,
    prob_is_ground(Arg,Res) , 
    % if argument is ground interpret predicate
    (Res = true
    ->  pint(b(Pred,pred,Info),Value) , 
        (Value = true
        ->  Shrunken = b(truth,pred,Info)
        ;   Shrunken = b(falsity,pred,Info))
    ;   (Type = negation
        ->  minimize_pred(Arg,NewArg)
        ;   minimize_set_expr(Arg,NewArg)) , 
        NewPred =.. [Type,NewArg] , 
        Shrunken = b(NewPred,pred,Info)).

minimize_pred(b(exists(IDs,InnerPred),pred,Info),b(exists(IDs,NewInner),pred,Info)) :- 
    minimize_pred(InnerPred,NewInner).

minimize_pred(b(forall(IDs,Constraints,InnerPred),pred,Info),b(forall(IDs,NewConstraints,NewInner),pred,Info)) :- 
    minimize_pred(InnerPred,NewInner) , 
    minimize_constraint_pred(Constraints,NewConstraints).

% two argument predicates
minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [Type,Arg1,Arg2] ,
    member(Type,[conjunct,disjunct,implication,equivalence,equal,not_equal]) ,
    prob_is_ground(Arg1,true) , 
    prob_is_ground(Arg2,true) , 
    pint(b(Pred,pred,Info),Value) ,
    (Value = true
    ->  Shrunken = b(truth,pred,Info)
    ;   Shrunken = b(falsity,pred,Info)).
minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [Type,Arg1,Arg2] ,
    member(Type,[conjunct,disjunct,implication,equivalence,equal,not_equal]) ,
    prob_is_ground(Arg1,Res1) ,
    prob_is_ground(Arg2,Res2) ,
    % hold boolean value and minimize predicate
    (Res1 = true , Res2 = false
    ->  minimize_pred(Arg2,NewArg2) ,
        NewExpr =.. [Type,Arg1,NewArg2] ,
        Shrunken = b(NewExpr,pred,Info)
    ;   Res1 = false , Res2 = true
    ->  minimize_pred(Arg1,NewArg1) ,
        NewExpr =.. [Type,NewArg1,Arg2] ,
        Shrunken = b(NewExpr,pred,Info)
    ;   % minimize both expressions
        minimize_pred(Arg1,NewArg1) ,
        minimize_pred(Arg2,NewArg2) ,
        NewExpr =.. [Type,NewArg1,NewArg2] ,
        Shrunken = b(NewExpr,pred,Info)).

% member, not_member
minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [Type,_,_] , 
    member(Type,[member,not_member]) , 
    pint(b(Pred,pred,Info),Value) ,
    (Value = true
    ->  Shrunken = b(truth,pred,Info)
    ;   Shrunken = b(falsity,pred,Info)).

% skip identifier
minimize_pred(b(identifier(Name),Type,Info),b(identifier(Name),Type,Info)).

minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [_,Arg1,Arg2] ,
    prob_is_ground(Arg1,true) , 
    prob_is_ground(Arg2,true) , 
    pint(b(Pred,pred,Info),Value) ,
    (Value = true
    ->  Shrunken = b(truth,pred,Info)
    ;   Shrunken = b(falsity,pred,Info)).
minimize_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [Type,Expr1,Expr2] ,
    prob_is_ground(Expr1,Res1) , prob_is_ground(Expr2,Res2) ,
    % hold boolean value and minimize integer expressions
    ( Res1 = true , Res2 = false
    ->  (shrink(prob_ast_expr(_),Expr2,NewArg2) ; shrink(prob_ast_pred(_),Expr2,NewArg2)) , 
        NewExpr =.. [Type,Expr1,NewArg2] ,
        Shrunken = b(NewExpr,pred,Info)
    ; Res1 = false , Res2 = true
    ->  (shrink(prob_ast_expr(_),Expr1,NewArg1) ; shrink(prob_ast_pred(_),Expr1,NewArg1)) , 
        NewExpr =.. [Type,NewArg1,Expr2] ,
        Shrunken = b(NewExpr,pred,Info)
    ;   % minimize both expressions
        (shrink(prob_ast_expr(_),Expr1,NewArg1) ; shrink(prob_ast_pred(_),Expr1,NewArg1)) , 
        (shrink(prob_ast_expr(_),Expr2,NewArg2) ; shrink(prob_ast_pred(_),Expr2,NewArg2)) , 
        NewExpr =.. [Type,NewArg1,NewArg2] ,
        Shrunken = b(NewExpr,pred,Info)).

% get one bottom predicate from an ast

% return exists or forall nodes, it's not reasonable to get their
% inner predicates because they need their IDs and Constraints
get_inner_pred(b(exists(ListOfIDs,Pred),pred,Info),b(exists(ListOfIDs,NewPred),pred,Info)) :- 
    minimize_pred(Pred,NewPred).
get_inner_pred(b(forall(ListOfIDs,IDConstraints,Pred),pred,Info),b(forall(ListOfIDs,IDConstraints,NewPred),pred,Info)) :- 
    minimize_pred(Pred,NewPred).

% two argument predicates
get_inner_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [_,Arg1,Arg2] , 
    (prob_is_ground(Arg1,Res1) ; Arg1 = b(_,integer,_)) , % only predicates
    (prob_is_ground(Arg2,Res2) ; Arg2 = b(_,integer,_)) ,
    ( Res1 = true , Res2 = true ->
      % done
        Shrunken = b(Pred,pred,Info) 
    ; % get inner predicate of second argument
     Res1 = true , Res2 = false
    ->  get_inner_pred(Arg2,Shrunken)
    ; % both arguments are predicates
     Res1 = false , Res2 = false
    ->  % random choice heuristic
        random_member(Argument,[Arg1,Arg2]) ,
        get_inner_pred(Argument,Shrunken)
    ; % get inner predicate of first argument
        get_inner_pred(Arg1,Shrunken)).
% one argument predicates
get_inner_pred(b(Pred,pred,Info),Shrunken) :- 
    Pred =.. [_,Arg] , 
    (prob_is_ground(Arg,Res) ; Arg = b(_,integer,_)) ,
    (Res = true
    ->  Shrunken = b(Pred,pred,Info)
    ;   get_inner_pred(Arg,Shrunken)).

get_inner_pred(b(Pred,pred,Info),b(Pred,pred,Info)).

% addition to minimize constraints from forall node
% minimize domain from each side
minimize_constraint_int(b(Expr,pred,Info),b(NewExpr,pred,Info)) :- 
    Expr =.. [Type,IDNode,b(integer(Value),integer,IntInfo)] , 
    member(Type,[less,less_equal]) ,  
    NewValue is Value - 1 , 
    NewExpr =.. [Type,IDNode,b(integer(NewValue),integer,IntInfo)].
minimize_constraint_int(b(Expr,pred,Info),b(NewExpr,pred,Info)) :- 
    Expr =.. [Type,IDNode,b(integer(Value),integer,IntInfo)] , 
    member(Type,[greater,greater_equal]) , 
    NewValue is Value + 1 , 
    NewExpr =.. [Type,IDNode,b(integer(NewValue),integer,IntInfo)].
minimize_constraint_int(b(equal(IDNode,Constraint),pred,Info),b(equal(IDNode,Constraint),pred,Info)).

% minimize cardinality of set constraints
minimize_constraint_pred(b(equal(Card,b(integer(Val),integer,[])),pred,Info),_,b(equal(Card,b(integer(NewVal),integer,[])),pred,Info)) :- 
    (Val = 0 -> NewVal = Val ; NewVal is Val - 1).
minimize_constraint_pred(Constraint,_,Constraint) :- 
    Constraint = b(member(_),_,_) , !.
% minimize constraints given as predicates less, less_equal, greater, greater_equal 
% surrounded by conjunct 
minimize_constraint_pred(b(conjunct(Constraint1,Constraint2),pred,Info),b(NewPred,pred,Info)) :- 
    minimize_constraint_int(Constraint1,NewConstraint1) , 
    minimize_constraint_int(Constraint2,NewConstraint2) , 
    % get identifier and integer node from the restricting predicate
    NewConstraint1 = b(ConstrPred1,_,_) , 
    ConstrPred1 =.. [_Pred1,IDNode,Val1] , 
    NewConstraint2 = b(ConstrPred2,_,_) , 
    ConstrPred2 =.. [_Pred2,IDNode,Val2] , 
    % check if identifier node is still in use 
    %(member(IDNode,UsedIDs) -> 
        % return an equality constraint if domain is down to one value
        (Val1 == Val2
        ->  NewPred = equal(IDNode,Val1)
        ;   NewPred = conjunct(NewConstraint1,NewConstraint2)).
    %;   NewPred = truth).
minimize_constraint_pred(b(conjunct(Constraint1,Constraint2),pred,Info),b(conjunct(NewConstraint1,NewConstraint2),pred,Info)) :- 
    minimize_constraint_pred(Constraint1,NewConstraint1) , 
    minimize_constraint_pred(Constraint2,NewConstraint2).
% skip sequence constraints
minimize_constraint_pred(b(truth,pred,[]),b(truth,pred,[])) :- !.