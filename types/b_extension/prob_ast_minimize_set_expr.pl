% minimize ProB AST set expressions for shrinking stage

:- use_module(library(sets)).
:- use_module(library(lists),[is_list/1]).
:- use_module(library(avl),[avl_to_list/2,list_to_avl/2]).

minimize_set_expr(b(set_extension(Set),set(Type),Info),b(set_extension(ShrunkenSet),set(Type),Info)) :- 
    is_list(Set) , Set \= [b(_,set(_),_)] ,
    shrink(list(_),Set,ShrunkenSet).

% minimize set of set
minimize_set_expr(b(set_extension([Set]),set(Type),Info),Shrunken) :-
    minimize_set_expr(Set,ShrunkenSet) , 
    (ShrunkenSet = b(value(avl_set(empty)),set(empty),Info)
    ->  Shrunken = ShrunkenSet
    ;   Shrunken = b(set_extension([ShrunkenSet]),set(Type),Info)).

minimize_set_expr(b(set_extension([]),set(empty),Info),b(set_extension([]),set(empty),Info)).

minimize_set_expr(b(value(Set),set(Type),Info),Shrunken) :- 
    shrink(prob_value_set,Set,ShrunkenSet) , 
    (ShrunkenSet = []
    ->  Shrunken = b(value(avl_set(empty)),set(empty),Info)
    ;   Shrunken = b(value(ShrunkenSet),set(Type),Info)).

% shrink both sides of interval down to one integer 
minimize_set_expr(b(interval(b(integer(Value1),integer,Info1),b(integer(Value2),integer,Info2)),set(integer),Info),Shrunken) :- 
    NewValue1 is Value1 + 1 , 
    NewValue2 is Value2 - 1 , 
    (NewValue1 >= NewValue2
    ->  Shrunken = b(set_extension([b(integer(NewValue1),integer,Info1)]),set(integer),Info)
    ;   Shrunken = b(interval(b(integer(NewValue1),integer,Info1),b(integer(NewValue2),integer,Info2)),set(integer),Info)).

% union, intersection, set_subtraction
% shrink expression to its value when both arguments are set expressions
minimize_set_expr(b(Expression,set(SetType),Info),Shrunken) :- 
    Expression =.. [Type,Arg1,Arg2] ,
    member(Type,[union,intersection,set_subtraction]) ,
    prob_is_ground(Arg1,true) , 
    prob_is_ground(Arg2,true) ,
    sint_avl(b(Expression,set(SetType),Info),NewSet) ,
    (NewSet = avl_set(empty)
    ->  Shrunken = b(value(avl_set(empty)),set(empty),Info)
    ;   Shrunken = b(value(NewSet),set(SetType),Info)).
minimize_set_expr(b(Expression,set(SetType),Info),Shrunken) :- 
    Expression =.. [Type,Arg1,Arg2] ,
    member(Type,[union,intersection,set_subtraction]) ,
    prob_is_ground(Arg1,Res1) , 
    prob_is_ground(Arg2,Res2) ,
    % hold set and minimize expression
    ( Res1 = true , Res2 = false
    ->  minimize_set_expr(Arg2,NewArg2) ,
        NewExpr =.. [Type,Arg1,NewArg2] ,
        Shrunken = b(NewExpr,set(SetType),Info)
    ; Res1 = false , Res2 = true
    ->  minimize_set_expr(Arg1,NewArg1) ,
        NewExpr =.. [Type,NewArg1,Arg2] ,
        Shrunken = b(NewExpr,set(SetType),Info)
    ;   % minimize both expressions
        minimize_set_expr(Arg1,NewArg1) ,
        minimize_set_expr(Arg2,NewArg2) ,
        NewExpr =.. [Type,NewArg1,NewArg2] ,
        Shrunken = b(NewExpr,set(SetType),Info)).

% general_union,  general_intersection
minimize_set_expr(b(Expression,set(SetType),Info),Shrunken) :- 
    Expression =.. [Type,Arg] ,
    Arg = b(set_extension([Set]),_,_) ,
    member(Type,[general_union,general_intersection]) ,
    prob_is_ground(Set,Res) ,
    (Res = true
    ->  sint_avl(b(Expression,set(SetType),Info),NewSet) ,
        % don't shrink to an empty set
        (NewSet = avl_set(empty)
        ->  Shrunken = b(Expression,set(SetType),Info)
        ;   Shrunken = b(value(NewSet),set(SetType),Info))
    ;   minimize_set_expr(Arg,NewArg) , 
        NewExpr =.. [Type,NewArg] , 
        Shrunken = b(NewExpr,set(SetType),Info)).

% skip identifier
minimize_set_expr(b(identifier(Name),Type,Info),b(identifier(Name),Type,Info)).