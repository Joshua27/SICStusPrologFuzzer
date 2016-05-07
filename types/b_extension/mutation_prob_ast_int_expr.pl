:- multifile generate/2.

% shrinking of all mutations defined in mutation.pl

:- use_module(library(lists),[is_list/1,maplist/2]).
:- use_module(library(random),[random_permutation/2,random_member/2,random/3]).
:- use_module(library(clpfd)).

% ProB ast integer expressions
generate(mutation(Expression:prob_ast_int_expr),NewExpression) :- 
    \+is_list(Expression) , 
    random_int_expr_mutation(Expression,NewExpression).   
% mutation of a list of integer expressions
generate(mutation(Expressions:prob_ast_int_expr),NewExpression) :- 
    % well-definedness problem with concatenation using div and modulo, division by zero
    % power_of also deprecated because of integer overflow
    concatenate_ast(Expressions,[add,minus,multiplication],Expression) , 
    random_int_expr_mutation(Expression,NewExpression).   

% randomly replaces ground integer ast nodes with a matching 
% integer expression

% leaves
random_int_expr_mutation(Node,Mutation) :- 
    prob_is_ground(Node,true) ,
    replace_ground_with_expr(Node,Mutation).

% two argument expressions
random_int_expr_mutation(b(Expression,integer,Info),b(NewExpression,integer,Info)) :- 
    Expression =.. [Type,Expr1,Expr2] , 
    member(Type,[add,minus,multiplication,div,modulo,power_of]) , 
    % random choice heuristic for argument
    random(0,3,R) ,
    % if expression is ground replace its value by an arithmetic expression 
    ( R = 0
    ->  (prob_is_ground(Expr1,true)
        ->  replace_ground_with_expr(Expr1,NewExpr1)
        ;   random_int_expr_mutation(Expr1,NewExpr1)) ,
        NewExpr2 = Expr2
    ; R = 1
    ->  (prob_is_ground(Expr2,true)
        ->  replace_ground_with_expr(Expr2,NewExpr2)
        ;   random_int_expr_mutation(Expr2,NewExpr2)) ,
        NewExpr1 = Expr1
    ;   % try replacing both arguments with expressions
        (prob_is_ground(Expr1,true)
        ->  replace_ground_with_expr(Expr1,NewExpr1)
        ;   random_int_expr_mutation(Expr1,NewExpr1)) ,
        (prob_is_ground(Expr2,true)
        ->  replace_ground_with_expr(Expr2,NewExpr2)
        ;   random_int_expr_mutation(Expr2,NewExpr2))) , 
    NewExpression =.. [Type,NewExpr1,NewExpr2].
random_int_expr_mutation(b(Expression,integer,Info),b(NewExpression,integer,Info)) :- 
    Expression =.. [Type,Expr1,Expr2] , 
    random_int_expr_mutation(Expr1,NewExpr1) , 
    random_int_expr_mutation(Expr2,NewExpr2) , 
    NewExpression =.. [Type,NewExpr1,NewExpr2].

% one argument expressions
random_int_expr_mutation(b(Expression,integer,Info),b(NewExpression,integer,Info)) :- 
    Expression =.. [Type,Expr] , 
    \+member(Type,[max,min]) ,
    (prob_is_ground(Expr,true)
    ->  replace_ground_with_expr(Expr,NewExpr)
    ;   random_int_expr_mutation(Expr,NewExpr)) , 
    NewExpression =.. [Type,NewExpr].

% no mutation for max_int, min_int, max, min or identifier
random_int_expr_mutation(Expression,Expression).

% replace ground value with an integer expression 
replace_ground_with_expr(b(integer(Value),integer,Info),NewExpr) :- 
    find_expr_clpfd(Value,Expr) , ! ,
    % random choice of further mutation
    random(0,10,R) , 
    (R < 7
    ->  NewExpr = b(Expr,integer,Info)
    ;   random_int_expr_mutation(b(Expr,integer,Info),NewExpr)).

% evaluate an expression using clpfd labeling to mutate a specific value
find_expr_clpfd(Value,Expr) :- 
    % random range to get different mutations
    maplist(random(-20000,20000),[X1,Y1,X2,Y2]) , 
    % change constraints order if neccessary
    (X1 #=< Y1
    ->  Val1 in X1..Y1 
    ;   Val1 in Y1..X1) ,
    (X2 #=< Y2
    ->  Val2 in X2..Y2 
    ;   Val2 in Y2..X2) ,
    random_member(Operator,[add,minus,multiplication,modulo]) , % power_of, div
    ( Operator = add
    ->  Val1 + Val2 #= Value
    ; Operator = minus
    ->  Val1 - Val2 #= Value
    ; Operator = div
    ->  Val1 / Val2 #= Value
    ; Operator = modulo
    ->  Val1 in 1..X2 ,
        Val2 in 1..Y2 ,     % no negative numbers
        Val1 mod Val2 #= Value
    ;   Val1 * Val2 #= Value) , 
    labeling([],[Val1,Val2]) , 
    Expr =.. [Operator,b(integer(Val1),integer,[]),b(integer(Val2),integer,[])]. 
% if labeling failed try another expression
find_expr_clpfd(Value,Expr) :-  
    find_expr_clpfd(Value,Expr).