:- multifile generate/2.

% shrinking of all mutations defined in mutation.pl

:- use_module(library(lists),[is_list/1]).
:- use_module(library(random),[random_permutation/2,random_member/2,random/3]).

% mutation of ProB ast set expressions
generate(mutation(Expression:prob_ast_set_expr),NewExpression) :-
    \+is_list(Expression) ,
    random_set_expr_mutation(Expression,NewExpression).
% list of set expressions, random concatenation of expression then
% mutation of consisting set expressions
generate(mutation(Expressions:prob_ast_set_expr),NewExpression) :-
    maplist(random_set_expr_mutation,Expressions,Mutated) ,
    concatenate_ast(Mutated,[union,intersection],NewExpression).

% mutation of ProB value list sets
generate(mutation(L:prob_value_set),Value) :-
    generate(mutation(L:list),Value).

% mutation of ProB value avl sets
generate(mutation(avl_set(Set):prob_value_set),avl_set(Value)) :-
    avl_to_list(Set,AvlList) ,
    random_permutation(AvlList,Permutation) ,
    % don't sort values during creation of avl
    ord_list_to_avl(Permutation,Value).
% mutate several avl sets and return a single one
generate(mutation(L:prob_value_set),avl_set(Value)) :-
    maplist(avl_to_list,L,ListOfLists) ,
    flatten(ListOfLists,List) ,
    random_permutation(List,Permutation) ,
    ord_list_to_avl(Permutation,Value).

% union, intersection, set_subtraction
random_set_expr_mutation(b(Expression,set(SetType),Info),b(NewExpression,set(SetType),Info)) :-
    Expression =.. [Type,Expr1,Expr2] ,
    member(Type,[union,intersection,set_subtraction]) ,
    random(0,3,R) ,
    random_set_expr_mutation_aux(R,Expr1,Expr2,NewExpr1,NewExpr2) ,
    NewExpression =.. [Type,NewExpr1,NewExpr2].

% general_union, general_intersection
random_set_expr_mutation(b(Expression,set(SetType),Info),b(NewExpression,set(SetType),Info)) :-
    Expression =.. [Type,Expr] ,
    member(Type,[general_union,general_intersection]) ,
    Expr = b(set_extension([Set]),set(set(SetType)),ArgInfo) ,
    % just permutation
    permutate_set(Set,NewSet) ,
    NewExpr = b(set_extension([NewSet]),set(set(SetType)),ArgInfo) ,
    NewExpression =.. [Type,NewExpr].

% if set_extension or value set is given
random_set_expr_mutation(Expression,NewExpression) :-
    mutate_set(Expression,NewExpression).

% don't mutate empty set or interval
random_set_expr_mutation(Expression,Expression).

random_set_expr_mutation_aux(0,Expr1,Expr2,NewExpr1,Expr2) :-
    random_set_expr_mutation(Expr1,NewExpr1).
random_set_expr_mutation_aux(1,Expr1,Expr2,Expr1,NewExpr2) :-
    random_set_expr_mutation(Expr2,NewExpr2).
random_set_expr_mutation_aux(2,Expr1,Expr2,NewExpr1,NewExpr2) :-
    random_set_expr_mutation(Expr1,NewExpr1) ,
    random_set_expr_mutation(Expr2,NewExpr2).

% mutate set_extension nodes by replacing with matching set expressions like union or set_subtraction
mutate_set(b(set_extension(Set),SetType,Info),Mutation) :-
    (random(0,2,0)
    ->  random_union(Set,SubSetA,SubSetB) ,
        Temp = b(union(b(set_extension(SubSetA),SetType,Info),b(set_extension(SubSetB),SetType,Info)),SetType,[])
    ;   random_subtraction(Set,SetA,SetB) ,
        Temp = b(set_subtraction(b(set_extension(SetA),SetType,Info),b(set_extension(SetB),SetType,Info)),SetType,[])) ,
    % random choice of further mutation
    random(0,10,R) ,
    (R < 7
    ->  Mutation = Temp
    ;   random_set_expr_mutation(Temp,Mutation)).
mutate_set(b(value(Set),SetType,Info),b(value(NewSet),SetType,Info)) :-
    generate(mutation(Set:prob_value_set),NewSet).

% just permutate given data, no generation
permutate_set(b(set_extension(Set),SetType,Info),b(set_extension(NewSet),SetType,Info)) :-
    generate(mutation(Set:list),NewSet).
permutate_set(b(value(Set),SetType,Info),b(value(NewSet),SetType,Info)) :-
    generate(mutation(Set:prob_value_set),NewSet).

% split a list in two sublists randomly
random_union([],[],[]).
random_union(List,A,B) :-
    length(List,Length) ,
    generate(integer([between(1,Length)]),LengthA) ,
    LengthB is Length - LengthA ,
    LengthA >= 0 , LengthB >= 0 ,
    % set length of both sublists
    length(A,LengthA) ,
    length(B,LengthB) ,
    % use append to find solutions
    append(A,B,List).
random_union(_,[],[]).

% add random list B to first argument so that A - B = List
% like replace [1,2] with [1,2,3] - [3]
random_subtraction(List,A,B) :-
    List = [b(_,Type,_)|_] ,
    NType =.. [Type,[]] ,
    generate(prob_ast_set(NType,[extension]),b(set_extension(B),_,_)) ,
    append(List,B,A).
