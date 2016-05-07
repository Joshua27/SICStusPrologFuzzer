:- multifile generate/2.

% shrinking of all mutations defined in mutation.pl

:- use_module(library(lists),[is_list/1]).
:- use_module(library(sets),[subtract/3]).
:- use_module(library(random),[random_permutation/2,random_member/2,random/3]).

% mutation of ProB ast predicates
generate(mutation(Predicate:prob_ast_pred),NewPredicate) :- 
    \+is_list(Predicate) , 
    random_pred_mutation(Predicate,NewPredicate).
% list of predicates, random concatenation of predicates then 
% mutation of consisting integer and set expressions
generate(mutation(Predicates:prob_ast_pred),NewPredicate) :- 
    concatenate_ast(Predicates,[conjunct,disjunct,implication,equivalence],Predicate) ,
    random_pred_mutation(Predicate,NewPredicate). 

% randomly replaces ground integer, set or sequence ast nodes with a matching 
% expression all over the predicate, ProB values get permutated

% two argument predicates
random_pred_mutation(b(Predicate,pred,Info),b(NewPredicate,pred,Info)) :- 
    Predicate =.. [Type,Expr1,Expr2] , 
    random_mutation(Expr1,NewExpr1) , 
    random_mutation(Expr2,NewExpr2) , 
    % randomly swap predicates with a matching one
    random(0,9,R) , 
    (R > 4
    ->  swap_type(Type,MutatedType) 
    ;   MutatedType = Type) , 
    NewPredicate =.. [MutatedType,NewExpr1,NewExpr2].

% one argument predicates
random_pred_mutation(b(Predicate,pred,Info),b(NewPredicate,pred,Info)) :- 
    Predicate =.. [Type,Expr] , 
    random_mutation(Expr,NewExpr) , 
    NewPredicate =.. [Type,NewExpr]. 

random_pred_mutation(Value,Value).

% apply respective mutation
random_mutation(Expr,Mutation) :- 
    Expr = b(_,set(_),_) ,
    random_set_expr_mutation(Expr,Mutation).
random_mutation(Expr,Mutation) :- 
    Expr = b(_,seq(_),_) ,
    random_seq_expr_mutation(Expr,Mutation).
random_mutation(Expr,Mutation) :- 
    Expr = b(_,integer,_) , 
    random_int_expr_mutation(Expr,Mutation).
random_mutation(Expr,Mutation) :- 
    random_pred_mutation(Expr,Mutation).

% concatenate a list of expressions or predicates by using one expression of argument Nodes
concatenate_ast(ListOfExpressions,Nodes,Expression) :- 
    random_permutation(ListOfExpressions,[H|Permutation]) , 
    % use an accumulator
    concatenate_ast(Permutation,H,Nodes,Expression).
concatenate_ast([],NewExpression,_,NewExpression).
concatenate_ast([Expression|T],Current,Nodes,NewExpression) :- 
    concatenate(Expression,Current,Nodes,NewCurrent) , 
    % permutate rest of list
    random_permutation(T,Permutation) ,
    concatenate_ast(Permutation,NewCurrent,Nodes,NewExpression).

concatenate(Expression1,Expression2,Nodes,NewExpression) :- 
    random_member(SurroundingType,Nodes) , 
    InnerType =.. [SurroundingType,Expression1,Expression2] , 
    NewExpression = b(InnerType,integer,[]).

% change some types for mutation
swap_type(Type,NType) :- 
    ValidTypes = [less,less_equal,greater,greater_equal] , 
    member(Type,ValidTypes) , 
    % don't choose the same type for replacement
    subtract(ValidTypes,Type,NewValidTypes) , 
    random_member(NType,[equal,not_equal|NewValidTypes]).
swap_type(disjunct,conjunct).
swap_type(conjunct,disjunct).
swap_type(implication,equivalence).
swap_type(equivalence,implication).
swap_type(equal,not_equal).
swap_type(not_equal,equal).
swap_type(subset,subset_strict).
swap_type(subset_strict,subset).
swap_type(member,not_member).
swap_type(not_member,member).
swap_type(Type,Type).