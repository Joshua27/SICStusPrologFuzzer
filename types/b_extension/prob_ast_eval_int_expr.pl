% evaluation of ProB AST integer expressions

:- use_module(library(lists),[is_list/1,min_member/2,max_member/2]).
:- use_module(library(avl),[avl_to_list/2,list_to_avl/2]).

% return integer ast node
int_prob(Expression,b(integer(Value),integer,[])) :- 
    int(Expression,Value).

% return prolog value
int(b(Expression,_,_),Value) :- 
    Expression =.. [Type,_,_] ,
    member(Type,[add,minus,multiplication,div,power_of,modulo]) ,
    % catch too big numbers or evaluation errors
    (on_exception(_,eval(b(Expression,_,_),Value),fail)
    ->  true
    ;   int(b(max_int,_,_),Value)).

int(b(max_int,_,_),3).  
    /*preferences:get_preference(maxint,Max).*/
int(b(min_int,_,_),-1).
    /*preferences:get_preference(minint,Min).*/

int(b(integer(Value),_,_),Value).

int(int(Value),Value).

int(b(card(Set),_,_),Value) :- 
    sint(Set,ValueSet) , 
    length(ValueSet,Value).

int(b(unary_minus(Expr),_,_),Value) :- 
    int(Expr,Temp) , 
    Value is -(Temp).

% prob ast sets
% not-well-defined for empty set
int(b(max(b(set_extension([]),set(_),_)),integer,_),0).
int(b(min(b(set_extension([]),set(_),_)),integer,_),0).
int(b(max(b(set_extension(ProBValueList),set(integer),_)),integer,_),Max) :- 
    maplist(ast_to_value,ProBValueList,List) , 
    max_member(Max,List).
int(b(min(b(set_extension(ProBValueList),set(integer),_)),integer,_),Min) :- 
    maplist(ast_to_value,ProBValueList,List) , 
    min_member(Min,List).

% prob value sets
% also interpret max, min of empty sets for shrinking of 
% not well defined integer expressions
int(b(max(b(value([]),set(integer),[])),integer,[]),0).
int(b(min(b(value([]),set(integer),[])),integer,[]),0).

int(b(max(b(value(avl_set(empty)),set(integer),[])),integer,[]),0).
int(b(min(b(value(avl_set(empty)),set(integer),[])),integer,[]),0).
% list set
int(b(max(b(value(Set),set(integer),_)),integer,_),Max) :- 
    is_list(Set) ,
    max_member(MaxMem,Set) , 
    MaxMem = int(Max).
int(b(min(b(value(Set),set(integer),_)),integer,_),Min) :- 
    is_list(Set) ,
    min_member(MinMem,Set) , 
    MinMem = int(Min).

% avl set
int(b(max(b(value(avl_set(Set)),set(integer),_)),integer,_),Max) :- 
    avl_to_list(Set,Temp) ,
    findall(Key,member(Key-_,Temp),List) ,
    max_member(MaxMem,List) , 
    MaxMem = int(Max).
int(b(min(b(value(avl_set(Set)),set(integer),_)),integer,_),Min) :- 
    avl_to_list(Set,Temp) ,
    findall(Key,member(Key-_,Temp),List) ,
    min_member(MinMem,List) , 
    MinMem = int(Min).

% evaluation
eval(b(add(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    Value is Value1 + Value2.
eval(b(minus(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    Value is Value1 - Value2.
eval(b(multiplication(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    Value is Value1 * Value2.
eval(b(div(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    % round values to be able to interpret not-well-defined expressions
    % so shrinking will be accepted by ProB interpreter
    %Value is round(Value1 / Value2).
    Value is Value1 / Value2.
eval(b(modulo(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Temp1) , Value1 is integer(Temp1) ,   % meanwhile bugfix (see thesis, chapter 2):
    int(Expr2,Temp2) , Value2 is integer(Temp2) ,   % problem with mod(0.0,_) in SICStus 4.3.1 implementation, so integer/1 fixes the issue
    Value is mod(Value1,Value2).
eval(b(power_of(Expr1,Expr2),_,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    Value is Value1 ^ Value2.