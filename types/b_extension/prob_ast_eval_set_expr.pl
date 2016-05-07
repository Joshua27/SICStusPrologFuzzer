% evaluation of ProB AST set expressions

:- use_module(library(sets)).
:- use_module(library(lists),[is_list/1,maplist/3]).
:- use_module(library(avl)).

% returns avl_set
sint_avl(Expression,avl_set(AVL)) :- 
    sint(Expression,Value) , 
    findall(Key-true,member(Key,Value),AVLList) , 
    list_to_avl(AVLList,AVL).

% ast record
sint(b(rec(Record),_,_),rec(NewRecord)) :- 
    maplist(sint,Record,NewRecord).
% value field
sint(field(Name,Value),field(Name,NewValue)) :- 
    sint(Value,Temp) , 
    ( number(Temp) -> 
        NewValue = int(Temp) 
    ; Temp = true ->
        NewValue = pred_true
    ; Temp = false ->
        NewValue = pred_false
    ;   NewValue = string(Temp)).

% ast set expressions
sint(b(set_extension([]),_,_),[]).
    
sint(b(boolean_true,_,_),true).
sint(b(boolean_false,_,_),false).
sint(b(string(String),_,_),String).
sint(b(integer(Integer),_,_),Integer).

sint(b(value(Set),_,_),Set) :-
    is_list(Set).
sint(b(value(avl_set(AvlSet)),_,_),Set) :-
    avl_to_list(AvlSet,AVLList) , 
    findall(Key,member(Key-_,AVLList),Set).

sint(b(interval(b(integer(A),integer,_),b(integer(B),integer,_)),set(integer),_),Value) :- 
    interval(A,B,Value).

sint(b(union(Set1,Set2),_,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    union(ValueSet1,ValueSet2,Value).

sint(b(general_union(Set),_,_),Value) :- 
    sint(Set,ValueSet) ,   
    flatten(ValueSet,Value).

sint(b(intersection(Set1,Set2),_,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    intersection(ValueSet1,ValueSet2,Value).

sint(b(general_intersection(Set),_,_),Value) :- 
    sint(Set,ValueSet) , 
    flatten(ValueSet,Value).

sint(b(set_subtraction(Set1,Set2),_,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    subtract(ValueSet1,ValueSet2,Value).

sint(b(set_extension(Set),set(_),_),Value) :- 
    is_list(Set) , 
    % convert to prob value list set
    maplist(ast_to_prob_value,Set,Value).

sint(b(set_extension(Temp),_,_),[Set]) :- 
    sint(Temp,Set).

% create list from an interval
interval(A,A,[A]) :- !.
interval(A,B,[A|T]) :-
    A1 is A + 1 , 
    interval(A1,B,T).   

% convert to prob value
ast_to_prob_value(b(integer(Value),_,_),int(Value)).
ast_to_prob_value(b(string(Value),_,_),string(Value)).
ast_to_prob_value(b(Bool,_,_),Value) :- 
    (Bool = boolean_true -> Value = pred_true ; Bool = boolean_false -> Value = pred_false).
% true for prob value
ast_to_prob_value(_,_).
% convert to prolog value
ast_to_value(int(Value),Value).
ast_to_value(string(Value),Value).
ast_to_value(Bool,Value) :- 
    (Bool = pred_true -> Value = true ; Bool = pred_false -> Value = false).
ast_to_value(b(integer(Value),_,_),Value).
ast_to_value(b(string(Value),_,_),Value).
ast_to_value(b(Bool,_,_),Value) :- 
    (Bool = boolean_true -> Value = true ; Bool = boolean_false -> Value = false).