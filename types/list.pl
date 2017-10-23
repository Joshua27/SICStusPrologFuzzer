:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists)).
:- use_module(library(random),[random/3,random_member/2]).

% Options:
% size:X with X the amount of elements in the list

% list of any type
generate(list(Options),Value) :-
    is_list(Options) ,
    generate(ground_type,Type) ,
    generate(list(Type,Options),Value).

generate(list(Type),Value) :-
    generate(list(Type,[]),Value).

generate(list(_,Options),[]) :-
    member(size:0,Options).

% list of given type
generate(list(Type,Options),Value) :-
    (member(size:Size,Options)
    ->  Size > 0
    ;   random(1,50,Size)) ,
    length(Value,Size) ,  
    maplist(generate(Type),Value).

% shrink list of list
shrink(Type,Value,Shrunken) :-
    Type =..[list|_] ,
    \+ flattened(Value) ,
    maplist(shrink(list),Value,Shrunken).

% check empty list
shrink(Type,[_],[]) :-
    Type =..[list|_].

% remove random elements
shrink(Type,L,NL) :-
    Type =.. [list|_] ,
    is_list(L) ,
    random_member(E,L) ,
    delete(L,E,NL).

% remove last element of a list
remove_last([],[]).
remove_last([_],[]).
remove_last([H|T],[H|T2]) :-
    remove_last(T,T2).
