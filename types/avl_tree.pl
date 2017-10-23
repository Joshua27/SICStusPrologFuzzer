:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(avl)).

% Options:
% size:X with X the amount of elements in the tree

% random avl tree
generate(avl_tree(Options),Value) :-
    is_list(Options) ,
    generate(avl_tree(any([]),Options),Value).

generate(avl_tree(Type),Value) :-
    generate(avl_tree(Type,[]),Value).

% avl tree with given type and options
generate(avl_tree(Type,Options),AVL) :-
    (member(size:Size,Options)
    ->  Size >= 0
    ;   random(1,50,Size)) ,
    length(Value,Size),
    maplist(generate(Type),Value),
    findall(Key-true,member(Key,Value),Pairs),
    list_to_avl(Pairs,AVL).

% check empty avl
shrink(Type,_,E) :-
    Type =..[avl_tree|_] ,
    empty_avl(E).
% remove some member
shrink(Type,Old,New) :-
    Type =..[avl_tree|_] ,
    avl_member(Key,Old),
    avl_delete(Key,Old,_,New).
