:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3,random_member/2]).
:- use_module(library(trees),[list_to_tree/2,tree_to_list/2]).

% Options:
% size:X with X the amount of elements in the tree

% random binary tree
generate(tree(Options),Value) :-
    is_list(Options) , 
    generate(tree(any,Options),Value).

generate(tree(Type),Value) :-
    generate(tree(Type,[]),Value).

% tree of given type
generate(tree(Type,Options),Value) :- 
    % size specifies amount of elements in the tree
    (member(size:Size,Options)
    ->  Size > 0 
    ;   random(1,20,Size)) ,
    generate(list(Type,[size:Size]),List) ,
    list_to_tree(List,Value).

% shrink binary tree
% empty tree
shrink(Type,t(_,t,t),t) :- 
    Type =..[tree|_].

shrink(Type,Value,Shrunken) :- 
    Type =.. [tree|_] , 
    tree_to_list(Value,List) , 
    % refer to shrinking of lists
    shrink(list(_),List,ShrunkenList) , 
    list_to_tree(ShrunkenList,Shrunken).