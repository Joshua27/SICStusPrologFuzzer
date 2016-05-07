:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists),[is_list/1,maplist/2]).
:- use_module(library(random),[random_permutation/2]).

% mutation of a list
generate(mutation(L:list),Value) :- 
    flattened(L) ,
    random_permutation(L,Value).
% mutation of a list of lists
generate(mutation(ListOfLists:list),Value) :- 
    % mutate each list on its own
    maplist(random_permutation,ListOfLists,Temp) ,
    % also mutate the whole list
    random_permutation(Temp,Value).

% mutation of trees
% random permutation of a binary tree
generate(mutation(Tree:tree(_)),Value) :- 
    \+ is_list(Tree) , 
    tree_to_list(Tree,TreeList) , 
    random_permutation(TreeList,TreePerm) , 
    list_to_tree(TreePerm,Value). 
    
% random permutation of several binary trees given in a list
generate(mutation(List:tree(_)),Value) :- 
    maplist(tree_to_list,List,TreeList) , 
    flatten(TreeList,FlattenedList) , 
    random_permutation(FlattenedList,TreePerm) , 
    list_to_tree(TreePerm,Value).

shrink(mutation(_:Type),Value,Shrunken) :-
    shrink(Type,Value,Shrunken).

% flatten a nested list 
flatten([],[]) :- !.
flatten(H,[H]) :- \+is_list(H).
flatten([H|T],List) :- 
    flatten(H,L1) , 
    flatten(T,L2) , 
    append(L1,L2,List).

% tests if list is flattened
flattened(L) :- 
    flatten(L,L).

% convert all Key-Value pairs to Key
avl_list_to_list(AVLList,List) :- 
    findall(Key,member(Key-_,AVLList),List).

% convert (Index,Value) to Value
seq_to_list(Seq,List) :- 
    findall(Value,member((_,Value),Seq),List).