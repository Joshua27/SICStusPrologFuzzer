:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).
:- use_module(library(avl),[list_to_avl/2]).

% Options:
% not-well-defined-values, size:S, list, avl or none for random value sequence

% any type
generate(prob_value_seq(Options),Value) :- 
    is_list(Options) , 
    generate(ground_type,Type) , 
    generate(prob_value_seq(Type,Options),Value).
% no options
generate(prob_value_seq(Type),Value) :- 
    generate(prob_value_seq(Type,[]),Value).

generate(prob_value_seq(Type,Options),Value) :-
    (member(not-well-defined,Options)
    ->  delete(Options,not-well-defined,T) , 
        TOptions = [not-well-defined-values|T] 
    ;   TOptions = Options) ,
    (member(size:Size,TOptions)
    ->  true
    ;   random(1,20,Size)) , 
    gen_type(Type,value,NType) , 
    length(List,Size) , 
    maplist(generate(NType),List) , 
    (member(not-well-defined-values,TOptions)
    ->  maplist(gen_random_indexed_couple,List,CoupleList)
    ;   remove_dups(List,NList) ,
        gen_indexed_couple_list(1,NList,CoupleList)) ,
    % random choice of list set or avl set if no option is given
    random(0,2,R) ,
    ( \+member(list,TOptions) , (member(avl,TOptions) ; R = 0)
    ->  % add key to every element to use list_to_avl
        findall(Key-true,member(Key,CoupleList),Pairs) ,
        list_to_avl(Pairs,AVL) , 
        Value = avl_set(AVL)
    ;   % else list sequence
        Value = CoupleList).

% replace each list element with an indexed couple, starting at 1
gen_indexed_couple_list(_,[],[]).
gen_indexed_couple_list(C,[Elm|T],[(int(C),Elm)|NT]) :- 
    C1 is C + 1 ,
    gen_indexed_couple_list(C1,T,NT).
    
% use for no well-definedness
gen_random_indexed_couple(Value,(Index,Value)) :- 
    generate(prob_value_integer([small]),Index).

shrink(Type,avl_set(Value),avl_set(Shrunken)) :- 
    Type =..[prob_value_seq|_] , 
    avl_to_list(Value,AVLList) , 
    findall(Key,member(Key-_,AVLList),SeqList) , 
    shrink(prob_value_seq,SeqList,NewSeqList) ,  
    findall(Key-true,member(Key,NewSeqList),NewAVL) , 
    list_to_avl(NewAVL,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =..[prob_value_seq|_] , 
    findall(Val,member((_,Val),Value),ValueList) , 
    shrink(list,ValueList,ShrunkenList) , 
    gen_indexed_couple_list(1,ShrunkenList,Shrunken).