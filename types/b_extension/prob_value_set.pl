:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists),[delete/3]).

% Options:
% not-well-defined-values, size:S, list, avl or none for random value set

% generate any type
generate(prob_value_set(Options),Value) :- 
    is_list(Options) , 
    generate(ground_type,Type) , 
    generate(prob_value_set(Type,Options),Value).

% no options
generate(prob_value_set(Type),Value) :- 
    generate(prob_value_set(Type,[]),Value).

generate(prob_value_set(Type,Options),Value) :- 
    (member(not-well-defined,Options)
    ->  delete(Options,not-well-defined,T) , 
        TOptions = [not-well-defined-values|T] 
    ;   TOptions = Options) ,  
    % only generate sets of the same type for any 
    (Type = any(_) -> generate(ground_type,NType) ; NType = Type) , 
    gen_type(NType,value,GenType) , 
    ( member(size:Size,TOptions)
    ->  Size > 0 , NOptions = TOptions 
    ;   random(1,50,Size) , NOptions = [size:Size|TOptions]) ,
    ( member(avl,TOptions)
    ->  % avl tree doesn't have duplicates
        generate(avl_tree(GenType,NOptions),Temp) , 
        Value = avl_set(Temp)
    ; member(list,TOptions)
    ->  generate(list(GenType,NOptions),Temp) , 
        (member(not-well-defined-values,NOptions)
        ->  Value = Temp 
        ;   remove_dups(Temp,Value))
    ;   random_member(SetType,[avl,list]) , 
        generate(prob_value_set(NType,[SetType|NOptions]),Value)).

shrink(Type,avl_set(Value),avl_set(Shrunken)) :-
    Type =.. [prob_value_set|_] ,
    avl_to_list(Value,AVLList) , 
    findall(Key,member(Key-_,AVLList),List) , 
    ((flattened(List) , \+member(avl_set(_),List))
    ->  shrink(list,List,NewList) 
    ;   maplist(shrink(prob_value_any),List,NewList)) , 
    findall(Key-true,member(Key,NewList),NewAVLList) , 
    list_to_avl(NewAVLList,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_value_set|_] ,
    shrink(list,Value,Shrunken).