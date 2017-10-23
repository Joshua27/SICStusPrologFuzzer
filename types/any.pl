:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% Options:
% noset to generate no lists or trees

generate(any(Options),Value) :-
    TypeList = [atom,boolean,float,integer,rational,number] ,
    member(noset,Options) ,
    delete(Options,noset,NOptions) , % no nested data structures
    random_member(Temp,TypeList) ,
    Type =.. [Temp,NOptions] ,
    generate(Type,Value).
generate(any(Options),Value) :-
    TypeList = [atom,boolean,float,integer,rational,number] ,
    \+member(noset,Options) ,
    random_member(TempType,[list,avl_tree,tree|TypeList]) ,
    random_member(SubType,TypeList) , % random subtype for nested data structures
    nested_type(TempType,Options,SubType,Type) ,
    generate(Type,Value).

nested_type(TempType,Options,SubType,Type) :-
    member(TempType,[list,avl_tree,tree]) ,
    InnerType =.. [SubType,Options] ,
    Type =.. [TempType,InnerType,Options].
nested_type(TempType,Options,_,Type) :-
    \+member(TempType,[list,avl_tree,tree]) ,
    Type =.. [TempType,Options].
