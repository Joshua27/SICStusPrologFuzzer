:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% Options: 
% noset to generate no lists or trees

generate(any(Options),Value) :- 
    TypeList = [atom,boolean,float,integer,rational,number] , 
    (member(noset,Options) -> 
        delete(Options,noset,NOptions) , 
        random_member(Temp,TypeList) ,
        Type =.. [Temp,NOptions]
    ;   random_member(Temp,[list,avl_tree,tree|TypeList]) , 
        Type =.. [Temp,Options]) , 
    generate(Type,Value).