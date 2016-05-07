:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% list of options:
% extension, value (i.e. random value set) to generate a specific type of ast set
% avl, list for specific type of value set, none for any set
% any to generate either set or expression
% size:Value to limit length of set

generate(prob_ast_set(Options),Value) :- 
    is_list(Options) , 
    generate(ground_type,Type) , 
    generate(prob_ast_set(Type,Options),Value).

generate(prob_ast_set(Type),Value) :- 
    generate(prob_ast_set(Type,[]),Value).

generate(prob_ast_set(Type,Options),Value) :-
    member(Type,[integer(_),boolean(_),string(_),couple(_,_)]) , 
    Type =.. [InnerType,_] , ! , 
    ( member(extension,Options)
    ->  delete(Options,extension,NOptions) ,
        gen_type(Type,ast,NType) , 
        generate(list(NType,NOptions),Set),
        Value = b(set_extension(Set),set(InnerType),[])
    ; (member(avl,Options) ; member(list,Options) ; member(value,Options))
    ->  generate(prob_value_set(Type,Options),Set),
        Value = b(value(Set),set(InnerType),[])
    ; % set or set expression
      member(any,Options) ->
        delete(Options,any,NOptions) , 
        random_member(T,[set,expr]) ,
        (T = set
        ->  generate(ground_type,SetType) , 
            generate(prob_ast_set(SetType,NOptions),Value)
        ;   generate(prob_ast_set_expr(NOptions),Value))
    ;   random_member(SetOption,[[extension|Options],[value|Options]]) , 
        generate(prob_ast_set(Type,SetOption),Value)).

generate(prob_ast_set(empty([]),_),b(set_extension([]),set(empty),[])).

% set of several sets
generate(prob_ast_set(SetTypeOptions,Options),b(Set,set(NType),[])) :-
    SetTypeOptions =.. [set,Type|InnerOptions] , 
    (InnerOptions = [NOptions]
    ->  true
    ;   NOptions = InnerOptions) , 
    (member(size:Size,Options)
    ->  true 
    ;   random(1,10,Size)) ,
    length(List,Size) , 
    ( member(list,Options)
    ->  maplist(generate(prob_value_set(Type,NOptions)),List) ,
        Set = value(List)
    ; member(avl,Options) 
    ->  maplist(generate(prob_value_set(Type,NOptions)),List) ,
        findall(Key-true,member(Key,List),AVLList) , 
        list_to_avl(AVLList,AVL) , 
        Set = value(avl_set(AVL))
    ;   maplist(generate(prob_ast_set(Type,NOptions)),List) ,
        Set = set_extension(List)) , 
    % remove options for node type
    inner_type(set(Type),_,Outter) , 
    length(Outter,L) , 
    nth1(L,Outter,NewInner,NewOutter) , 
    surround_type(NewInner,NewOutter,NType).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_set|_] ,
    minimize_set_expr(Value,Shrunken).