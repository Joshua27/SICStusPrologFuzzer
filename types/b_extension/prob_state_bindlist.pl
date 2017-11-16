:- multifile generate/2.

% return explicit bindings as terms bind/2 and the used identifier nodes
generate(prob_state_bindlist(AST),Bindings:TypedIDs) :-
    find_typed_identifier_uses_with_info(AST,TypedIDs) ,
    maplist(ast_ids_to_bindlist,TypedIDs,Bindings) , !.
% unbound bindlist
generate(prob_state_bindlist(AST,Options),Bindings:TypedIDs) :-
    member(unbound,Options) ,
    find_typed_identifier_uses_with_info(AST,TypedIDs) ,
    findall(bind(ID,_),member(b(identifier(ID),_,_),TypedIDs),Bindings) , !.

% generate a binding for an identifier
ast_ids_to_bindlist(b(identifier(Name),record(FieldList),_),bind(Name,Record)) :-
    FieldList = [field(_,Type)|_] ,
    NType =.. [Type,[]] ,
    findall(FieldName,member(field(FieldName,_),FieldList),NameList) ,
    generate(prob_ast_record(NType,[names:NameList]),Record).
ast_ids_to_bindlist(b(identifier(Name),seq(empty),_),bind(Name,Value)) :-
    generate(prob_ast_seq(empty_sequence),Value).
ast_ids_to_bindlist(b(identifier(Name),set(empty),_),bind(Name,Value)) :-
    generate(prob_ast_set(empty([]),_),Value).
ast_ids_to_bindlist(b(identifier(Name),Type,_),bind(Name,Value)) :-
    % add options to inner type for generation
    inner_type(Type,Inner,Outter) ,
    % options for integer, other types ignore this option
    NewInner =.. [Inner,[small,positive,nozero]] ,
    surround_type(NewInner,Outter,TempType) ,
    gen_type(TempType,value,NewType) ,
    generate(NewType,Value).
