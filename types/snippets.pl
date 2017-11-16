% collection of code snippets

% try to insert a given amount of identifier nodes to an ast, the actual amount can be less
insert_identifier_to_ast(AST,0,AST) :- !.
insert_identifier_to_ast(b(Node,Type,_),_,NewAST) :-
    (Node = min_int ; Node = max_int ; Type = boolean ; Node = integer(_) ; Node = string(_)) ,
    generate(prob_ast_identifier(Type),NewAST).
insert_identifier_to_ast(b(Node,Type,_),_,NewAST) :-
    Node =.. [_,Temp] , Type \= pred , Type \= record(_) , (Temp =.. [_] ; is_list(Temp)) ,
    generate(prob_ast_identifier(Type),NewAST).
insert_identifier_to_ast(b(rec(_),record(Type),_),_,NewAST) :-
    generate(prob_ast_identifier(record(Type)),NewAST).
insert_identifier_to_ast(b(Node,Type,Info),Amount,NewAST) :-
    Node =.. [Inner,Arg] ,
    insert_identifier_to_ast(Arg,Amount,NewArg) ,
    NewNode =.. [Inner,NewArg] ,
    NewAST = b(NewNode,Type,Info).
% no identifier for power_of
insert_identifier_to_ast(b(Node,Type,Info),_,b(Node,Type,Info)) :-
    Node = power_of(_,_).
insert_identifier_to_ast(b(Node,Type,Info),Amount,NewAST) :-
    Node =.. [Inner,LArg,RArg] ,
    LAmount is Amount div 2 ,
    RAmount is Amount - LAmount ,
    insert_identifier_to_ast(LArg,LAmount,NLArg) ,
    insert_identifier_to_ast(RArg,RAmount,NRArg) ,
    NewNode =.. [Inner,NLArg,NRArg] ,
    NewAST = b(NewNode,Type,Info).
insert_identifier_to_ast(AST,_,AST).

% creates the type to use in generate/2 with prefix 'prob_ast_' or 'prob_value'
% only atom_concat wouldn't succeed if type is a term like set(integer([]))
% ASTType is either 'ast' or 'value'

% don't expand fixed values
gen_type(TypeIn,_,TypeIn) :-
    TypeIn =.. [fixed,_].
gen_type(TypeIn,ASTType,NType) :-
    TypeIn =.. [set,In] ,
    atom_concat(prob_,ASTType,Temp) ,
    atom_concat(Temp,'_',ProBType) ,
    atom_concat(ProBType,set,NewPre) ,
    NType =.. [NewPre,In,[]]. % no options for ast or value sets
gen_type(TypeIn,ASTType,NType) :-
    TypeIn =.. [Type,In1,In2] ,
    atom_concat(prob_,ASTType,Temp) ,
    atom_concat(Temp,'_',ProBType) ,
    atom_concat(ProBType,Type,NewPre) ,
    NType =.. [NewPre,In1,In2].
gen_type(TypeIn,ASTType,NType) :-
    TypeIn =.. [Type,In] , ! ,
    atom_concat(prob_,ASTType,Temp) ,
    atom_concat(Temp,'_',ProBType) ,
    atom_concat(ProBType,Type,NewPre) ,
    NType =.. [NewPre,In].
gen_type(Type,ASTType,NType) :-
    Type =.. [_] ,
    atom_concat(prob_,ASTType,Temp) ,
    atom_concat(Temp,'_',ProBType) ,
    atom_concat(ProBType,Type,NType).

% detype an ast like the parsers output
:- multifile generate/2.
generate(detype(AST),DetypedAST) :-
    detype_ast(AST,1,DetypedAST,_NodeAmount).

detype_ast(AST,DetypedAST) :-
    detype_ast(AST,1,DetypedAST,_NodeAmount).
% remove index of sequence elements
detype_ast((_,Node),C,DetypedAST,NC) :-
    detype_ast(Node,C,DetypedAST,NC).
% ProB-Values
detype_ast(int(Value),C,integer(C,Value),C).
detype_ast(string(Value),C,string(C,Value),C).
detype_ast(Bool,C,DetypedBool,C) :-
    ( Bool = pred_true ->
        DetypedBool = boolean_true(C)
    ; Bool = pred_false ->
        DetypedBool = boolean_false(C)).
% ProB-AST-Nodes
detype_ast(b(integer(Value),_,_),C,integer(C,Value),C).
detype_ast(b(string(Value),_,_),C,string(C,Value),C).
detype_ast(b(Node,_,_),C,DetypedAST,C) :-
    Node =.. [_] ,
    DetypedAST =.. [Node,C].
detype_ast(field(Name,ValueNode),C,rec_entry(C,identifier(C1,Name),DetypedValueNode),NC) :-
    C1 is C + 1 ,
    C2 is C1 + 1 ,
    detype_ast(ValueNode,C2,DetypedValueNode,NC).
detype_ast(b(identifier(Name),_,_),C,identifier(C,Name),C).
% sequence_extension or value sequence
detype_ast(b(Node,_,_),C,DetypedAST,NC) :-
    Node =.. [NodeType,Arg] ,
    avl_to_list_set(Arg,NArg) ,
    (NodeType = sequence_extension ; (NodeType = value , NArg = [(_,_)|_])) ,
    C1 is C + 1 ,
    detype_set(NArg,C1,DetypedArg,NC) ,
    % use 'sequence_extension' instead of 'value' for detyped ASTs
    DetypedAST =.. [sequence_extension,C,DetypedArg].
% set_extension or value set
detype_ast(b(Node,_,_),C,DetypedAST,NC) :-
    Node =.. [NodeType,Arg] ,
    (NodeType = set_extension ; NodeType = value) ,
    avl_to_list_set(Arg,NArg) ,
    C1 is C + 1 ,
    detype_set(NArg,C1,DetypedArg,NC) ,
    % use 'set_extension' instead of 'value' for detyped ASTs
    DetypedAST =.. [set_extension,C,DetypedArg].
detype_ast(b(rec(FieldList),_,_),C,rec(C,DetypedFieldList),NC) :-
    C1 is C + 1 ,
    detype_set(FieldList,C1,DetypedFieldList,NC).
% quantifier
detype_ast(b(exists(IDList,Predicate),_,_),C,exists(C,DetypedIDList,DetypedPredicate),NC) :-
    C1 is C + 1 ,
    detype_set(IDList,C1,DetypedIDList,TempC) ,
    C2 is TempC + 1 ,
    detype_ast(Predicate,C2,DetypedPredicate,NC).
detype_ast(b(forall(IDList,LHS,RHS),_,_),C,forall(C,DetypedIDList,implication(ImpC,DetypedLHS,DetypedRHS)),NC) :-
    C1 is C + 1 ,
    detype_set(IDList,C1,DetypedIDList,TempC) ,
    ImpC is TempC + 1 ,
    C2 is ImpC + 1 ,
    detype_ast(LHS,C2,DetypedLHS,TempC2) ,
    C3 is TempC2 + 1 ,
    detype_ast(RHS,C3,DetypedRHS,NC).
% one argument nodes
detype_ast(b(Node,_,_),C,DetypedAST,NC) :-
    Node =.. [NodeType,Arg] ,
    C1 is C + 1 ,
    detype_ast(Arg,C1,DetypedArg,NC) ,
    DetypedAST =.. [NodeType,C,DetypedArg].
% two argument nodes
detype_ast(b(Node,_,_),C,DetypedAST,NC) :-
    Node =.. [NodeType,Arg1,Arg2] ,
    C1 is C + 1 ,
    detype_ast(Arg1,C1,DetypedArg1,TempC) ,
    C2 is TempC + 1 ,
    detype_ast(Arg2,C2,DetypedArg2,NC) ,
    DetypedAST =.. [NodeType,C,DetypedArg1,DetypedArg2].

% detype a set of ast or value nodes
detype_set([],C,[],NC) :-
    NC is C - 1.
detype_set([Node|R],C,[NewNode|NewR],NC) :-
    detype_ast(Node,C,NewNode,TempC) ,
    C1 is TempC + 1 ,
    detype_set(R,C1,NewR,NC).

% convert avl set to list set, otherwise just return the list set
avl_to_list_set(avl_set(AVL),Set) :-
    avl_to_list(AVL,TempSet) ,
    findall(Elm,member(Elm-_,TempSet),Set).
avl_to_list_set(Set,Set).

% get inner type and a list of the surrounding terms of an ast type
% for input like set(set(integer([]))) inner_type/3 will return
% [] and [set,set,integer] in its second and third argument
inner_type(Type,Inner,[H|Outter]) :-
    Type =.. [H,Temp] , ! ,
    inner_type(Temp,Inner,Outter).
inner_type(Inner,Inner,[]).

% surround type with terms from a list
surround_type(Inner,[],Inner).
surround_type(Inner,[Outter],New) :-
    New =.. [Outter,Inner].
surround_type(Inner,[Outter|T],New) :-
    surround_type(Inner,T,Temp) ,
    New =.. [Outter,Temp].
surround_type(Inner,_,Inner).

get_type_from_value(field(_,Value),Type) :-
    get_type_from_value(Value,Type).
get_type_from_value(int(_),integer).
get_type_from_value(string(_),string).
get_type_from_value(Bool,boolean) :-
    Bool = pred_true ; Bool = pred_false.
get_type_from_value(Seq,seq(Type)) :-
    is_list(Seq) , Seq = [Value|_] ,
    Value = (_,_) ,
    get_type_from_value(Value,Type).
get_type_from_value(Set,set(Type)) :-
    is_list(Set) , Set = [Value|_] ,
    get_type_from_value(Value,Type).
get_type_from_value(rec(FieldList),record(Type)) :-
    FieldList = [Value|_] ,
    get_type_from_value(Value,Type).

value_to_ast(int(Value),b(integer(Value),integer,[])).
value_to_ast(string(Value),b(string(Value),string,[])).
value_to_ast(pred_true,b(boolean_true,boolean,[])).
value_to_ast(pred_false,b(boolean_false,boolean,[])).
value_to_ast(Seq,b(value(Seq),seq(Type),[])) :-
    is_list(Seq) , Seq = [Value|_] ,
    Value = (_,_) ,
    get_type_from_value(Value,Type).
value_to_ast(Set,b(value(Set),set(Type),[])) :-
    is_list(Set) , Set = [Value|_] ,
    get_type_from_value(Value,Type).
value_to_ast(Record,b(Record,record(FieldTypes),[])) :-
    Record = rec(FieldList) ,
    findall(field(Name,Type),(member(field(Name,Value),FieldList) ,
        get_type_from_value(Value,Type)),FieldTypes).

% add solutions given as a list of bindings to an ast (smt-solver)
add_solution_to_ast(AST,Bindings,NewAST) :-
    add_solution_to_ast(AST,Bindings,empty_ast,NewAST).

add_solution_to_ast(AST,[],Acc,NewAcc) :-
    NewAcc = b(conjunct(AST,Acc),pred,[]).
add_solution_to_ast(AST,[binding(Name,Value,_RawValue)|Bindings],Acc,NewAST) :-
    value_to_ast(Value,ASTValue) ,
    ASTValue = b(_,Type,_) ,
    Node = b(equal(b(identifier(Name),Type,[]),ASTValue),pred,[]) ,
    (Acc = empty_ast ->
        NewAcc = Node
    ;   NewAcc = b(conjunct(Acc,Node),pred,[])) ,
    add_solution_to_ast(AST,Bindings,NewAcc,NewAST).

% check if prob ast node has "ground" value
% here ground means ast leave, one-element (or empty) set/seq or quantifier
prob_is_ground(b(Bool,pred,_),true) :-
    Bool = truth ; Bool = falsity.
prob_is_ground(b(Bool,boolean,_),true) :-
    Bool = boolean_true ; Bool = boolean_false.
prob_is_ground(b(identifier(_),_,_),true).
prob_is_ground(b(Int,integer,_),true) :-
    Int = max_int ; Int = min_int.
prob_is_ground(b(integer(_),integer,_),true).
prob_is_ground(b(unary_minus(b(integer(_),integer,[])),integer,_),true).
prob_is_ground(b(string(_),string,_),true).
prob_is_ground(b(rec([_]),_,_),true).
% one value or empty sets are ground
prob_is_ground(b(set_extension([]),set(_),_),true).
prob_is_ground(b(set_extension([_]),set(_),_),true).
prob_is_ground(b(sequence_extension([]),seq(_),_),true).
prob_is_ground(b(sequence_extension([_]),seq(_),_),true).
% handle exists and forall as "ground" to improve shrinking
% by allowing to evaluate those nodes
prob_is_ground(b(exists(_,_),_,_),true).
prob_is_ground(b(forall(_,_,_),_,_),true).
prob_is_ground(b(value([]),_,_),true).
prob_is_ground(b(value([_]),_,_),true).
prob_is_ground(b(value(avl_set(empty)),_,_),true).
prob_is_ground(b(value(avl_set(node(_,_,0,empty,empty))),_,_),true).
% true for small interval, used in shrinking
prob_is_ground(b(interval(b(integer(Value1),integer,_),b(integer(Value2),integer,_)),set(integer),_),true) :-
    C1 is Value1 + 5 ,
    C2 is Value2 - 5 ,
    C1 >= C2.
prob_is_ground(_,false).

% search identifier nodes within an ast and keep each nodes information
find_typed_identifier_uses_with_info(AST,UsedIDs) :-
    find_typed_identifier_uses_with_info_aux(AST,TempUsedIDs) ,
    remove_dups(TempUsedIDs,UsedIDs) , !.

find_typed_identifier_uses_with_info_aux(List,IDs) :-
    is_list(List) ,
    findall(ID,(member(ID,List) , ID = b(identifier(_),_,_)),IDs).
find_typed_identifier_uses_with_info_aux(b(identifier(IDName),Type,Info),[b(identifier(IDName),Type,Info)]).
find_typed_identifier_uses_with_info_aux(b(Node,_,_),UsedIDs) :-
    Node =.. [if_then_else,Condition,TrueOut,FalseOut] ,
    find_typed_identifier_uses_with_info_aux(Condition,ConditionIDs) ,
    find_typed_identifier_uses_with_info_aux(TrueOut,TrueOutIDs) ,
    find_typed_identifier_uses_with_info_aux(FalseOut,FalseOutIDs) ,
    append([ConditionIDs,TrueOutIDs,FalseOutIDs],UsedIDs).
find_typed_identifier_uses_with_info_aux(b(Node,_,_),UsedIDs) :-
    Node =.. [forall,_,Left,Right] ,
    find_typed_identifier_uses_with_info_aux(Left,LeftIDs) ,
    find_typed_identifier_uses_with_info_aux(Right,RightIDs) ,
    append([LeftIDs,RightIDs],UsedIDs).
find_typed_identifier_uses_with_info_aux(b(Node,_,_),UsedIDs) :-
    Node =.. [_,Arg] ,
    find_typed_identifier_uses_with_info_aux(Arg,UsedIDs).
find_typed_identifier_uses_with_info_aux(b(Node,_,_),UsedIDs) :-
    Node =.. [_,Arg1,Arg2] ,
    find_typed_identifier_uses_with_info_aux(Arg1,UsedIDs1) ,
    find_typed_identifier_uses_with_info_aux(Arg2,UsedIDs2) ,
    append(UsedIDs1,UsedIDs2,UsedIDs).
% rlevent
find_typed_identifier_uses_with_info_aux(b(Node,_,_),UsedIDs) :-
    Node =.. [_,_,_,_,_,Body|_] ,
    find_typed_identifier_uses_with_info_aux(Body,UsedIDs).
find_typed_identifier_uses_with_info_aux(_,[]).
