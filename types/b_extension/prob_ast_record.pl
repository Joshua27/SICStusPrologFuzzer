:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).

% list of options: 
% size:Size to set the size of record, names:NameList to specify the used names in the record
% if both Options are given the length of NameList will determine the size of the record
%
% the optional declaration of a NameList is useful to be able to generate 
% different records with the same field names, so the ProB Interpreter is able to 
% compare two records with the chance of success (different names are also accepted, but those
% comparisons would probably never succeed with the addition of random selected names)

generate(prob_ast_record(Type),Record) :- 
    \+is_list(Type) , 
    generate(prob_ast_record(Type,[]),Record).

% generate a list of records with the same field names but mixed values
generate(prob_ast_record(Type,Options),ListOfRecords) :- 
    member(list:Size,Options) , 
    (member(names:NameList,Options)
    ->  true
    ;   (member(size:R,Options)
        ->  true
        ;   random(1,20,R)) ,  
        generate(list(atom([size:5,alph]),[size:R]),NameList)) , 
    length(ListOfRecords,Size) , 
    maplist(generate(prob_ast_record(Type,[names:NameList])),ListOfRecords).

% record 
generate(prob_ast_record(Type,Options),b(rec(Record),record(FieldTypes),[])) :- 
    member(Type,[integer(_),string(_),boolean(_),any(_)]) , 
    (member(size:Size,Options)
    ->  true
    ;   random(1,20,Size)) , 
    length(NType,Size) , 
    % any or given type
    (Type = any(InOptions)
    ->  maplist(generate(ground_type(InOptions)),NType) 
    ;   length(NType,Size) , 
        maplist(equal(Type),NType)) , 
    (member(names:NameList,Options) 
    ->  length(NameList,NewSize) ,
        length(Record,NewSize) , 
        % size of name list is more substantial than given size
        length(NNType,NewSize) , 
        maplist(equal(Type),NNType) , 
        maplist(prob_field(ast),NNType,NameList,Record) 
    ;   % list of single type to use maplist
        length(Record,Size) , 
        maplist(prob_field(ast),NType,Record)) ,
    maplist(field_value_to_type,Record,FieldTypes).

equal(A,A).

% shrink single record
shrink(Type,b(rec(Record),RecType,Info),b(rec(Shrunken),RecType,Info)) :- 
    Type =.. [prob_ast_record|_] , 
    shrink(list(_),Record,Temp) , 
    % don't shrink to an empty record
    (Temp = [] -> Shrunken = Record , ! ; Shrunken = Temp).
% shrink list of records
shrink(Type,ListOfRecords,Shrunken) :- 
    Type =.. [prob_ast_record|_] , 
    is_list(ListOfRecords) ,
    maplist(shrink(prob_ast_record),ListOfRecords,Shrunken).

% generation of a field used in records
% not defined within generate/2 to be able to use maplist with list of names
% ProBType is either ast or value 
prob_field(ProBType,Type,field(VarName,FieldValue)) :- 
    generate(atom([alph,size:5]),VarName) , 
    gen_type(Type,ProBType,NType) , 
    generate(NType,FieldValue).
prob_field(ProBType,Type,VarName,field(VarName,FieldValue)) :- 
    gen_type(Type,ProBType,NType) , 
    generate(NType,FieldValue).

% abstract value to its type in a field
field_value_to_type(field(VarName,FieldValue),field(VarName,FieldType)) :- 
    FieldValue = b(_,FieldType,_).