:- multifile generate/2.
:- multifile shrink/3.

% Options:
% names:ListOfUsedNames, list:S will generate S records using the same names
% size:S of record elements, 

generate(prob_value_record(Type),Record) :- 
    \+is_list(Type) , 
    generate(prob_value_record(Type,[]),Record).

% generate a list of records with the same field names but mixed values
generate(prob_value_record(Type,Options),ListOfRecords) :- 
    member(list:Size,Options) , 
    (member(names:NameList,Options)
    ->  true
    ;   random(1,20,R) , 
        generate(list(atom([size:5,alph]),[size:R]),NameList)) , 
    length(ListOfRecords,Size) , 
    maplist(generate(prob_value_record(Type,[names:NameList])),ListOfRecords).

% record 
generate(prob_value_record(Type,Options),rec(Record)) :- 
    member(Type,[integer(_),string(_),boolean(_),any(_)]) , 
    (member(size:Size,Options)
    ->  true
    ;   random(1,20,Size)) , 
    length(NType,Size) , 
    % any or given type
    (Type = any(InOptions)
    ->  maplist(generate(ground_type(InOptions)),NType) 
    ;   % list of single type to use maplist
        length(NType,Size) , 
        maplist(equal(Type),NType)) , 
    (member(names:NameList,Options)
    ->  length(NameList,NewSize) ,
        length(Record,NewSize) , 
        % size of name list is more substantial than given size
        length(NNType,NewSize) , 
        maplist(equal(Type),NNType) , 
        maplist(prob_field(value),NNType,NameList,Record) 
    ;   length(Record,Size) , 
        maplist(prob_field(value),NType,Record)). 

% shrink single record
shrink(Type,rec(Record),rec(Shrunken)) :- 
    Type =.. [prob_value_record|_] , 
    shrink(list(_),Record,Temp) , 
    % don't shrink to an empty record
    (Temp = [] -> Shrunken = Record , ! ; Shrunken = Temp).
% shrink list of records
shrink(Type,ListOfRecords,Shrunken) :- 
    Type =.. [prob_value_record|_] , 
    is_list(ListOfRecords) ,
    maplist(shrink(prob_value_record),ListOfRecords,Shrunken).