% evaluate ProB AST predicates

:- use_module(library(lists),[is_list/1]).
:- use_module(library(sets)).

pint(b(truth,pred,_),true).
pint(b(falsity,pred,_),false).

% conjunct, disjunct, implication, equivalence
pint(b(conjunct(Pred1,Pred2),pred,_),Value) :- 
    pint(Pred1,Value1) , 
    pint(Pred2,Value2) ,
    (Value1 , Value2
    ->  Value = true
    ;   Value = false).
pint(b(disjunct(Pred1,Pred2),pred,_),Value) :- 
    pint(Pred1,Value1) , 
    pint(Pred2,Value2) ,
    ((Value1 ; Value2)
    ->  Value = true
    ;   Value = false).
pint(b(implication(Pred1,Pred2),pred,_),Value) :- 
    pint(Pred1,Res1) , 
    pint(Pred2,Res2) , 
    ((\+Res1 ; Res2)
    ->  Value = true
    ;   Value = false).
pint(b(equivalence(Pred1,Pred2),pred,_),Value) :- 
    pint(Pred1,Res1) , 
    pint(Pred2,Res2) , 
    (Res1 = Res2
    ->  Value = true
    ;   Value = false).
pint(b(negation(Pred),pred,_),Value) :- 
    pint(Pred,Temp) , 
    (Temp
    ->  Value = false
    ;   Value = true).

%%%%%%%%%%%%
pint(b(finite(_),pred,_),true).
%%%%%%%%%%%%

% equal, not_equal
pint(b(equal(b(value(Set1),_,_),b(value(Set2),_,_)),pred,_),true) :- 
    Set1 == Set2.
pint(b(equal(b(value(Set1),_,_),b(value(Set2),_,_)),pred,_),false) :- 
    Set1 \== Set2.
pint(b(equal(b(set_extension(Set1),_,_),b(set_extension(Set2),_,_)),pred,_),true) :- 
    Set1 == Set2.
pint(b(equal(b(set_extension(Set1),_,_),b(set_extension(Set2),_,_)),pred,_),false) :- 
    Set1 \== Set2.

pint(b(equal(b(Expr1,integer,_),b(Expr2,integer,_)),pred,_),true) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) ,
    Value1 == Value2.
pint(b(equal(b(Expr1,integer,_),b(Expr2,integer,_)),pred,_),false) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) ,
    Value1 \== Value2.

pint(b(equal(Set1,Set2),pred,_),true) :- 
    Set1 == Set2.
pint(b(equal(Set1,Set2),pred,_),false) :- 
    Set1 \== Set2.

pint(b(not_equal(Pred1,Pred2),pred,_),true) :- 
    pint(b(equal(Pred1,Pred2),pred,_),false).
pint(b(not_equal(Pred1,Pred2),pred,_),false) :- 
    pint(b(equal(Pred1,Pred2),pred,_),true).

% less, less_equal, greater, greater_equal
pint(b(less_equal(Expr1,Expr2),pred,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    (Value1 =< Value2
    ->  Value = true
    ;   Value = false).
pint(b(less(Expr1,Expr2),pred,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    (Value1 < Value2
    ->  Value = true
    ;   Value = false).
pint(b(greater_equal(Expr1,Expr2),pred,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    (Value1 >= Value2
    ->  Value = true
    ;   Value = false).
pint(b(greater(Expr1,Expr2),pred,_),Value) :- 
    int(Expr1,Value1) , 
    int(Expr2,Value2) , 
    (Value1 > Value2
    ->  Value = true
    ;   Value = false).

% member, not_member
pint(b(member(Set,SetOfSet),pred,_),Value) :- 
    Set = b(_,set(_),_) ,
    SetOfSet = b(_,set(set(_)),_) ,
    sint(Set,ValueList) ,
    sint(SetOfSet,SetValue) ,
    (member(ValueList,SetValue)
    ->  Value = true
    ;   Value = false).
pint(b(member(b(Elm,integer,_),b(set_extension(Set),_,_)),pred,_),Value) :- 
    maplist(ast_to_value,Set,List) ,
    int(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,List)
    ->  Value = true
    ;   Value = false).
pint(b(member(b(Elm,integer,_),b(set_extension(Set),_,_)),pred,_),Value) :- 
    int(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,Set)
    ->  Value = true
    ;   Value = false).
% value list set
pint(b(member(b(Elm,integer,_),b(value(Set),_,_)),pred,_),Value) :- 
    is_list(Set) ,
    int(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,Set)
    ->  Value = true
    ;   Value = false).
% value avl set
pint(b(member(b(Elm,integer,_),b(value(avl_set(Set)),_,_)),pred,_),Value) :- 
    int(b(Elm,_,_),ElmValue) ,
    avl_to_list(Set,TempList) , 
    findall(Key,member(Key-_,TempList),List) ,
    (member(ElmValue,List)
    ->  Value = true
    ;   Value = false).
pint(b(member(b(Elm,_,_),b(set_extension(Set),_,_)),pred,_),Value) :- 
    maplist(ast_to_value,Set,List) ,
    sint(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,List)
    ->  Value = true
    ;   Value = false).
pint(b(member(b(Elm,_,_),b(set_extension(Set),_,_)),pred,_),Value) :- 
    sint(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,Set)
    ->  Value = true
    ;   Value = false).
% value list set
pint(b(member(b(Elm,_,_),b(value(Set),_,_)),pred,_),Value) :- 
    is_list(Set) ,
    sint(b(Elm,_,_),ElmValue) ,
    (member(ElmValue,Set)
    ->  Value = true
    ;   Value = false).
% value avl set
pint(b(member(b(Elm,_,_),b(value(avl_set(Set)),_,_)),pred,_),Value) :- 
    sint(b(Elm,_,_),ElmValue) ,
    avl_to_list(Set,TempList) , 
    findall(Key,member(Key-_,TempList),List) ,
    (member(ElmValue,List)
    ->  Value = true
    ;   Value = false).

pint(b(not_member(Elm,Set),pred,_),Value) :- 
    pint(b(member(Elm,Set),pred,_),R) ,
    (R = true
    ->  Value = false
    ;   Value = true).

% subset, not_subset,
pint(b(subset(Set1,Set2),pred,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    (subset(ValueSet1,ValueSet2)
    ->  Value = true
    ;   Value = false).
pint(b(not_subset(Set1,Set2),pred,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    (\+subset(ValueSet1,ValueSet2)
    ->  Value = true
    ;   Value = false).
    
% subset_strict, not_subset_strict
pint(b(subset_strict(Set1,Set2),pred,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    (subset_strict(ValueSet1,ValueSet2)
    ->  Value = true
    ;   Value = false).
pint(b(not_subset_strict(Set1,Set2),pred,_),Value) :- 
    sint(Set1,ValueSet1) , 
    sint(Set2,ValueSet2) , 
    (subset(ValueSet1,ValueSet2)
    ->  Value = false
    ;   Value = true).

%%% placeholder, needs some improvement 
pint(b(exists(_,_),_,_),true).
pint(b(forall(_,_,_),_,_),true).

% check if set is strict subset 
subset_strict([],[]).
subset_strict(Subset,Set) :- 
    append(Subset,_,Set).
subset_strict(Set,[_|T]) :- 
    subset_strict(Set,T).