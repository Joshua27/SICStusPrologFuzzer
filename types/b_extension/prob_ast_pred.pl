:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3,random_member/2]).

generate(prob_ast_pred,Value) :- 
    generate(prob_ast_pred([]),Value).

% Options:
% not-well-defined
% all options of any type are accepted and will be applied when valid

% Pred :: boolean
generate(prob_ast_pred(Options),Value) :- 
    PredList = [conjunct,disjunct,implication,equivalence,negation,finite,
                equal,not_equal,less_equal,less,greater_equal,greater,member,
                not_member,subset,not_subset,subset_strict,not_subset_strict] , %truth,falsity
    (member(noQuantifier,Options)
    ->  Predicates = PredList
    ;   append(PredList,[exists,forall],Predicates)) , 
    random_member(Pred,Predicates) , 
    generate(prob_ast_pred(Pred,Options),Value).

% specific predicate
generate(prob_ast_pred(truth,_),b(truth,pred,[])).
generate(prob_ast_pred(falsity,_),b(falsity,pred,[])).

generate(prob_ast_pred(finite,Options),b(finite(Set),pred,[])) :- 
    generate(prob_ast_set(_,[any|Options]),Set).

generate(prob_ast_pred(negation,Options),b(negation(Value),pred,[])) :- 
    generate(prob_ast_pred(Options),Value).

generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[conjunct,disjunct,implication,equivalence]) ,
    generate(prob_ast_pred(Options),Value1) ,
    generate(prob_ast_pred(Options),Value2) ,
    NewPred =.. [Pred,Value1,Value2].

generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[member,not_member]) , 
    generate(ground_type,Temp) ,
    Temp =.. [TypeNoOpt|_] , 
    Type =.. [TypeNoOpt,Options] , 
    gen_type(Type,ast,NType) , 
    generate(NType,Value) ,
    generate(prob_ast_set(Type,Options),Set) ,
    NewPred =.. [Pred,Value,Set].

generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[subset,not_subset,subset_strict,not_subset_strict]) , 
    generate(prob_ast_set_expr(Options),Value1) ,
    Value1 = b(_,set(Type),_) ,
    (Type = empty
    ->  generate(ground_type,NType)
    ;   inner_type(Type,Inner,Outter) , 
        Temp =.. [Inner,[]] ,   % no options
        surround_type(Temp,Outter,NType)) ,
    generate(prob_ast_set(NType,Options),Value2) ,
    NewPred =.. [Pred,Value1,Value2].

% generate two records of the same type using the same field names with random values
generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[equal,not_equal]) , 
    member(record,Options) , ! , 
    generate(ground_type,RecordType) , 
    generate(prob_ast_record(RecordType,[list:2]),[Value1,Value2]) , 
    NewPred =.. [equal,Value1,Value2].

generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[equal,not_equal]) ,  
    % generate any value for equal, not_equal
    % make sure both values are of the same type
    generate(prob_ast_expr(Options),Value1) , 
    Value1 = b(_,Type,_) , 
    inner_type(Type,Inner,Outter) ,
    % if empty structure then just use an integer 
    (Inner = empty -> NInner = integer ; NInner = Inner) ,  
    Temp =.. [NInner,Options] , 
    surround_type(Temp,Outter,TempType) , 
    gen_type(TempType,ast,NType) ,
    generate(NType,Value2) , 
    NewPred =.. [Pred,Value1,Value2].

generate(prob_ast_pred(Pred,Options),b(NewPred,pred,[])) :- 
    member(Pred,[greater,greater_equal,less,less_equal]) , 
    generate(prob_ast_int_expr([small|Options]),Value1) ,
    generate(prob_ast_int_expr([small|Options]),Value2) ,
    NewPred =.. [Pred,Value1,Value2].

% quantifier
generate(prob_ast_pred(exists,Options),b(exists(ListOfIDNodes,NewPredicate),pred,[used_ids(UsedIDs)])) :-  
    (member(id,Options)
    ->  generate(prob_ast_pred(Options),Predicate)
    ;   generate(prob_ast_pred([id|Options]),Predicate)) , 
    % get a list of all used identifier nodes
    generate(prob_state_bindlist(Predicate),_:TempListOfIDNodes) , 
    % if there are no identifier just insert one instead of backtracking
    (TempListOfIDNodes = []
    ->  insert_identifier_to_ast(Predicate,2,NewPredicate) , 
        generate(prob_state_bindlist(NewPredicate),_:ListOfIDNodes)
    ;   TempListOfIDNodes = ListOfIDNodes , 
        NewPredicate = Predicate) , 
    b_ast_cleanup:find_identifier_uses(NewPredicate,[],UsedIDs).

generate(prob_ast_pred(forall,Options),b(forall(ListOfIDNodes,IDConstraints,NewPredicate),pred,[used_ids(UsedIDs)])) :- 
    (member(id,Options)
    ->  generate(prob_ast_pred(Options),Predicate)
    ;   generate(prob_ast_pred([id|Options]),Predicate)) , 
    % get a list of all used identifier nodes
    generate(prob_state_bindlist(Predicate),_:TempListOfIDNodes) , 
    (TempListOfIDNodes = []
    ->  insert_identifier_to_ast(Predicate,2,NewPredicate) , 
        generate(prob_state_bindlist(NewPredicate),_:ListOfIDNodes)
    ;   TempListOfIDNodes = ListOfIDNodes , 
        NewPredicate = Predicate) , 
    b_ast_cleanup:find_identifier_uses(NewPredicate,[],UsedIDs) , 
    % set constraints for every identifier (except of boolean, string) 
    % defined in prob_ast_identifier.pl
    generate(prob_ast_id_constraints(ListOfIDNodes),IDConstraints).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_pred|_] ,
    minimize_pred(Value,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_pred|_] ,
    get_inner_pred(Value,Shrunken).