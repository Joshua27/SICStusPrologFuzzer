:- multifile generate/2.
:- multifile shrink/3.

generate(prob_ast_sat(Options),ResultPred) :- 
    generate(prob_ast_pred(Options),Pred) , 
    get_id_amount_from_options(Options,AmountOfIDs) ,
    insert_identifier_to_ast(Pred,AmountOfIDs,NewPred) ,  % defined in 'snippets.pl'
    set_identifier_constraints(NewPred,Options,ConstrainedPred) , 
    extend_predicate_if_necessary(ConstrainedPred,AmountOfIDs,Options,ResultPred).

set_identifier_constraints(Pred,Options,b(conjunct(Pred,Constraints),pred,[])) :- 
    generate(prob_state_bindlist(Pred),_:ListOfIdentifier) , 
    generate(prob_ast_id_constraints(ListOfIdentifier,Options),Constraints).

get_id_amount_from_options(Options,AmountOfIDs) :- 
    get_min_max_and_type(Options,(Min,Max),interval) , ! , 
    NMax is Max + 1 , 
    random(Min,NMax,AmountOfIDs).
get_id_amount_from_options(Options,AmountOfIDs) :- 
    get_min_max_and_type(Options,(Min,_),min) , ! , 
    random(0,10,R) , 
    AmountOfIDs is Min + R.
get_id_amount_from_options(Options,AmountOfIDs) :- 
    get_min_max_and_type(Options,(_,Max),max) , ! , 
    random(0,Max,AmountOfIDs). 
get_id_amount_from_options(_,AmountOfIDs) :- 
    random(0,10,AmountOfIDs).

order_min_max(Min,Max,Min,Max) :- 
    Min =< Max.
order_min_max(Min,Max,Max,Min) :- 
    Min > Max.

get_min_max_and_type(Options,(NMin,NMax),interval) :- 
    subset([minID:Min,maxID:Max],Options) , 
    order_min_max(Min,Max,NMin,NMax).
get_min_max_and_type(Options,(Min,_),min) :- 
    member(minID:Min,Options).
get_min_max_and_type(Options,(_,Max),max) :- 
    member(maxID:Max,Options).

valid_pred_according_to_options(CurAmountOfIDs,_,Options) :- 
    get_min_max_and_type(Options,(Min,Max),interval) , 
    CurAmountOfIDs >= Min , CurAmountOfIDs =< Max.
valid_pred_according_to_options(CurAmountOfIDs,AmountOfIDs,Options) :- 
    CurAmountOfIDs >= AmountOfIDs , 
    get_min_max_and_type(Options,_,min).
valid_pred_according_to_options(CurAmountOfIDs,AmountOfIDs,Options) :- 
    CurAmountOfIDs =< AmountOfIDs , 
    get_min_max_and_type(Options,_,max).

% generate new predicate to conjunct with the current one if the minimum 
% amount of identifiers is not reached
extend_predicate_if_necessary(Pred,AmountOfIDs,Options,ResultPred) :- 
    bsyntaxtree:find_identifier_uses(Pred,[],IDs) , 
    length(IDs,CurAmountOfIDs) , 
    valid_pred_according_to_options(CurAmountOfIDs,AmountOfIDs,Options) , ! , 
    MissingAmount is AmountOfIDs - CurAmountOfIDs , 
    extend_predicate_if_necessary_aux(Pred,AmountOfIDs,MissingAmount,IDs,Options,ResultPred).
extend_predicate_if_necessary(Pred,_,_,Pred).

extend_predicate_if_necessary_aux(Pred,_,0,_,_,Pred).
extend_predicate_if_necessary_aux(Pred,AmountOfIDs,MissingAmount,_IDs,Options,ResultPred) :- 
    generate(prob_ast_pred(Options),NewPred) , 
    % Todo: evaluate using previous identifier names (or generate new one based on random choice) 
    % but the predicates seem complex enough for now
    insert_identifier_to_ast(NewPred,MissingAmount,NewPredWithIDs) , 
    extend_predicate_if_necessary(b(conjunct(Pred,NewPredWithIDs),pred,[]),AmountOfIDs,Options,ResultPred).

shrink(Type,Value,Value) :- 
    Type =.. [prob_ast_sat|_].
