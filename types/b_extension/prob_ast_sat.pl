:- multifile generate/2.
:- multifile shrink/3.

% generate B predicates with constraints for testing SMT-Solver
generate(prob_ast_sat(Options),ConstraintPred) :- 
    generate(prob_ast_pred(Options),Pred) ,  
    (member(maxID:C,Options)
    ->  R = C
    ;   random(1,5,R)) , 
    insert_identifier_to_ast(Pred,R,NewPred) ,  % defined in 'snippets.pl'
    generate(prob_state_bindlist(NewPred),_:ListOfIdentifier) , 
    generate(prob_ast_id_constraints(ListOfIdentifier,Options),Constraints) , 
    ConstraintPred = b(conjunct(Constraints,NewPred),pred,[]).

shrink(Type,Value,Value) :- 
    Type =.. [prob_ast_sat|_].  