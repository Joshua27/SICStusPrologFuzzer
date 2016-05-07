:- multifile generate/2.
:- multifile shrink/3.

% any expression
generate(prob_ast_expr(Option),Value) :- 
    random_member(Type,[prob_ast_set_expr(Option),prob_ast_int_expr(Option),prob_ast_seq_expr(Option)]),
    generate(Type,Value).

% differenciate between record, integer, set or sequence expression
shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_expr|_] , 
    Value = b(_,record(_),_) , 
    shrink(prob_ast_record,Value,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_expr|_] , 
    Value = b(_,set(_),_) , 
    shrink(prob_ast_set_expr,Value,Shrunken).
  
shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_expr|_] , 
    Value = b(_,seq(_),_) , 
    shrink(prob_ast_seq_expr,Value,Shrunken).

shrink(Type,Value,Shrunken) :- 
    Type =.. [prob_ast_expr|_] , 
    Value = b(_,integer,_) , 
    shrink(prob_ast_int_expr,Value,Shrunken).