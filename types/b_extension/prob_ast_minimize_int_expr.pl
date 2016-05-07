% minimize ProB AST integer expressions for shrinking stage

minimize_int_expr(b(integer(Value),integer,[]),b(integer(Value),integer,[])).

% add, minus, multiplication, div, power_of, modulo
% shrink expression to its value when both arguments are integer values
minimize_int_expr(b(Expression,integer,Info),Shrunken) :- 
    Expression =.. [Type,Arg1,Arg2] ,
    member(Type,[add,minus,multiplication,div,power_of,modulo]) ,
    prob_is_ground(Arg1,true) , 
    prob_is_ground(Arg2,true) , 
    int(b(Expression,integer,Info),Value) ,
    Shrunken = b(integer(Value),integer,Info).
minimize_int_expr(b(Expression,integer,Info),Shrunken) :- 
    Expression =.. [Type,Arg1,Arg2] ,
    member(Type,[add,minus,multiplication,div,power_of,modulo]) ,
    % hold integer value and minimize expression
    (prob_is_ground(Arg1,true) , prob_is_ground(Arg2,false)
    ->  minimize_int_expr(Arg2,NewArg2) ,
        NewExpr =.. [Type,Arg1,NewArg2] ,
        Shrunken = b(NewExpr,integer,Info)
    ;   prob_is_ground(Arg1,false) , prob_is_ground(Arg2,true)
    ->  minimize_int_expr(Arg1,NewArg1) ,
        NewExpr =.. [Type,NewArg1,Arg2] ,
        Shrunken = b(NewExpr,integer,Info)
    ;   % minimize both expressions
        minimize_int_expr(Arg1,NewArg1) ,
        minimize_int_expr(Arg2,NewArg2) ,
        NewExpr =.. [Type,NewArg1,NewArg2] ,
        Shrunken = b(NewExpr,integer,Info)).

% unary_minus
minimize_int_expr(b(unary_minus(Expression),integer,Info),Shrunken) :- 
    (Expression = b(integer(Value),integer,_)
    ->  Temp is -(Value) , 
        Shrunken = b(integer(Temp),integer,Info)
    ;   minimize_int_expr(Expression,Temp) , 
        Shrunken = b(unary_minus(Temp),integer,Info)).

% card
/*minimize_int_expr(b(card(Set),integer,Info),Shrunken) :- 
    (prob_is_ground(Set,true) ->
        int(b(card(Set),integer,Info),Value) , 
        Shrunken = b(integer(Value),integer,Info)
    ;
        minimize_set_expr(Set,ShrunkenSet) , 
        Shrunken = b(card(ShrunkenSet),integer,Info)).*/

% max_int, min_int
minimize_int_expr(b(Expression,integer,Info),Shrunken) :- 
    member(Expression,[max_int,min_int]) , 
    int(b(Expression,integer,Info),Value) , 
    Shrunken = b(integer(Value),integer,Info).

% max, min
minimize_int_expr(b(Expression,integer,Info),Shrunken) :- 
    Expression =.. [Type,Set] ,
    member(Type,[max,min]) , 
    (prob_is_ground(Set,true)
    ->  int(b(Expression,integer,Info),Val) , 
        Shrunken = b(integer(Val),integer,[])
    ;   minimize_set_expr(Set,Temp) ,
        % no empty set in max, min for well-definedness
        ((Temp = [] ; Temp = avl_set(empty))
        ->  ShrunkenSet = Set
        ;   ShrunkenSet = Temp) ,
        NewExpr =.. [Type,ShrunkenSet] , 
        Shrunken = b(NewExpr,integer,Info)).

% skip identifier
minimize_int_expr(b(identifier(Name),Type,Info),b(identifier(Name),Type,Info)).

% get the deepest value or expression of an prob ast expression
get_inner_expr(b(Expression,Type,Info),Shrunken) :- 
    Expression =.. [_,Arg1,Arg2] ,
    prob_is_ground(Arg1,Res1) , prob_is_ground(Arg2,Res2) , 
    ( Res1 = true , Res2 = true
    ->  % done
        Shrunken = b(Expression,Type,Info)
    ; % get inner expression of second argument
      Res1 = true , Res2 = false
    ->  get_inner_expr(Arg2,Shrunken)
    ; % both arguments are expressions
      Res1 = false , Res2 = false
    ->  % random choice heuristic
        random_member(Argument,[Arg1,Arg2]) ,
        get_inner_expr(Argument,Shrunken)  
    ; % get inner expression of first argument
        get_inner_expr(Arg1,Shrunken)).
    
get_inner_expr(Expr,Expr).