% minimize ProB AST sequence expressions for shrinking stage

minimize_seq_expr(b(sequence_extension(Seq),seq(Type),Info),b(sequence_extension(ShrunkenSeq),seq(Type),Info)) :- 
    is_list(Seq) , 
    shrink(list(_),Seq,ShrunkenSeq).

minimize_seq_expr(b(value(Seq),seq(Type),Info),Shrunken) :- 
    shrink(prob_value_seq,Seq,ShrunkenSeq) ,
    Shrunken = b(value(ShrunkenSeq),seq(Type),Info).

minimize_seq_expr(b(sequence_extension([]),seq(empty),Info),b(sequence_extension([]),seq(empty),Info)).

minimize_seq_expr(b(general_concat(SeqOfSeq),seq(Type),Info),Shrunken) :- 
    (prob_is_ground(SeqOfSeq,true)
    ->  seqint_avl(b(general_concat(SeqOfSeq),seq(Type),Info),ValueAvl) , 
        Shrunken = b(value(ValueAvl),seq(Type),Info)
    ;   SeqOfSeq = b(sequence_extension(Seq),seq(OutterType),OutterInfo) ,
        maplist(minimize_seq_expr,Seq,ShrunkenSeq) , 
        Shrunken = b(general_concat(b(sequence_extension(ShrunkenSeq),seq(OutterType),OutterInfo)),seq(Type),Info)).

minimize_seq_expr(b(Expression,seq(Type),Info),Shrunken) :- 
    Expression =.. [Expr,Arg] , 
    member(Expr,[front,tail,rev]) , 
    (prob_is_ground(Arg,true)
    ->  seqint_avl(b(Expression,seq(Type),Info),ValueAvl) , 
        (ValueAvl = avl_set(empty) -> 
            % no empty set for well-definedness
            Shrunken = b(Expression,seq(Type),Info)
        ;   Shrunken = b(value(ValueAvl),seq(Type),Info)) 
    ;   minimize_seq_expr(Arg,NewArg) , 
        NewExpr =.. [Expr,NewArg] , 
        Shrunken = b(NewExpr,seq(Type),Info)).

minimize_seq_expr(b(Expression,seq(Type),Info),Shrunken) :- 
    Expression =.. [Expr,Seq,Value] , 
    member(Expr,[restrict_front,restrict_tail]) ,
    (prob_is_ground(Seq,true)
    ->  seqint_avl(b(Expression,seq(Type),Info),ValueAvl) , 
        Shrunken = b(value(ValueAvl),seq(Type),Info)
    ;   minimize_seq_expr(Seq,NewArg) , 
        % decrease restriction value by one after every shrinking call
        % to keep well-definedness
        Value = b(integer(Val),integer,IntInfo) , 
        (Val = 0 -> NVal = Val ; NVal is Val - 1) , 
        NewValue = b(integer(NVal),integer,IntInfo) , 
        NewExpr =.. [Expr,NewArg,NewValue] , 
        Shrunken = b(NewExpr,seq(Type),Info)).

minimize_seq_expr(b(concat(Seq1,Seq2),seq(Type),Info),b(value(ValueSeq),seq(Type),Info)) :- 
    prob_is_ground(Seq1,true) , 
    prob_is_ground(Seq2,true) , 
    seqint_avl(b(concat(Seq1,Seq2),seq(Type),Info),ValueSeq).
minimize_seq_expr(b(concat(Seq1,Seq2),seq(Type),Info),b(concat(NewSeq1,NewSeq2),seq(Type),Info)) :- 
    prob_is_ground(Seq1,Res1) , 
    prob_is_ground(Seq2,Res2) , 
    ( Res1 = true , Res2 = false
    ->  minimize_seq_expr(Seq2,NewSeq2) , 
        NewSeq1 = Seq1
    ; Res1 = false , Res2 = true
    ->  minimize_seq_expr(Seq1,NewSeq1) , 
        NewSeq2 = Seq2
    ;   minimize_seq_expr(Seq1,NewSeq1) , 
        minimize_seq_expr(Seq2,NewSeq2)).

minimize_seq_expr(b(insert_front(Value,Seq),seq(Type),Info),Shrunken) :- 
    (prob_is_ground(Seq,true)
    ->  seqint_avl(b(insert_front(Value,Seq),seq(Type),Info),ValueAvl) , 
        Shrunken = b(value(ValueAvl),seq(Type),Info)
    ;   minimize_seq_expr(Seq,NewSeq) , 
        NewExpr =.. [insert_front,Value,NewSeq] , 
        Shrunken = b(NewExpr,seq(Type),Info)).

minimize_seq_expr(b(insert_tail(Seq,Value),seq(Type),Info),Shrunken) :- 
    (prob_is_ground(Seq,true)
    ->  seqint_avl(b(insert_tail(Value,Seq),seq(Type),Info),ValueAvl) , 
        Shrunken = b(value(ValueAvl),seq(Type),Info)
    ;   minimize_seq_expr(Seq,NewSeq) , 
        NewExpr =.. [insert_tail,NewSeq,Value] , 
        Shrunken = b(NewExpr,seq(Type),Info)).

% skip identifier
minimize_seq_expr(b(identifier(Name),Type,Info),b(identifier(Name),Type,Info)).