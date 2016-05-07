% evaluate ProB AST sequence expressions

% returns indexed avl_set
seqint_avl(Expression,avl_set(AVL)) :- 
    seqint(Expression,Value) , 
    gen_indexed_couple_list(1,Value,CoupleList) ,
    findall(Key-true,member(Key,CoupleList),AVLList) , 
    list_to_avl(AVLList,AVL).

seqint(b(value(Seq),seq(_),_),Seq) :- 
    is_list(Seq).

seqint(b(value(avl_set(Seq)),seq(_),_),Value) :- 
    avl_to_list(Seq,List) , 
    findall(Key,member(Key-true,List),Value).

seqint(b(sequence_extension([Seq]),seq(seq(_)),_),[Value]) :- 
    seqint(Seq,Value).

seqint(b(sequence_extension(Set),seq(_),_),Value) :- 
    maplist(ast_to_prob_value,Set,Value).

seqint(b(general_concat(SeqOfSeq),seq(_),_),Value) :- 
    seqint(SeqOfSeq,Temp) , 
    flatten(Temp,Value).

seqint(b(front(Seq),seq(_),_),Value) :- 
    seqint(Seq,Temp) ,
    remove_last(Temp,Value).

seqint(b(tail(Seq),seq(_),_),Value) :- 
    seqint(Seq,[_|Value]).

seqint(b(rev(Seq),seq(_),_),Value) :- 
    seqint(Seq,Temp) , 
    reverse(Temp,Value).

seqint(b(restrict_front(Seq,RestrictBy),seq(_),_),Value) :- 
    seqint(Seq,Temp) , 
    int(RestrictBy,RestrictValue) , 
    get_first_n(Temp,RestrictValue,Value).

seqint(b(restrict_tail(Seq,RestrictBy),seq(_),_),Value) :- 
    seqint(Seq,Temp) , 
    int(RestrictBy,RestrictValue) , 
    rem_first_n(Temp,RestrictValue,Value).

seqint(b(concat(Seq1,Seq2),seq(_),_),Value) :- 
    seqint(Seq1,ValueSeq1) , 
    seqint(Seq2,ValueSeq2) , 
    append(ValueSeq1,ValueSeq2,Value).

seqint(b(insert_tail(Val,Seq),seq(_),_),Value) :- 
    sint(Val,Insert) , 
    seqint(Seq,ValueSeq) , 
    ( Insert = true -> NInsert = [pred_true] 
    ; Insert = false -> NInsert = [pred_false] 
    ; NInsert = [Insert]) , 
    append(NInsert,ValueSeq,Value).

seqint(b(insert_front(Seq,Val),seq(_),_),Value) :- 
    sint(Val,Insert) , 
    seqint(Seq,ValueSeq) , 
    ( Insert = true -> NInsert = [pred_true] 
    ; Insert = false -> NInsert = [pred_false] 
    ; NInsert = [Insert]) , 
    append(ValueSeq,NInsert,Value).

% return the first n elements of a list 
get_first_n([],_,[]).
get_first_n(_,0,[]).
get_first_n([H|T],N,[H|NT]) :-
    N1 is N - 1 ,  
    get_first_n(T,N1,NT).

% removes the first n elements of a list
rem_first_n([],_,[]).
rem_first_n(L,0,L).
rem_first_n([_|T],N,NL) :- 
    N1 is N - 1 , 
    rem_first_n(T,N1,NL).