:- multifile generate/2.

% shrinking of all mutations defined in mutation.pl

:- use_module(library(random),[random/3]).
:- use_module(library(lists),[is_list/1]).

% mutation of ProB ast sequence expressions
generate(mutation(Expression:prob_ast_seq_expr),NewExpression) :- 
    \+is_list(Expression) , 
    random_seq_expr_mutation(Expression,NewExpression).
% list of set expressions, random concatenation of expression then 
% mutation of consisting sequence expressions
generate(mutation(Expressions:prob_ast_seq_expr),NewExpression) :-
    maplist(random_seq_expr_mutation,Expressions,Mutated) , 
    concatenate_ast(Mutated,[concat],NewExpression). 


% mutation of ProB value sequences
% value avl seq
generate(mutation(avl_set(Seq):prob_value_seq),avl_set(Value)) :- 
    avl_to_list(Seq,AVLList) , 
    % remove indices to generate new ones after mutation
    % to get well-definedness
    findall(Key,member(Key-_,AVLList),Temp) ,
    seq_to_list(Temp,SeqValues) , 
    random_permutation(SeqValues,Permutation) ,  
    gen_indexed_couple_list(1,Permutation,CoupleList) , 
    findall(Key-true,member(Key,CoupleList),ValueAVL) ,
    list_to_avl(ValueAVL,Value).
generate(mutation([H|ListOfSeq]:prob_value_seq),avl_set(Value)) :- 
    H = avl_set(_) , 
    findall(Tree,member(avl_set(Tree),[H|ListOfSeq]),Seq) ,
    maplist(avl_to_list,Seq,Temp) , 
    maplist(avl_list_to_list,Temp,Temp2) , 
    maplist(seq_to_list,Temp2,List) , 
    flatten(List,SeqValues) , 
    random_permutation(SeqValues,Permutation) , 
    gen_indexed_couple_list(1,Permutation,CoupleList) , 
    findall(Key-true,member(Key,CoupleList),ValueAVL) ,
    list_to_avl(ValueAVL,Value).

% value list seq
generate(mutation(Seq:prob_value_seq),Value) :- 
    flattened(Seq) ,  
    seq_to_list(Seq,SeqValues) , 
    random_permutation(SeqValues,Permutation) , 
    gen_indexed_couple_list(1,Permutation,Value).
generate(mutation(ListOfSeq:prob_value_seq),Value) :- 
    maplist(seq_to_list,ListOfSeq,Temp) , 
    flatten(Temp,SeqValues) , 
    random_permutation(SeqValues,Permutation) , 
    gen_indexed_couple_list(1,Permutation,Value).

% front, tail, rev
random_seq_expr_mutation(b(Expr,SeqType,Info),b(NewExpr,SeqType,Info)) :- 
    Expr =.. [Type,Seq] , 
    member(Type,[front,tail,rev]) , 
    mutate_seq(Seq,NewSeq) ,  
    NewExpr =.. [Type,NewSeq].

% restrict_front, restrict_tail
random_seq_expr_mutation(b(Expr,SeqType,Info),b(NewExpr,SeqType,Info)) :- 
    Expr =.. [Type,Seq,Restriction] , 
    member(Type,[restrict_front,restrict_tail]) , 
    mutate_seq(Seq,NewSeq) ,  
    NewExpr =.. [Type,NewSeq,Restriction].

% concat
random_seq_expr_mutation(b(concat(Expr1,Expr2),SeqType,Info),b(concat(NewExpr1,NewExpr2),SeqType,Info)) :- 
    random_seq_expr_mutation(Expr1,NewExpr1) , 
    random_seq_expr_mutation(Expr2,NewExpr2).

% insert_front
random_seq_expr_mutation(b(insert_front(Value,Expr),SeqType,Info),b(insert_front(Value,NewExpr),SeqType,Info)) :- 
    random_seq_expr_mutation(Expr,NewExpr).

% insert_tail
random_seq_expr_mutation(b(insert_tail(Expr,Value),SeqType,Info),b(insert_tail(NewExpr,Value),SeqType,Info)) :- 
    random_seq_expr_mutation(Expr,NewExpr).

% general_concat
random_seq_expr_mutation(b(general_concat(b(sequence_extension(InnerSeq),seq(SeqType),Info)),SeqType,OutterInfo),Mutation) :- 
    maplist(mutate_seq,InnerSeq,NewInnerSeq) ,  
    Mutation = b(general_concat(b(sequence_extension(NewInnerSeq),seq(SeqType),Info)),SeqType,OutterInfo).

% if sequence_extension or value sequence is given
random_seq_expr_mutation(Seq,NewSeq) :- 
    mutate_seq(Seq,NewSeq).

% don't mutate empty set 
random_seq_expr_mutation(Expression,Expression).

% mutate sequence_extension nodes by replacing with matching sequence expressions
% like restrict_front, restrict_tail or concat
mutate_seq(b(sequence_extension(Seq),Type,Info),Mutation) :- 
    random(0,3,R) , 
    ( R = 0
    ->  random_front_restriction(Seq,Temp,Restriction) , 
        Temp = b(restrict_front(b(sequence_extension(Temp),Type,Info),b(integer(Restriction),integer,[])),Type,Info)
    ; R = 1
    ->  random_tail_restriction(Seq,Temp,Restriction) , 
        Temp = b(restrict_tail(b(sequence_extension(Temp),Type,Info),b(integer(Restriction),integer,[])),Type,Info)
    ;   random_union(Seq,SubSeqA,SubSeqB) , 
        Temp = b(concat(b(sequence_extension(SubSeqA),Type,Info),b(sequence_extension(SubSeqB),Type,Info)),Type,[])) ,
    % random choice of further mutation
    random(0,10,R) , 
    (R < 7
    ->  Mutation = Temp
    ;   random_set_expr_mutation(Temp,Mutation)).
mutate_seq(b(value(Seq),Type,Info),b(value(NewSeq),Type,Info)) :- 
    generate(mutation(Seq:prob_value_seq),NewSeq).

% generate random list, concatenate with given one and get restriction value
random_front_restriction(List,NewList,Restriction) :- 
    List = [b(_,Type,_)|_] ,
    NType =.. [Type,[]] ,  
    generate(prob_ast_set(NType,[extension]),b(set_extension(Set),_,_)) ,
    append(List,Set,NewList) , 
    length(List,Restriction).
% tail restriction diverse to front restriction in interpretation
random_tail_restriction(List,NewList,Restriction) :- 
    List = [b(_,Type,_)|_] ,
    NType =.. [Type,[]] ,  
    generate(prob_ast_set(NType,[extension]),b(set_extension(Set),_,_)) ,
    append(Set,List,NewList) , 
    length(Set,Restriction).