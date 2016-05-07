:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

generate(prob_ast_seq_expr,Value) :- 
    generate(prob_ast_seq_expr([]),Value).

% Options:
% id to enable random generation of identifier
% not-well-defined

% Expr :: seq(_)
generate(prob_ast_seq_expr(Options),Value) :- 
    random_member(Expr,[empty_sequence,front,tail,rev,concat,insert_front,insert_tail,
                        restrict_front,restrict_tail,general_concat]) ,
    generate(prob_ast_seq_expr(Expr,Options),Value).

generate(prob_ast_seq_expr(empty_sequence,_),b(sequence_extension([]),seq(empty),[])).

generate(prob_ast_seq_expr(general_concat,Options),b(general_concat(SeqOfSeq),SeqType,[])) :- 
    generate(ground_type,Ground) , 
    generate(prob_ast_seq(seq(Ground),Options),SeqOfSeq) , 
    SeqOfSeq = b(_,seq(SeqType),_).

generate(prob_ast_seq_expr(Type,Options),b(NewExpr,SeqType,[])) :- 
    member(Type,[front,tail,rev]) , 
    generate(ground_type,InnerType) , 
    (member(id,Options)
    ->  generate(id_or_ast(seq(InnerType)),Seq)   
    ;   generate(prob_ast_seq(InnerType,Options),Seq)) , 
    Seq = b(_,SeqType,_) , 
    NewExpr =.. [Type,Seq]. 

% not well defined when trying to restrict front by
% more values than the sequence contains
generate(prob_ast_seq_expr(Type,Options),b(NewExpr,SeqType,[])) :- 
    member(not-well-defined,Options) , 
    member(Type,[restrict_front,restrict_tail]) ,
    generate(prob_ast_seq_expr(Options),Seq) ,
    Seq = b(_,SeqType,_) ,
    generate(prob_ast_integer([small,positive]),Value) ,
    NewExpr =.. [Type,Seq,Value].
% generate integer in the range of the sequence size
generate(prob_ast_seq_expr(Type,Options),b(NewExpr,SeqType,[])) :- 
    member(Type,[restrict_front,restrict_tail]) ,
    generate(ground_type,Ground) , 
    generate(prob_ast_seq(Ground,[extension|Options]),Seq) ,
    Seq = b(sequence_extension(Set),SeqType,_) ,
    length(Set,Length) ,
    generate(prob_ast_integer([small,between(0,Length)]),Value) ,
    NewExpr =.. [Type,Seq,Value].

generate(prob_ast_seq_expr(concat,Options),b(concat(Seq1,Seq2),seq(Type),[])) :-
    generate(prob_ast_seq_expr(Options),Seq1) , 
    Seq1 = b(_,seq(Type),_) ,
    (Type = empty
    ->  Seq2 = b(sequence_extension([]),seq(empty),[])
    ;   inner_type(Type,Inner,Outter) , 
        NewInner =.. [Inner,[]] ,   % no options
        surround_type(NewInner,Outter,NType) ,
        (member(id,Options) , Type \= empty
        ->  generate(id_or_ast(seq(NType)),Seq2)
        ;   generate(prob_ast_seq(NType,Options),Seq2))).

generate(prob_ast_seq_expr(insert_front,Options),b(insert_front(Value,ValueSeq),seq(SeqType),[])) :- 
    generate(prob_ast_seq_expr(Options),ValueSeq) , 
    ValueSeq = b(_,seq(Type),_) ,
    (Type = empty
    ->  generate(ground_type,NType) , 
        NType =.. [SeqType,_]
    ;   inner_type(Type,Inner,Outter) , 
        NewInner =.. [Inner,[]] ,  
        surround_type(NewInner,Outter,NType) , 
        Type = SeqType) ,
    (member(id,Options)
    ->  generate(id_or_ast(seq(NType)),Value)
    ;   gen_type(NType,ast,GenType) , 
        generate(GenType,Value)).

generate(prob_ast_seq_expr(insert_tail,Options),b(insert_tail(ValueSeq,Value),seq(SeqType),[])) :- 
    generate(prob_ast_seq_expr(Options),ValueSeq) , 
    ValueSeq = b(_,seq(Type),_) , 
    (Type = empty
    ->  generate(ground_type,NType) , 
        NType =.. [SeqType,_]
    ;   inner_type(Type,Inner,Outter) , 
        NewInner =.. [Inner,[]] ,  
        surround_type(NewInner,Outter,NType) , 
        Type = SeqType) ,
    (member(id,Options)
    ->  generate(id_or_ast(seq(NType)),Value)
    ;   gen_type(NType,ast,GenType) , 
        generate(GenType,Value)).

shrink(Type,Value,Shrunken) :-
    Type =.. [prob_ast_seq_expr|_] , 
    minimize_seq_expr(Value,Shrunken).

shrink(Type,Value,Shrunken) :-
    Type =.. [prob_ast_seq_expr|_] , 
    % defined in prob_ast_minimize_int_expr.pl
    get_inner_expr(Value,Shrunken).