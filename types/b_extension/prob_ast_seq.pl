:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random_member/2]).

% list of options:
% extension, value (i.e. random value set) to generate a specific type of ast sequence
% avl, list for specific type of value sequence
% any to generate either sequence or expression
% size:Value to limit length of sequence

generate(prob_ast_seq(Options),Value) :- 
    is_list(Options) , 
    generate(ground_type,Type) , 
    generate(prob_ast_seq(Type,Options),Value).

generate(prob_ast_seq(Type),Value) :- 
    generate(prob_ast_seq(Type,[]),Value).

generate(prob_ast_seq(Type,Options),Value) :-
    member(Type,[integer(_),boolean(_),string(_)]) ,
    Type =.. [InnerType,_] , ! , 
    ( member(extension,Options)
    ->  delete(Options,extension,NOptions) ,
        gen_type(Type,ast,NType) , 
        generate(list(NType,NOptions),Set),
        Value = b(sequence_extension(Set),seq(InnerType),[])
    ; member(avl,Options)
    ->  generate(prob_value_seq(Type,Options),Seq),
        Value = b(value(Seq),seq(InnerType),[])
    ; member(list,Options)
    ->  generate(prob_value_seq(Type,Options),Set),
        Value = b(value(Set),seq(InnerType),[])
    ; % any value sequence
      member(value,Options)
    ->  generate(prob_value_seq(Type,Options),Seq),
        Value = b(value(Seq),seq(InnerType),[])
    ; % sequence or sequence expression
      member(any,Options)
    ->  random_member(T,[seq,expr]) ,
        (T = seq
        ->  generate(ground_type,SeqType) , 
            generate(prob_ast_seq(SeqType,[]),Value)
        ;   generate(prob_ast_seq_expr([well-defined]),Value))
    ;   random_member(SeqOption,[[extension|Options],[value|Options]]) , 
        generate(prob_ast_seq(Type,SeqOption),Value)).

generate(prob_ast_seq(empty_sequence,_),b(sequence_extension([]),seq(empty),[])).

% sequence of several sequences
generate(prob_ast_seq(SeqTypeOptions,Options),b(Seq,seq(NType),[])) :-
    SeqTypeOptions =.. [seq,Type|InnerOptions] , 
    (InnerOptions = [NOptions]
    ->  true
    ;   NOptions = InnerOptions) , 
    (member(size:Size,Options)
    ->  true 
    ;   random(1,10,Size)) ,
    length(List,Size) , 
    ( member(list,Options)
    ->  maplist(generate(prob_value_seq(Type,NOptions)),List) ,
        Seq = value(List)
    ; member(avl,Options)
    ->  maplist(generate(prob_value_seq(Type,NOptions)),List) ,
        findall(Key-true,member(Key,List),AVLList) , 
        list_to_avl(AVLList,AVL) , 
        Seq = value(avl_set(AVL))
    ;   maplist(generate(prob_ast_seq(Type,NOptions)),List) ,
        Seq = sequence_extension(List)) , 
    % remove options for node type
    inner_type(seq(Type),_,Outter) , 
    length(Outter,L) , 
    nth1(L,Outter,NewInner,NewOutter) , 
    surround_type(NewInner,NewOutter,NType).

shrink(prob_ast_seq,Value,Shrunken) :- 
    shrink(prob_ast_seq_expr,Value,Shrunken).