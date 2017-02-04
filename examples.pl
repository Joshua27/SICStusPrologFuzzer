:- use_module(fuzzing,[fuzz/2,generate/2]).

reverse(L,R) :-
    reverse(L,[],R).
reverse([],Acc,Acc).
reverse([H|T],Acc,R) :- 
    reverse(T,[H|Acc],R).
% a simple fuzz test
run_test :- 
    fuzz(reverse/2,list(atom([alph]),[]):var).
