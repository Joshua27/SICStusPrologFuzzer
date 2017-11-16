:- use_module(fuzzing).

% The predicate to be tested.
reverse(L,R) :-
    reverse(L,[],R).
reverse([],Acc,Acc).
reverse([H|T],Acc,R) :-
    reverse(T,[H|Acc],R).

% A simple fuzz test generating random lists of atoms where each atom uses only alphabetic characters.
run_test :-
    fuzz(reverse/2,list(atom([alph]),[]):var).
% Fuzz the predicate a specific amount of times.
run_test2 :-
    fuzz(reverse/2,100000,list(atom([alph]),[]):var).

% Reproduce a test for a specific seed.
reproduce_example :-
    reproduce_test(reverse/2,list(atom([alph]),[]):var,random(27134,9213,17773,425005073)).
