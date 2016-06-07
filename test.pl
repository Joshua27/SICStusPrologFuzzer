:- use_module(fuzzing).

% defective predicate
a(N) :- N \= 4.
a(4) :- a(4).

test1(Int) :- 
    a(Int) ; \+a(Int).
run_test :- 
    fuzz(test1/1,integer([positive])).
