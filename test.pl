:- use_module(fuzzing).

% defective predicate
a(1).
a(2).
a(3).
a(4) :- a(4).
a(5).
a(6).

test1(Int) :- 
    a(Int) ; \+a(Int).
run_test :- 
    fuzz(test1/1,integer([positive])).
