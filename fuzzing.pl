:- module(fuzzing,[fuzz/2,fuzz/3,
                   reproduce_test/3,
                   generate/2,
                   error_occurred/0]).

:- use_module(library(lists)).
:- use_module(library(timeout),[time_out/3]).
:- use_module(library(random),[getrand/1,setrand/1]).
:- use_module(library(file_systems),[file_members_of_directory/3]).

:- multifile generate/2.
:- multifile shrink/3.

include_type_definition(_-FullPath) :- consult(FullPath).

% set true to use the extension to generate B/Event-B ASTs like they are represented in ProB
b_extension(false).

include_b_extension :-
    b_extension(true) ,
    file_members_of_directory('./types/b_extension/','*.pl',FileList),
    maplist(include_type_definition,FileList).
include_b_extension :-
    b_extension(false).

:- file_members_of_directory('./types/','*.pl',FileList),
   maplist(include_type_definition,FileList) ,
   include_b_extension.

:- meta_predicate fuzz(1,+).
:- meta_predicate fuzz(1,+,+).

fuzz(Module:Predicate/Arity,Arguments) :-
    fuzz(Module:Predicate/Arity,20000,Arguments).
% fuzzing with specific amount of tests
fuzz(Module:Predicate/Arity,Testcount,Arguments) :-
    (current_predicate(Module:Predicate/Arity)
    ->  true
    ;   error_process(existence_error,Predicate,_,_,_) , fail) ,
    % split arguments by ':'
    get_types(Arguments,Types) ,
    length(Types,Arity) ,
    getrand(Seed) ,
    format('Start fuzzing predicate ~w/~w~n',[Predicate,Arity]) ,
    format('First state is ~w~n',[Seed]) ,
    % run randomized tests
    (run_tests(Predicate,Types,Module,Testcount,Result)
    ->  fuzz_aux(Result)
    ;   % run tests failed, error in the code
        error_process(generation_error,Predicate,_,Types,_)).
fuzz(_:Predicate/Arity,_,_) :-
    error_process(not_enough_arguments,Predicate/Arity,_,_,_).

fuzz_aux(true) :-
    format('~nAll tests passed~n',[]).
fuzz_aux(_).

:- meta_predicate reproduce_test(1,+,+).

% reproduce test case by using a given seed
reproduce_test(Module:Predicate/Arity,Arguments,Seed) :-
    get_types(Arguments,Types) ,
    length(Types,Arity) ,
    format('Start fuzzing predicate ~w/~w for given seed~n',[Predicate,Arity]) ,
    setrand(Seed) ,
    % run single test
    (run_tests(Predicate,Types,Module,1,Result)
    ->  reproduce_test_aux(Result,Seed)
    ;   % run tests failed
        error_process(generation_error,Predicate,_,Types,_)).
reproduce_test(_:Predicate/Arity,_,_) :- error_process(not_enough_arguments,Predicate/Arity,_,_,_).

reproduce_test_aux(true,Seed) :-
    format('Test passed for seed ~w~n',[Seed]).
 % pass, because error has already been printed in run_tests
reproduce_test_aux(_,_).

:- dynamic error_occurred/0.

run_tests(_,_,_,0,true).
% run tests with random arguments for a prolog predicate
run_tests(Predicate,Types,Module,Testcount,Result) :-
    getrand(Seed) ,
    random_arguments(Types,Values) ,
    Term =.. [Predicate|Values] ,
    call_term(Module,Term,Error) ,
    % write '.' every thousandth testcase
    (0 is mod(Testcount,1000)
    ->  write('.')
    ;   true) ,
    run_tests_aux(Predicate,Types,Module,Testcount,Seed,Values,Error,Result).

run_tests_aux(Predicate,Types,Module,Testcount,_Seed,_Values,Error,Result) :-
    % go on with testing if no error detected
    Error = none ,
    NTestcount is Testcount - 1 ,
    run_tests(Predicate,Types,Module,NTestcount,Result).
run_tests_aux(Predicate,Types,Module,_Testcount,Seed,Values,Error,Result) :-
    Error \= none ,
    Result = false ,
    % try shrinking arguments and print error
    format('~nError detected, shrink arguments~n',[]) ,
    % don't print input from mutation(Input:Type) for user readability
    minimum_typelist(Types,NTypes) ,
    shrink_values(Predicate,Module,Types,Values,Shrunken) , nl ,
    assert(error_occurred) ,
    error_process(Error,Predicate,Shrunken,NTypes,Seed) , nl.

% calls a term within its given module with error and timeout exception
call_term(Module,Term,Error) :-
    time_out(on_exception(_,Module:call(Term),fail),5000,Result) ,
    call_term_aux(Result,Error).
call_term(_Module,_Term,Error) :-
    % predicate failed
    Error = false.

call_term_aux(success,none).
call_term_aux(_,timeout).

% shrink arguments
shrink_values(Predicate,Module,Types,Values,Result) :-
    % write sth at every shrinking step
    write('.') ,
    maplist(shrink_arguments,Types,Values,Shrunken) ,
    % termination condition
    Values \= Shrunken ,
    Term =.. [Predicate|Shrunken] ,
    % catch timeout and error exception
    \+ time_out(on_exception(_,Module:call(Term),fail),1000,success) ,
    shrink_values(Predicate,Module,Types,Shrunken,Result).
shrink_values(_,_,_,Result,Result).

shrink_arguments(Type,Value,Shrunken) :-
    shrink(Type,Value,Shrunken).
shrink_arguments(_,Value,Value).

% convert types divided by ':' to a list
get_types(Type:T,[Type|NT]) :-
    get_types(T,NT) , !.
get_types(Type,[Type]).

% generate random arguments from a list of types
random_arguments([],[]).
random_arguments([Type|T1],[Value|T2]) :-
    generate(Type,Value) ,
    random_arguments(T1,T2).

% make typelist readable, i.e. don't print input from mutation(Input:Type)
minimum_typelist([],[]).
minimum_typelist([mutation(_:Type)|T],[mutation(Type)|NT]) :- ! ,
    minimum_typelist(T,NT).
minimum_typelist([Type|T],[Type|NT]) :-
    minimum_typelist(T,NT).

% error prints
error_process(existence_error,Predicate,_,_,_) :-
    format('Predicate ~w does not exist.~n',[Predicate]).
error_process(not_enough_arguments,Predicate,_,_,_) :-
    format('Wrong amount of arguments for predicate ~w~n',[Predicate]).
error_process(error(type_error(_,_),_),Predicate,_,Types,_) :-
    format('Wrong type of arguments in predicate ~w of type ~w~n',[Predicate,Types]).
error_process(generation_error,_,_,Types,_) :-  % error in generate/2
    format('Either the type is not defined or there is an implementation error in a prolog file for a type of ~w~n',[Types]).
error_process(timeout,Predicate,Values,Types,Seed) :-
    length(Types,Arity) ,
    format('Timeout in ~w/~w for input ~w of type ~w~nSeed for reproducing test case: ~w~n',[Predicate,Arity,Values,Types,Seed]).
error_process(false,Predicate,Values,Types,Seed) :-
    length(Types,Arity) ,
    format('Predicate ~w/~w false for input ~w of type ~w~nSeed for reproducing test case: ~w~n',[Predicate,Arity,Values,Types,Seed]).
