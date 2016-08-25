:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(random),[random/3]).
:- use_module(library(sets),[subset/2]).

% Options:
% small, positive, negative, nozero, between(A,B) or none

generate(integer(Options),Value) :- 
    member(between(A,B),Options) , 
    generate(between(A,B),Value).

generate(integer(Options),Value) :-
    subset([small,positive,nozero],Options) , 
    random(1,128,Value).

generate(integer(Options),Value) :-
    subset([small,positive],Options) , 
    random(0,128,Value).

generate(integer(Options),Value) :-
    subset([small,negative],Options) , 
    random(-128,-1,Value).

generate(integer(Options),Value) :-
    subset([positive,nozero],Options) , 
    random(1,20000,Value).

generate(integer(Options),Value) :-
    member(positive,Options) , 
    random(0,20000,Value).

generate(integer(Options),Value) :-
    member(negative,Options) , 
    random(-20000,-1,Value).

generate(integer(Options),Value) :-
    member(small,Options) , 
    random(-64,64,Value).

generate(integer(_),Value) :- 
    random(-20000,20000,Value).

% try 0
shrink(integer(_),_,0).

% divide by two for big numbers
shrink(integer(_),Value,Shrunken) :-
    (Value > 100 ; Value < -100) , 
    Shrunken is round(Value / 2).

% smaller steps
shrink(integer(_),Value,Shrunken) :-
    Value > 0 ,
    Shrunken is Value - 1 .
shrink(integer(_),Value,Shrunken) :-
    Value < 0 ,
    Shrunken is Value + 1.
