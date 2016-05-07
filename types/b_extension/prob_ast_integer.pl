:- multifile generate/2.
:- multifile shrink/3.

:- use_module(library(lists),[delete/3]).
:- use_module(library(random),[random_member/2]).
:- use_module(library(sets),[subset/2]).

% Options: 
% small, positive, negative, nozero, between(A,B) none (i.e. []) for any integer
% expr for a well-defined ast integer expression, 
% random to get either an ast integer node or an integer expression

generate(prob_ast_integer(Options),Value) :-
    member(expr,Options) , ! ,
    delete(Options,expr,DOptions) ,
    % if no definedness is given set to well-defined
    (member(_-defined,DOptions) 
    ->  NOptions = DOptions
    ;   NOptions = [well-defined|DOptions]) ,
    generate(prob_ast_int_expr(NOptions),Value).

generate(prob_ast_integer(Options),Value) :-
    \+member(random,Options) , ! ,
    generate(integer(Options),Gen) ,
    Value = b(integer(Gen),integer,[]). 

generate(prob_ast_integer(Options),Value) :- 
    % delete random from options to terminate
    delete(Options,random,NOptions) ,
    % higher chance to generate expression
    random(0,10,R) , 
    (R > 2
    ->  RandomOption = [expr|NOptions] 
    ;   RandomOption = NOptions) ,
    generate(prob_ast_integer(RandomOption),Value).

shrink(prob_ast_integer(_),b(integer(Value),integer,Info),b(integer(Shrunken),integer,Info)) :-
    shrink(integer(_),Value,Shrunken).
    
shrink(prob_ast_integer(_),Value,Shrunken) :-
    shrink(prob_ast_int_expr,Value,Shrunken).