# SICStusPrologFuzzer
SICStus Prolog Fuzzing Tool - including an extension to generate classical B and Event-B predicates and expressions represented as abstract syntax trees


__Types and Options for generate/2:__


__Normal Types:__

* any(Options)
    * Options can be an arbitrary list of options for any type as listed below

* variable, boolean([])

* fixed(Value)   
    * keep value   

* member_of(List)   
    * random member from list   

* atom(Options)   
    * 'size:Length' to specify the length   
    * 'alph' to generate only alphabetic and small letters   

* list(Type,Options), avl_tree(Type,Options), tree(Type,Options)   
    * 'size:S' to set the amount of elements   

* integer(Options), float(Options), number(Options), rational(Options)   
    * 'small' ([-128,128]), 'positive', 'negative', 'nozero' ,'between(A,B)'   
    * also combinations like [small,positive,nozero]   

__B-Method / Event-B__

__ASTs:__   

* prob_value_integer(Options)   
    * see integer

* prob_value_any(Options)   
    * generate any ProB value   
    * 'noset' or any other option of any type  

* prob_value_set(Type,Options), prob_value_seq(Type,Options)   
    * 'avl' or 'list' for a specific type of set   
    * 'size:S' to set the amount of elements   
    * 'not-well-defined-values' to generate sets with duplicate elements, wrong indices according to sequences, etc.   
        * without this options only well-defined sets will be generated   
    * to generate nested structures the type has been reduced to "set" or "seq" like prob_value_set(set(boolean([]),[list]),[avl])    

* prob_ast_boolean([])   

* prob_ast_string(Options), prob_value_string(Options)   
    * see atom   

* prob_ast_integer(Options)   
    * see integer   
	* additionaly 'expr' will generate an integer expression   
	* 'random' will generate either an expression or integer leave   

* prob_ast_couple[1,2]   

* prob_ast_any(Options)   
    * generate any type of expression
    * 'noset' to generate no sets at all

* prob_ast_set(Type,Options), prob_ast_seq(Type,Options)   
    * 'extension', 'value', 'avl' or 'list' to generate a specific type of set   
    * 'size:S' to set amount of elements

* prob_ast_identifier(TypeOfIdentifier)    
	* generates identifier node with no binding   

* prob_state_bindlist(AST,Options)   
	* searches all identifier nodes all over AST and generates a bindlist with explicit values and a list of the occurring identifier ast nodes
    * option 'unbound' to generate an unbound bindlist
    * return value of generate/2 is BindList:ListOfIdentifiers

* prob_ast_pred(Type,Options), prob_ast_int_expr(Type,Options), prob_ast_set_expr(Type,Options), prob_ast_seq_expr(Type,Options)   
    * 'not-well-defined' according to B   
    * 'id' to enable generation of identifier   
    * 'maxInterval:S', 'posInterval', 'negInterval' to specify intervals   
    * random type if none is given   
    * only for predicates: 'noQuantifier'   

* prob_ast_sat(Options)   
    * generate B predicates yielding satisfiability problems to test the ProB constraint solver and its implementations of Z3 and CVC4
    * 'minID:Min', 'maxID:Max' to set the minimum/maximum amount of used identifier
		* both options can be used together to create an interval, i.e. randomly chose an amount of used identifiers from [Min,Max] 
    * 'maxInterval:C' to specify the maximum size of an interval to be able to control integer constraints   
    * 'posInterval', 'negInterval'   

* detype(AST)   
    * detype an ast like its given after parsing   
    * e.g. convert 'b(integer(7),integer,[])' to 'integer(1,7)', whereat 1 is the index   


__Mutations:__   

* generate(mutation(Input:Type),Out)    
	* Input is either single data or a list of data   
	* supported types:   
	     * list, avl_tree, tree   
	     * prob_value_set, prob_ast_set   
	     * prob_ast_pred, prob_ast_int_expr, prob_ast_set_expr, prob_ast_seq_expr   


__Implemented AST-Nodes:__   

* Predicates   
	* conjunct/2, disjunct/2, implication/2, equivalence/2, exists/2, forall/2   
	* equal/2, not_equal/2, less/2, less_equal/2, greater/2, greater_equal/2   
	* subset/2, not_subset/2, subset_strict/2, not_subset_strict/2   
	* member/2, not_member/2   
	* negation/1, finite/1    
	* truth/0, falsity/0   

* Integer Expressions   
	* add/2, minus/2, multiplication/2, div/2, modulo/2, power_of/2   
	* unary_minus/1, max/1, min/1, card/1   
	* max_int/0, min_int/0   

* Set Expressions   
	* interval/2, union/2, intersection/2, set_subtraction/2, general_union/2, general_intersection/2   
	* bool_set/0, string_set/0, empty_set/0   

* Sequence Expressions   
	* concat/2, general_concat/2, insert_front/2, insert_tail/2, restrict_front/2, restrict_tail/2   
	* front/1, tail/1, rev/1   
	* empty_sequence/0   
