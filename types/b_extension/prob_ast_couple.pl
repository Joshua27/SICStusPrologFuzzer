:- multifile generate/2.
:- multifile shrink/3.

% any couple
generate(prob_ast_couple,b(couple(V1,V2),couple(ValTypeA,ValTypeB),[])) :-  
    generate(prob_ast_any,V1) ,  
    generate(prob_ast_any,V2) , 
    V1 = b(_,ValTypeA,_) , 
    V2 = b(_,ValTypeB,_).

% same type
generate(prob_ast_couple(Type),b(couple(V1,V2),couple(ValType,ValType),[])) :- 
    gen_type(Type,ast,NType) , 
    generate(NType,V1) , 
    generate(NType,V2) , 
    V1 = b(_,ValType,_).
% possibly two different types
generate(prob_ast_couple(TypeA,TypeB),b(couple(V1,V2),couple(ValTypeA,ValTypeB),[])) :- 
    gen_type(TypeA,ast,NTypeA) ,
    gen_type(TypeB,ast,NTypeB) , 
    generate(NTypeA,V1) , 
    generate(NTypeB,V2) , 
    V1 = b(_,ValTypeA,_) , 
    V2 = b(_,ValTypeB,_).

% one ground value instead of type
generate(prob_ast_couple(V1,TypeB),b(couple(V1,V2),couple(ValType,ValTypeB),[])) :- 
    V1 = b(_,ValType,_) ,
    gen_type(TypeB,ast,NTypeB) , 
    generate(NTypeB,V2) , 
    V2 = b(_,ValTypeB,_).
generate(prob_ast_couple(TypeA,V2),b(couple(V1,V2),couple(ValTypeA,ValType),[])) :- 
    V2 = b(_,ValType,_) ,
    gen_type(TypeA,ast,NTypeA) , 
    generate(NTypeA,V1) , 
    V1 = b(_,ValTypeA,_).

% both values are ground
generate(prob_ast_couple(V1,V2),b(couple(V1,V2),couple(ValTypeA,ValTypeB),[])) :- 
    V1 = b(_,ValTypeA,_) ,
    V2 = b(_,ValTypeB,_).