%:- module(meta,[prove_question/3,explain_question/3,prove_rule/2]).

:- consult(library).

%%% meta-interpreter %%%

prove_question(Query,SessionId,Answer):-
	%portray_clause(user_error,Query),
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	prove_rb(Query,Rulebase),
	transform(Query,Clauses),
	phrase(sentence(Clauses),AnswerAtomList),
	atomics_to_string(AnswerAtomList," ",Answer).

explain_question(Query,SessionId,Answer):-
	%portray_clause(user_error,Query),
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,Proof) ->
		maplist(p2m,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		reverse([Last|Msg],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'This is as yet unexplained'
	).

prove_rule([Rule],SessionId):-
	%portray_clause(user_error,Rule),
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	\+(\+((numbervars(Rule,0,_),
	     Rule=(H:-B),
	     body2rules(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   ))).

body2rules((A,B),Rs0,Rs):-!,
	body2rules(A,Rs0,Rs1),
	body2rules(B,Rs1,Rs).
body2rules(A,Rs0,[[(A:-true)]|Rs0]).

prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).

prove_rb(Q,RB,RP):-
	prove_rb(Q,RB,[],P),
	reverse(P,RP).

prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(A,[(A:-true)]).

