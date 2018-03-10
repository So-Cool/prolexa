%:- module(meta,[prove_question/3,explain_question/3,prove_rule/2]).

:- consult(library).

%%% meta-interpreter %%%

all_facts(Answer):-
	findall(R,alexa_mod:sessionid_fact(_ID,R),Rules),
	maplist(r2m,Rules,Messages),
	( Messages=[] -> Answer = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

r2m(Rule,Message):-
	phrase(sentence1(Rule),Sentence),
	atomics_to_string(Sentence," ",Message).

all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries),
	maplist(q2m,Queries,Msg),
	delete(Msg,"",Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

q2m(Query,Message):-
	( prove_question1(Query,_,Message) -> true
	; otherwise -> Message=""
	).

p2m(p(_,Rule),Message):-
	r2m(Rule,Message).
p2m(n(Fact),Message):-
	r2m([(Fact:-true)],FM),
	atomic_list_concat(['It is not known that',FM]," ",Message).

prove_question1(Query,SessionId,Answer):-
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	prove_rb(Query,Rulebase),
	transform(Query,Clauses),
	phrase(sentence(Clauses),AnswerAtomList),
	atomics_to_string(AnswerAtomList," ",Answer).

% this version always succeeds
prove_question(Query,SessionId,Answer):-
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'This does not follow from what I know'
	).	

explain_question(Query,SessionId,Answer):-
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,Proof) ->
		maplist(p2m,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		reverse([Last|Msg],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'This cannot be explained by what I know'
	).

prove_rule([Rule],SessionId):-
	findall(R,alexa_mod:sessionid_fact(SessionId,R),Rulebase),
	try((numbervars(Rule,0,_),
	     Rule=(H:-B),
	     body2rules(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   )).

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
prove_rb(A,Rulebase,P0,[p(A,Rule),n(C)|P]):-
	find_clause(d((A:-B,not(C))),Rule,Rulebase),
	prove_rb(B,Rulebase,P0,P),
	not prove_rb(C,Rulebase,P,_).

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(A,[(A:-true)]).

