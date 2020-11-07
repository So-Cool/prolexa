%:- module(prolexa_engine,
%	[
%		prove_question/3,		% main question-answering engine
%		explain_question/3,		% extended version that constructs a proof tree
%		known_rule/2,			% test if a rule can be deduced from stored rules
%		all_rules/1,			% collect all stored rules 
%		all_answers/2,			% everything that can be proved about a particular Proper Noun
%	]).

:- consult(library).


%%% Main question-answering engine adapted from nl_shell.pl %%%

prove_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).	

% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).	


%%% Extended version of prove_question/3 that constructs a proof tree %%%
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).

% convert proof step to message
pstep2message(p(_,Rule),Message):-
	rule2message(Rule,Message).
pstep2message(n(Fact),Message):-
	rule2message([(Fact:-true)],FM),
	atomic_list_concat(['It is not known that',FM]," ",Message).


%%% test if a rule can be deduced from stored rules %%%
known_rule([Rule],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	try((numbervars(Rule,0,_),
	     Rule=(H:-B),
	     add_body_to_rulebase(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   )).

add_body_to_rulebase((A,B),Rs0,Rs):-!,
	add_body_to_rulebase(A,Rs0,Rs1),
	add_body_to_rulebase(B,Rs1,Rs).
add_body_to_rulebase(A,Rs0,[[(A:-true)]|Rs0]).


%%% meta-interpreter that constructs proofs %%%

% 3d argument is accumulator for proofs
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

% top-level version that ignores proof
prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).


%%% Utilities from nl_shell.pl %%%

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

% transform instantiated, possibly conjunctive, query to list of clauses
transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(A,[(A:-true)]).


%%% Two more commands: all_rules/1 and all_answers/2

% collect all stored rules 
all_rules(Answer):-
	findall(R,prolexa:stored_rule(_ID,R),Rules),
	maplist(rule2message,Rules,Messages),
	( Messages=[] -> Answer = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

% convert rule to sentence (string)
rule2message(Rule,Message):-
	phrase(sentence1(Rule),Sentence),
	atomics_to_string(Sentence," ",Message).

% collect everything that can be proved about a particular Proper Noun
all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries), % collect known predicates from grammar
	maplist(prove_question,Queries,Msg),
	delete(Msg,"",Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).


