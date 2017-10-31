:-module(alexa_mod,[alexa/1]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- dynamic sessionid_fact/2.
:- dynamic '$copy'/1.
:- op(600, xfy, '=>').

alexa(Request):-
	/*
	setup_call_cleanup(open('output.txt',append,Stream,[alias(myout)]),
			   authenticate_alexa(Request),
			   close(Stream)),
	*/
	http_read_json_dict(Request,DictIn),
	handle_dict(DictIn,DictOut),
	%my_json_answer(hello,DictOut),
	reply_json(DictOut).

handle_dict(DictIn,DictOut) :-
	/*
	setup_call_cleanup(
			   open('recieved.txt',append,Stream,[]),
			   (get_id(DictIn,Id),
			    format(Stream,"Id: ~w\n",[Id])),
			   close(Stream)
			  ),

	application_id(Id),
	*/
	IntentName = DictIn.request.intent.name,
	intent_dictOut(IntentName,DictIn,DictOut).

handle_dict(_DictIn,DictOut):-
	DictOut = _{
	      shouldEndSession: false,
	      response: _{outputSpeech:_{type: "PlainText", text: "Error Id did not match"}},
              version:"1.0"
	     }.

get_id(_Dict,_Id):-
  true.
	%get_dict(session,_Dict,SessionObject),
	%get_dict(application,SessionObject,ApplicationObject),
	%get_dict(applicationId,ApplicationObject,_Id).

application_id(X):-
	X= "amzn1.ask.skill.a27eb505-fcef-49bf-8975-3e1a6d7b7c74".

my_json_answer(Message,X):-
	X = _{
	      response: _{
			  shouldEndSession: false,
			  outputSpeech:_{type: "PlainText", text: Message}
			 },
              version:"1.0"
	     }.

go:-
	json_write_dict(current_output,_{version:"1.0", shouldEndSession: false, response: _{outputSpeech:_{type: "PlainText", text: "Wally is a walrus"}}}).

random_fact(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).


/*
 *  Steps needed
* 1. check the app id
  2. Check the time stamp
* 3. Make the json responce
*/

%%% stuff for Prolog skill %%%

intent_dictOut("getANewFact",_,DictOut):-
	random_fact(Fact),
	my_json_answer(Fact,DictOut).

intent_dictOut("forget",_,DictOut):-
	retractall(alexa_mod:sessionid_fact(_,_)),
	my_json_answer("I am a blank slate",DictOut).

intent_dictOut("KBdump",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	findall(Msg,
			(alexa_mod:sessionid_fact(SessionId,Rule),
			 phrase(sentence(Rule),Sentence),
			 atomics_to_string(Sentence," ",Msg)
			),
			Messages),
	( Messages = [] -> Message = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Message)
	),
	my_json_answer(Message,DictOut).

intent_dictOut("remember",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Value=DictIn.request.intent.slots.mySlot.value,
	portray_clause(user_error,Value),
	make_atomlist(Value,AtomList),
	( phrase(sentence(Rule),AtomList) ->
	  (assertz(alexa_mod:sessionid_fact(SessionId,Rule)),Answer=Value)
	; otherwise -> Answer='I am afraid I don\'t understand'
	),my_json_answer(Answer,DictOut).

intent_dictOut("question",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Value=DictIn.request.intent.slots.questionSlot.value,
	portray_clause(user_error,Value),
	make_atomlist(Value,AtomList),
	( phrase(question(Query),AtomList),
	  prove_question(Query,SessionId,Answer) -> true
	; otherwise -> Answer='I am afraid I can\'t answer your question'
	),my_json_answer(Answer,DictOut).

%%% this one is for Epi skill

intent_dictOut("utterance",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Utterance=DictIn.request.intent.slots.utteranceSlot.value,
	process_utterance(SessionId,Utterance,Answer),
	my_json_answer(Answer,DictOut).

intent_dictOut(_,_,DictOut):-
	my_json_answer('Unknown error',DictOut).

process_utterance(SessionId,Utterance,Answer):-
	portray_clause(user_error,Utterance),
	make_atomlist(Utterance,AtomList),
	( phrase(sentence(Rule),AtomList),
	  implied(Rule,SessionId) ->
		atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	; phrase(sentence(Rule),AtomList) ->
		assertz(alexa_mod:sessionid_fact(SessionId,Rule)),
		atomic_list_concat(['Thanks for telling me that',Utterance],' ',Answer)
	; phrase(question(Query),AtomList),
	  prove_question(Query,SessionId,Answer) -> true
	; phrase(command(g(Goal,Answer)),AtomList),
	  call(Goal) -> true
	; otherwise -> Answer='I am afraid I don\'t understand'
	).

make_atomlist(Value,AtomList):-
	split_string(Value," ","",StringList),
	maplist(string_lower,StringList,StringListLow),
	maplist(atom_string,AtomList,StringListLow).


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

implied([Rule],SessionId):-
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
body2rules(A,Rs0,[(A:-true)|Rs0]).

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

conj_append(true,Ys,Ys).
conj_append(X,Ys,(X,Ys)):-	% single-element conjunction
	X\=true, 
	X\=(_One,_TheOther).
conj_append((X,Xs),Ys,(X,Zs)):-
	conj_append(Xs,Ys,Zs).

%%% grammar %%%

utterance(C) --> sentence(C).
utterance(C) --> question(C).
utterance(C) --> command(C).

sword --> [].
sword --> [that]. 

qword --> [].
%qword --> [if]. 
%qword --> [whether]. 

cword --> [].
cword --> [to]. 

sentence(C) --> sword,sentence1(C).

sentence1(C) --> determiner(N,M1,M2,C),
                noun(N,M1),
                verb_phrase(N,M2).
sentence1([(L:-true)]) --> proper_noun(N,X),
                          verb_phrase(N,X=>L).

verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(p,M) --> [are], property(p,M).

property(N,M) --> adjective(N,M).
property(s,M) --> [a], noun(s,M).
property(p,M) --> noun(p,M).
%property(_N,X=>mortal(X)) --> [mortal].

determiner(s,X=>B,X=>H,[(H:-B)]) --> [every].
determiner(p,X=>B,X=>H,[(H:-B)])	--> [all].
%determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

proper_noun(s,caroline) --> [caroline].
proper_noun(s,george) --> [george].
proper_noun(s,peter) --> [peter].

%noun(s,X=>human(X)) --> [human].
%noun(p,X=>human(X)) --> [humans].
%noun(s,X=>living_being(X)) --> [living],[being].
%noun(p,X=>living_being(X)) --> [living],[beings].

question(Q) --> qword,question1(Q).

question1(Q) --> [who],[is], property(s,_X=>Q).
question1(Q) --> [is], proper_noun(N,X),
                property(N,X=>Q).
%question1((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),
%					  property(p,sk=>Q2).

% lexicon, driven by predicates
adjective(_,M)		--> [Adj],    {pred2gr(_P,1,a/Adj, M)}.
noun(s,M)			--> [Noun],   {pred2gr(_P,1,n/Noun,M)}.
noun(p,M)			--> [Noun_p], {pred2gr(_P,1,n/Noun,M),noun_s2p(Noun,Noun_p)}.
iverb(s,M)			--> [Verb_s], {pred2gr(_P,1,v/Verb,M),verb_p2s(Verb,Verb_s)}.
iverb(p,M)			--> [Verb],   {pred2gr(_P,1,v/Verb,M)}.

% unary predicates for adjectives, nouns and verbs
pred(human,   1,[a/human,n/human]).
pred(mortal,  1,[a/mortal,n/mortal]).
%pred(man,     1,[a/male,n/man]).
%pred(woman,   1,[a/female,n/woman]).
%pred(married, 1,[a/married]).
%pred(bachelor,1,[n/bachelor]).
pred(mammal,  1,[n/mammal]).

pred2gr(P,1,C/W,X=>Lit):-
	pred(P,1,L),
	member(C/W,L),
	Lit=..[P,X].

noun_s2p(Noun_s,Noun_p):-
	( Noun_s=woman -> Noun_p=women
	; Noun_s=man -> Noun_p=men
	; atom_concat(Noun_s,s,Noun_p)
	).

verb_p2s(Verb_p,Verb_s):-
	( Verb_p=fly -> Verb_s=flies
	; 	atom_concat(Verb_p,s,Verb_s)
	).

command(C) --> cword,command1(C).

command1(g(random_fact(Fact),Fact)) --> getanewfact.
command1(g(retractall(alexa_mod:sessionid_fact(_,C)),"I erased it from my memory")) --> forget,sentence(C). 
command1(g(retractall(alexa_mod:sessionid_fact(_,_)),"I am a blank slate")) --> forgetall. 
command1(g(all_facts(Answer),Answer)) --> kbdump. 
command1(g(all_answers(PN,Answer),Answer)) --> [tell,me,about],proper_noun(s,PN).
command1(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).
command1(g(mr(Answer),Answer)) --> explain_mr.

explain_mr --> ask,mr.

ask --> [ask],proper_noun(s,_),[to].
ask --> [].

mr --> [explain,mendelian,randomisation].
mr --> [explain,mendelian,randomization].

getanewfact --> getanewfact1.
getanewfact --> [tell,me],getanewfact1.

getanewfact1 --> [anything].
getanewfact1 --> [a,random,fact].
getanewfact1 --> [something,i,'don\'t',know].

kbdump --> [tell,me,everything].
kbdump --> [spill,the,beans].

forget --> [forget].

forgetall --> [forget,everything].

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
	( prove_question(Query,_,Message) -> true
	; otherwise -> Message=""
	).

p2m(p(_,Rule),Message):-
	r2m(Rule,Message).

%%% generating intents from grammar %%%

intents:-
	findall(
			_{
				%id:null,
				name:
					_{
						value:SS,
						synonyms:[]
					}
			},
		( phrase(alexa_mod:utterance(_),S),
		  atomics_to_string(S," ",SS)
		),
		L),
	json_write(current_output,
				_{
				  intents: [
					_{
					  name: 'AMAZON.CancelIntent',
					  samples: []
					},
					_{
					  name: 'AMAZON.HelpIntent',
					  samples: []
					},
					_{
					  name: 'AMAZON.StopIntent',
					  samples: []
					},
					_{
					  name: utterance,
					  samples: [
						'{utteranceSlot}'
					  ],
					  slots: [
						_{
						  name: utteranceSlot,
						  type: utteranceSlot,
						  samples: []
						}
					  ]
				  }
				  ],
				  types: [
						_{
							name:utteranceSlot,
							values:L
						}
					]
				}
			   ).


%%% test %%%

test:-
	read(Input),
	( Input=stop -> true
	; otherwise ->
		process_utterance(1,Input,Output),
		writeln(Output),
		test
	).


mr('In epidemiology, Mendelian randomization is a method of using measured variation in genes of known function to examine the causal effect of a modifiable exposure on disease in observational studies. The design was first proposed in 1986 and subsequently described by Gray and Wheatley as a method for obtaining unbiased estimates of the effects of a putative causal variable without conducting a traditional randomised trial. These authors also coined the term Mendelian randomization. The design has a powerful control for reverse causation and confounding which otherwise bedevil epidemiological studies. Its current hero is George Davey-Smith.').

