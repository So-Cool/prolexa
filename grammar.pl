%%% grammar %%%

:- op(600, xfy, '=>').

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

