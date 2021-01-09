%%% Definite Clause Grammer for prolexa utterances %%%

utterance(C) --> sentence(C).
utterance(C) --> question(C).
utterance(C) --> command(C).

:- op(600, xfy, '=>').

%%% lexicon, driven by predicates %%%

adjective(_,M)		--> [Adj],    {pred2gr(_P,1,a/Adj, M)}.
noun(s,M)			--> [Noun],   {pred2gr(_P,1,n/Noun,M)}.
noun(p,M)			--> [Noun_p], {pred2gr(_P,1,n/Noun,M),noun_s2p(Noun,Noun_p)}.
noun(m,M)           --> [Noun],   {pred2gr(_P,1,mn/Noun,M)}.
% noun(s,M,V)			--> [Noun], {pred2gr(_P,1,n/Noun,M,_V)}.
iverb(s,M)			--> [Verb_s], {pred2gr(_P,1,v/Verb,M),verb_p2s(Verb,Verb_s)}.
iverb(p,M)			--> [Verb],   {pred2gr(_P,1,v/Verb,M)}.
tverb(s,Y=>X)       --> [Verb_s], {pred2gr(_P,2,v/Verb,Y=>X),verb_p2s(Verb,Verb_s)}.
tverb(p,Y=>X)       --> [Verb],   {pred2gr(_P,2,v/Verb,Y=>X)}.

% unary predicates for adjectives, nouns and verbs
pred(human,   1,[a/human,n/human]).
pred(muggle,  1,[a/muggle,n/muggle]).
pred(mortal,  1,[a/mortal,n/mortal]).
pred(magic,	  1,[a/magic,n/magic]).
pred(metal,   1,[a/metal,n/metal]).

pred(conductive, 1,[a/conductive,n/conductive]).
pred(insulator, 1,[a/insulator,n/insulator]).
pred(metal,	1,[a/metal,n/metal]).
pred(iron,	1,[a/iron,n/iron]).
pred(nail,	1,[a/nail,n/nail]).
pred(wounded, 1,[a/wounded]).
pred(abnormal, 1,[a/abnormal]).
pred(blue, 1,[a/blue]).
pred(round, 1,[a/round]).
pred(cold, 1, [a/cold]).
pred(quiet, 1, [a/quiet]).

%pred(man,     1,[a/male,n/man]).
%pred(woman,   1,[a/female,n/woman]).
%pred(married, 1,[a/married]).
%pred(bachelor,1,[n/bachelor]).
%pred(mammal,  1,[n/mammal]).
pred(bird,    1,[n/bird]).
pred(thing,	  1,[n/thing]).
%pred(bat,     1,[n/bat]).


pred(electricity, 1,[mn/electricity]).
pred(iron, 1,[mn/iron]).

pred(conduct, 2, [v/conduct]).

pred(penguin, 1,[n/penguin]).
pred(sparrow, 1,[n/sparrow]).
pred(fly,     1,[v/fly]).
pred(vanish,  1,[v/vanish]).
pred(ostrich, 1,[n/ostrich]).

pred2gr(P,1,C/W,X=>Lit):-
	pred(P,1,L),
	member(C/W,L),
	Lit=..[P,X].

pred2gr(P,2,C/W,Y=>X=>Lit):-
	pred(P,2,L),
	member(C/W,L),
	Lit=..[P,X,Y].

noun_s2p(Noun_s,Noun_p):-
	( Noun_s=woman -> Noun_p=women
	; Noun_s=man -> Noun_p=men
	; Noun_s=bird -> Noun_p=birds
	; Noun_s=human -> Noun_p=humans
	; Noun_s=thing -> Noun_p=things
	; atom_concat(Noun_s,s,Noun_p)
	).

verb_p2s(Verb_p,Verb_s):-
	( Verb_p=fly -> Verb_s=flies
	; Verb_p=vanish -> Verb_s=vanishes
	; Verb_p=conduct -> Verb_s=conducts
	; atom_concat(Verb_p,s,Verb_s)
	).


%%% sentences %%%

sentence(C) --> sword,sentence1(C).

sword --> [].
sword --> [that]. 

% most of this follows Simply Logical, Chapter 7

% Example input: if someone is a bird and is not a penguin then they can fly
%			   : if something is a bird and is not a penguin then it flies
% 			   : explain why otto flies

% Example input: if someone is a bird and is not a penguin then they cannot fly
% Example input: if someone is not a bird then they are not a penguin'
% Example input: if something is not a bird then it is not a penguin'
% Example input: if someone cannot fly then they are not a bird
% Example input: if someone is not a penguin then they are a sparrow
% Cold things are quiet..


sentence1(C) --> determiner(N,M1,M2,C,n),noun(N,M1),negated_verb_phrase(N,_,_,M2).

sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,_,_,M2).

sentence1([(L:-true)])   --> proper_noun(N,X),verb_phrase(N,_,_,X=>L).
sentence1([(false:-L)])	--> proper_noun(N,X),negated_verb_phrase(N,_,_,X=>L).
sentence1([(H:-B)])      --> conjunction(c,Be),pronoun(P,Be),verb_phrase(N,Be,_,X=>B),adverb(T),pronoun(P,T),verb_phrase(N,T,P,X=>H).
sentence1([(not(H):-B)]) --> conjunction(c,Be),pronoun(P,Be),verb_phrase(N,Be,_,X=>B),adverb(T),pronoun(P,T),negated_verb_phrase(N,T,P,X=>H).
sentence1([(H:-B1,B2)])	 --> conjunction(c,Be),pronoun(_P,_C),verb_phrase(N,Be,_,X=>B1),conjunction(a,Be),verb_phrase(N,Be,_,X=>B2),adverb(T),pronoun(P,T),verb_phrase(N,T,P,X=>H).
sentence1([(H:-B1,not(B2))]) --> conjunction(c,Be),pronoun(_P,_C),verb_phrase(N,Be,_,X=>B1),conjunction(a,Be),negated_verb_phrase(N,Be,_,X=>B2),adverb(T),pronoun(P,T),verb_phrase(N,T,P,X=>H).
sentence1([(not(H):-B1,not(B2))]) --> conjunction(c,Be),pronoun(_P,_C),verb_phrase(N,Be,_,X=>B1),conjunction(a,Be),negated_verb_phrase(N,Be,_,X=>B2),adverb(T),pronoun(P,T),negated_verb_phrase(N,T,P,X=>H).
sentence1([(not(H):-not(B))])      --> conjunction(c,Be),pronoun(P,Be),negated_verb_phrase(N,Be,_,X=>B),adverb(T),pronoun(P,T),negated_verb_phrase(N,T,P,X=>H).
sentence1([(H:-not(B))])      --> conjunction(c,Be),pronoun(P,Be),negated_verb_phrase(N,Be,_,X=>B),adverb(T),pronoun(P,T),verb_phrase(N,T,P,X=>H).
sentence1([(not(H):-B)]) 	--> noun(N,X=>B),negated_verb_phrase(N,before,_,X=>H).

sentence1([(H:-B)]) --> adjective(_N,X=>B),[things],verb_phrase(s,after,person,X=>H).

% Introducing indefinite articles
% sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L,_V).

% Handling interpretation of negation
sentence_negation([not(L):-true])		--> proper_noun(N,X),negated_verb_phrase(N,_,_,X=>L).

% Otto negated verb phrase
negated_verb_phrase(s,before,_,M) --> [is],[not],property(s,M).
% Unsure why this isn't working -> seems to be working now but I'm still suspicious...
negated_verb_phrase(s,before,_,M) --> [cannot],iverb(p,M). 

negated_verb_phrase(s,after,object,M) --> [is],[not],property(s,M).
negated_verb_phrase(s,after,person,M) --> [are],[not],property(s,M).
negated_verb_phrase(s,after,object,M) --> [cannot],iverb(p,M).
negated_verb_phrase(s,after,person,M) --> [cannot],iverb(p,M).

% Plurals
negated_verb_phrase(p,before,_,M) --> [cannot],verb(do),property(s,M).
negated_verb_phrase(p,after,person,M) --> [are],[not],property(s,M).

negated_verb_phrase(_,_,_,X) --> [cannot],tverb(p,Y=>X),noun(m,Y).
negated_verb_phrase(_,_,_,X) --> [do,not],tverb(p,Y=>X),noun(m,Y).
negated_verb_phrase(_,_,_,X) --> [cannot],tverb(s,Y=>X),noun(m,Y).
negated_verb_phrase(_,_,_,X) --> [do,not],tverb(s,Y=>X),noun(m,Y).


negated_verb_phrase(p, M) --> [cannot],property(p, M).
negated_verb_phrase(N, M) --> [cannot], iverb(N,M).

conjunction(c, before) --> [if].
conjunction(a, before) --> [and].

% verb_phrase(s,M) --> [can],[do],property(s,M).
verb_phrase(s,before,_,M) --> [is],property(s,M).
verb_phrase(s,before,_,M) --> [can],verb(do),property(p,M).
verb_phrase(s,before,_,M) --> [can],iverb(p,M).
verb_phrase(s,before,_,M) --> [can],iverb(p,M).

verb_phrase(p,before,_,M) --> property(_,M).

verb_phrase(p,before,_,X) --> tverb(p,Y=>X),noun(p,Y).
verb_phrase(p,before,_,X) --> tverb(p,Y=>X),noun(m,Y).
verb_phrase(s,before,_,X) --> tverb(s,Y=>X),noun(p,Y).
verb_phrase(s,before,_,X) --> tverb(s,Y=>X),noun(m,Y).

verb_phrase(s,_,_,X) --> [can],tverb(p,Y=>X),noun(p,Y).
verb_phrase(s,_,_,X) --> [can],tverb(p,Y=>X),noun(m,Y).

verb_phrase(s,after,object,M) --> [is],property(s,M).
verb_phrase(s,after,person,M) --> [are],property(s,M).

verb_phrase(s,after,object,M) --> iverb(s,M).
verb_phrase(s,after,person,M) --> [can],iverb(p,M).



% JUst an idea for now..
verb(s, before)  --> [is].
verb(s, after) --> [are].
verb(can) --> [can].
verb(do)  --> [do].


/*
verb_phrase(s,M,V) --> [is],property(s,M,V).
*/

property(N,M) --> adjective(N,M).
property(s,M) --> [a],noun(s,M).
property(p,M) --> noun(p,M).
property(_,M) --> [made,of],noun(m,M).
property(_,M) --> [made,of],noun(s,M).
property(_,M) --> [made,of],noun(p,M).
% property(s,M,V) --> [an],noun(s,M,V).


% TO DO: Need expressions for 
determiner(s,X=>B,X=>H,[(H:-B)]) --> [every].
determiner(p,X=>B,X=>H,[(H:-B)]) --> [all].
determiner(p,X=>B,X=>H,[(H:-B)]) --> [].
determiner(p,X=>B,X=>H,[(not(H):-B)],n) --> [].
%determiner(p,X=>B,X=>H,[(H:-B)]) --> [].
%determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

pronoun(person, before) --> [a],[person].
pronoun(person, before) --> [someone].
pronoun(person, after) --> [they].

pronoun(object, before) --> [something].
pronoun(object, after) --> [it].

adverb(after) --> [then].

/*
indef_article(pre_consonant) --> [a].
indef_article(pre_vowel) --> [an].
*/

% Birds Rulebase
bird_determiner(s,X=>B,X=>H,[(H:-B)]) --> [if],[something].
bird_determiner(s,X=>B,X=>H,[(H:-B)]) --> [if],[something].


neg_determiner(s,X=>B,X=>H,[(not(H):-B)]) --> [every].
neg_determiner(p,X=>B,X=>H,[(not(H):-B)]) --> [all].

proper_noun(s,tweety) --> [tweety].
proper_noun(s,peter) --> [peter].
proper_noun(s,otto)	--> [otto].


% Birds Rulebase
proper_noun(s,arthur)	--> [arthur].
proper_noun(s,bill)	--> [bill].
proper_noun(s,colin)	--> [colin].

% Harry Potter
proper_noun(s,harry) --> [harry].
proper_noun(s,dursley) --> [dursley].


%%% questions %%%

question(Q) --> qword,question1(Q).

qword --> [].
%qword --> [if]. 
%qword --> [whether]. 

question1(Q) --> [who],verb_phrase(s,_,_,X=>Q).
question1(Q) --> [is], proper_noun(N,X),property(N,X=>Q).
% question1(Q) --> [is], noun(N,X),property(N,X=>Q).
question1(Q) --> [does],proper_noun(s,X),verb_phrase(N,_,_,X=>Q).
question1(Q) --> [does],noun(s,X),verb_phrase(_,_,_,X=>Q).
question1(Q) --> [are],noun(p,X),verb_phrase(_,_,_,X=>Q).
question1(Q) --> [can],proper_noun(_,X),verb_phrase(N,_,_,X=>Q).
question1(Q) --> [are],
question1(Q) --> [is],determiner(N,M1,M2,Q),noun(N,M1),property(N,M2).

% is nails made of metal'

/*
question1(Q) --> [does],proper_noun(_,X),verb_phrase(_,X=>Q).
question1(Q) --> [can],proper_noun(_,X),verb_phrase(_,X=>Q).
%question1((Q1,Q2)) --> [are,some],noun(p,sk=>Q1),
%					  property(p,sk=>Q2).
*/

%%% commands %%%

% These DCG rules have the form command(g(Goal,Answer)) --> <sentence>
% The idea is that if :-phrase(command(g(Goal,Answer)),UtteranceList). succeeds,
% it will instantiate Goal; if :-call(Goal). succeeds, it will instantiate Answer.
% See case C. in prolexa.pl
% Example: 
%	command(g(random_fact(Fact),Fact)) --> [tell,me,anything].
% means that "tell me anything" will trigger the goal random_fact(Fact), 
% which will generate a random fact as output for prolexa.

command(g(retractall(prolexa:stored_rule(_,C)),"I erased it from my memory")) --> forget,sentence(C). 
command(g(retractall(prolexa:stored_rule(_,_)),"I am a blank slate")) --> forgetall. 
command(g(all_rules(Answer),Answer)) --> kbdump. 
command(g(all_answers(PN,Answer),Answer)) --> tellmeabout,proper_noun(s,PN).

command(g(explain_question_negated(not(Q),_,Answer),Answer)) --> [explain,why],sentence_negation([not(Q):-true]).

command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).

command(g(random_fact(Fact),Fact)) --> getanewfact.
%command(g(pf(A),A)) --> peterflach. 
%command(g(iai(A),A)) --> what. 
command(g(rr(A),A)) --> thanks.

% The special form
%	command(g(true,<response>)) --> <sentence>. 
% maps specific input sentences to specific responses.

command(g(true,"I can do a little bit of logical reasoning. You can talk with me about humans and birds.")) --> [what,can,you,do,for,me,minerva]. 
%command(g(true,"Your middle name is Adriaan")) --> [what,is,my,middle,name]. 
%command(g(true,"Today you can find out about postgraduate study at the University of Bristol. This presentation is about the Centre for Doctoral Training in Interactive Artificial Intelligence")) --> today. 
%command(g(true,"The presenter is the Centre Director, Professor Peter Flach")) --> todaysspeaker. 

thanks --> [thank,you].
thanks --> [thanks].
thanks --> [great,thanks].

getanewfact --> getanewfact1.
getanewfact --> [tell,me],getanewfact1.

getanewfact1 --> [anything].
getanewfact1 --> [a,random,fact].
getanewfact1 --> [something,i,'don\'t',know].

kbdump --> [spill,the,beans].
kbdump --> [tell,me],allyouknow.

forget --> [forget].

forgetall --> [forget],allyouknow.

allyouknow --> all.
allyouknow --> all,[you,know].

all --> [all].
all --> [everything].

tellmeabout --> [tell,me,about].
tellmeabout --> [tell,me],all,[about].

rr(A):-random_member(A,["no worries","the pleasure is entirely mine","any time, peter","happy to be of help"]).

random_fact(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).


%%% various stuff for specfic events

% today --> [what,today,is,about].
% today --> [what,is,today,about].
% today --> [what,is,happening,today].
% 
% todaysspeaker --> [who,gives,'today\'s',seminar].
% todaysspeaker --> [who,gives,it].
% todaysspeaker --> [who,is,the,speaker].
% 
% peterflach --> [who,is],hepf.
% peterflach --> [tell,me,more,about],hepf.
% 
% what --> [what,is],iai.
% what --> [tell,me,more,about],iai.
% 
% hepf --> [he].
% hepf --> [peter,flach].
% 
% iai --> [that].
% iai --> [interactive,'A.I.'].
% iai --> [interactive,artificial,intelligence].
% 
% pf("According to Wikipedia, Pieter Adriaan Flach is a Dutch computer scientist and a Professor of Artificial Intelligence in the Department of Computer Science at the University of Bristol.").
% 
% iai("The Centre for Doctoral Training in Interactive Artificial Intelligence will train the next generation of innovators in human-in-the-loop AI systems, enabling them to responsibly solve societally important problems. You can ask Peter for more information.").
% 
