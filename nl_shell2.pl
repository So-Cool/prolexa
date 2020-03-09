%%% adapted from nl_shell.pl (Simply Logical, Chapter 7)

:-consult(library).

:-op(600,xfy,'=>').	% infix functor for predicate semantics

:-dynamic kb/2.	% for storing a Rulebase

%:-set_prolog_flag(unknown,fail).	% unknown predicates will fail silently

%%% Grammar %%%

sentence(Rule)			--> determiner(N,M1,M2,Rule),noun(N,M1),verb_phrase(N,M2).
sentence(d((H:-B,not(E))))			--> determiner(N,X=>B,X=>H,d(H:-B)),noun(N,X=>B),verb_phrase(N,X=>H),exception(N,X=>E).
sentence(c(Lit:-true))	--> proper_noun(N,X),verb_phrase(N,X=>Lit).
verb_phrase(s,M)		--> [is],property(s,M).
verb_phrase(p,M)		--> [are],property(p,M).
verb_phrase(N,M)		--> iverb(N,M).
property(s,M)			--> [a],noun(s,M).
property(p,M)			--> noun(p,M).
property(N,M)			--> adjective(N,M).
exception(N,M)		--> [except],noun(N,M).
determiner(s,X=>B,X=>H,c(H:-B))	--> [every].
determiner(p,X=>B,X=>H,c(H:-B))	--> [all].
determiner(p,X=>B,X=>H,d(H:-B))	--> [most].

% lexicon, driven by predicates
proper_noun(s,PN)	--> [PN].	% accept any proper noun in the right grammatical position
adjective(_,M)		--> [Adj],    {pred2gr(_P,1,a/Adj, M)}.
noun(s,M)			--> [Noun],   {pred2gr(_P,1,n/Noun,M)}.
noun(p,M)			--> [Noun_p], {pred2gr(_P,1,n/Noun,M),noun_s2p(Noun,Noun_p)}.
iverb(s,M)			--> [Verb_s], {pred2gr(_P,1,v/Verb,M),verb_p2s(Verb,Verb_s)}.
iverb(p,M)			--> [Verb],   {pred2gr(_P,1,v/Verb,M)}.

% unary predicates for adjectives, nouns and verbs
pred(human,   1,[a/human,n/human]).
pred(mortal,  1,[a/mortal,n/mortal]).
pred(man,     1,[a/male,n/man]).
pred(woman,   1,[a/female,n/woman]).
pred(married, 1,[a/married]).
pred(bachelor,1,[n/bachelor]).
pred(mammal,  1,[n/mammal]).
pred(bird,    1,[n/bird]).
pred(bat,     1,[n/bat]).
pred(penguin, 1,[n/penguin]).
pred(sparrow, 1,[n/sparrow]).
pred(fly,     1,[v/fly]).

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

question(Q)				--> [who],verb_phrase(s,_=>Q).
question(Q)				--> [is],proper_noun(N,X),property(N,X=>Q).
question(Q)				--> [is],determiner(N,M1,M2,Q),noun(N,M1),property(N,M2).
question(Q)				--> [does],proper_noun(_,X),verb_phrase(_,X=>Q).

%%% Interactive shell %%%

nl_shell:-
	show_help,
	kb(ex,RB),
	writes([nl,' ! Here is my current knowledge:',nl]),
	handle_input(show,RB).

nl_shell(Rulebase):-
	get_input(Input),
	handle_input(Input,Rulebase).

handle_input(Input,Rulebase):-
	( Input = stop	-> true
	; Input = help -> show_help,nl_shell(Rulebase)
	; Input = call(Q)	-> call(Q),nl_shell(Rulebase)
	% show Rulebase as sentences
	; Input = show	-> show_rules(Rulebase),nl_shell(Rulebase)
	% show Rulebase as clauses
	; Input = kb	-> show_kb(Rulebase),nl_shell(Rulebase)
	% save Rulebase
	; Input = save(N)	-> retractall(kb(N,_)),asserta(kb(N,Rulebase)),nl_shell(Rulebase)
	% load Rulebase
	; Input = load(N)	-> kb(N,RB),handle_input(show,RB)
	% list all possible sentences
	; Input = list	-> (phrase(sentence(_),S),show_answer(sentlist(S)),fail;true),nl_shell(Rulebase)
	% answer question
	; phrase(question(Query),Input),answer_query(Query,Rulebase,answers(A)) 
							-> show_answer(answers(A)),nl_shell(Rulebase)
	% answer for an explanation
	; Input=[why|In],phrase(question(Query),In),answer_query(Query,Rulebase,proof(Proof)) 
							-> show_answer(explain(Query,Proof)),nl_shell(Rulebase)
	% provide proof
	; Input=[prove,that|In],phrase(sentence((Query)),In),answer_query(Query,Rulebase,proof(Proof)) 
							-> show_answer(proof(Proof)),nl_shell(Rulebase)
	% second-order query
	; Input=[tell,me,about,In],phrase(proper_noun(s,PN),[In])
							-> all_answers(PN,Rulebase),show_answer(all(In)),nl_shell(Rulebase)
	; Input=[explain,all,about,In],phrase(proper_noun(s,PN),[In])
							-> all_explanations(PN,Rulebase),show_answer(all(In)),nl_shell(Rulebase)
	% check whether statement is either implied or inconsistent
	; phrase(sentence(Rule),Input),check(Rule,Rulebase,Check) 
							-> show_answer(Check),nl_shell(Rulebase)
	% add statement to Rulebase
	; phrase(sentence(Rule),Input) 
							-> show_answer(thanks),nl_shell([Rule|Rulebase])
	% remove statement from Rulebase
	; Input=[forget,that|In],phrase(sentence(Rule),In) 
							-> remove_one(Rule,Rulebase,RB),show_answer(forget),nl_shell(RB)
	% catchall if all of the above fail
	; otherwise -> show_answer(fail),nl_shell(Rulebase)
	).

% get input from user
get_input(Input):-
	read(Input).

% show help menu
show_help:-
	writes([' ! Here is a list of example commands:',nl]),
	writes([' - Save or load a rulebase: save(ex) or load(ex)',nl]),
	writes([' - Display the current rulebase: show or kb',nl]),
	writes([' - Add to the current rulebase: [all,humans,are,mortal] or [most,birds,fly,except,penguins]',nl]),
	writes([' - Ask a question: [who,is,mortal] or [does,tweety,fly]',nl]),
	writes([' - Ask for an explanation: [why,is,socrates,mortal] or [why,does,tweety,fly]',nl]),
	writes([' - Ask for a proof: [prove,that,socrates,is,mortal] or [prove,that,tweety,flies]',nl]),
	writes([' - Second-order queries: [tell,me,about,socrates] or [explain,all,about,tweety]',nl]),
	writes([' - Remove a statement from the current rulebase: [forget,that,all,humans,are,mortal]',nl]),
	true.

% show answer to user
show_answer(Answer):-
	write(' ! '),
	( Answer=hello         -> writes(['Hello, talk to me.'])
	; Answer=all           -> writes(['That is all I know.'])
	; Answer=all(X)        -> writes(['That is all I know about ',X,'.'])
	; Answer=thanks        -> writes(['Thanks for telling me.'])
	; Answer=forget        -> writes(['I have erased it from my memory.'])
	; Answer=implied       -> writes(['Thanks, but I already knew that.'])
	; Answer=inconsistent  -> writes(['I am afraid that contradicts what I know.'])
	; Answer=no            -> writes(['I am afraid I don\'t understand.'])
	; Answer=fail          -> writes(['I am afraid I don\'t understand.'])
	; Answer=proof(no)     -> writes(['I am afraid I don\'t understand.'])
	; Answer=explain(_,no) -> writes(['I am afraid I don\'t understand.'])
	; Answer=answers([])   -> writes(['No'])
	; Answer=explain(Q,P)  -> write_exp(Q,P)
	; Answer=proof(P)      -> write_proof(P),writes(['QED'])
	; Answer=sentence(S)   -> writes(sentence(S))
	; Answer=answers(L)    -> writes(L)
	; otherwise            -> writes(Answer)
	),nl.

writes(A):-
	( A=[]          -> true
	; A=nl          -> nl
	; A=[H|T]       -> writes(H),writes(T)
	; A=asis(T)     -> write(T)
	; A=clause(C)   -> portray_clause(C)
	; A=sentlist(L) -> numbervars(L,0,_),add_sep(L,' ',LS),writes(LS)
	; A=sentence(S) -> (phrase(sentence(S),L);phrase(sentence(c(S:-true)),L)),writes(sentlist(L))
	; otherwise     -> write(A)
	).

add_sep([H],_Sep,[H]).
add_sep([H|T],Sep,[H,Sep|TS]):-
	add_sep(T,Sep,TS).

% write proof tree
write_proof([]):-
	tab(15),write('[]'),nl.
write_proof([p(A,B)|Proof]):-
	write((:-A)),nl,
	tab(5),write('|'),tab(10),try((numbervars(B,0,_),write(B))),nl,
	tab(5),write('|'),tab(20),write('/'),nl,
	write_proof(Proof).

write_exp(Q,[]):-
	writes(['therefore, ',sentence(Q)]).
write_exp(Q,[p(_,C)|T]):-
	writes([sentence(C),'; ']),
	write_exp(Q,T).

% show current rulebase
show_rules([]):-
	show_answer(all).
show_rules([Rule|Rules]):-
	show_answer(sentence(Rule)),
	show_rules(Rules).

% show current rulebase as clauses
show_kb([]):-
	show_answer(all).
show_kb([Rule|Rules]):-
	show_answer(clause(Rule)),
	show_kb(Rules).
	
%%% Query answering %%%

answer_query(Query,Rulebase,answers(L)):-
	setof0(sentence(Query),Query^P^prove_rb(Query,Rulebase,P),L).
answer_query(Query,Rulebase,proof(Proof)):-
	( prove_rb(Query,Rulebase,Proof) -> true
	; otherwise -> Proof=no
	).

check(Rule,Rulebase,implied):-
	try((numbervars(Rule,0,_),
	     Rule=c(H:-B),
	     body2rules(B,Rulebase,RB2),
	     prove_rb(H,RB2,_)
	   )).

all_answers(PN,Rulebase):-
	forall((pred(P,1,_),Q=..[P,PN],prove_rb(Q,Rulebase,_)),show_answer(sentence(Q))).

all_explanations(PN,Rulebase):-
	forall((pred(P,1,_),Q=..[P,PN],prove_rb(Q,Rulebase,Proof)),show_answer(explain(Q,Proof))).

% meta-interpreter
prove_rb(Q,RB,RP):-
	prove_rb(Q,RB,[],P),
	reverse(P,RP).

prove_rb(c(H:-B),Rulebase,P0,P):-!,
	numbervars(c(H:-B),0,_),
	body2rules(B,Rulebase,RB2),
	prove_rb(H,RB2,P0,P).
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause(c(A:-C),Rule,Rulebase),
	conj_append(C,B,D),
	prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
	find_clause(c(A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
prove_rb(A,Rulebase,P0,[p(A,Rule)|P]):-
	find_clause(d((A:-B,not(C))),Rule,Rulebase),
	prove_rb(B,Rulebase,P0,P),
	not prove_rb(C,Rulebase,P,_).

body2rules((A,B),Rs0,Rs):-!,
	body2rules(A,Rs0,Rs1),
	body2rules(B,Rs1,Rs).
body2rules(A,Rs0,[c(A:-true)|Rs0]).

% find applicable clause in rulebase
find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,Clause).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

:-Cs=[
c((mortal(X):-human(X))),
c((human(X):-woman(X))),
c((human(X):-man(X))),
c((woman(helena):-true)),
c((man(socrates):-true)),
d((fly(X):-bird(X),not penguin(X))),
c((bird(tweety):-true))
],assert(kb(ex,Cs)).
