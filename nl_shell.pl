%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 7 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: nl_shell/1                             %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 7.3  Interpretation of natural language %%%

:-op(600,xfy,'=>').

sentence(C)  --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).
sentence([(L:-true)])      --> proper_noun(N,X),verb_phrase(N,X=>L).
verb_phrase(s,M)           --> [is],property(s,M).
verb_phrase(p,M)           --> [are],property(p,M).
property(s,M)              --> [a],noun(s,M).
property(p,M)              --> noun(p,M).
property(N,X=>mortal(X))   --> [mortal].
determiner(s,X=>B,X=>H,[(H:-B)])                    --> [every].
determiner(p,sk=>H1,sk=>H2,[(H1:-true),(H2:-true)]) --> [some].
proper_noun(s,socrates)    --> [socrates].
noun(s,X=>human(X))        --> [human].
noun(p,X=>human(X))        --> [humans].
noun(s,X=>living_being(X)) --> [living],[being].
noun(p,X=>living_being(X)) --> [living],[beings].

question(Q)       --> [who],[is],property(s,X=>Q).
question(Q)       --> [is],proper_noun(N,X),property(N,X=>Q).
question((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),property(p,sk=>Q2).


% natural language shell
nl_shell(Rulebase):-
	get_input(Input),
	handle_input(Input,Rulebase).

handle_input(stop,Rulebase):-!.
handle_input(show,Rulebase):-!,
	show_rules(Rulebase),
	nl_shell(Rulebase).
handle_input(Sentence,Rulebase):-	% new rule
	phrase(sentence(Rule),Sentence),!,
	nl_shell([Rule|Rulebase]).
handle_input(Question,Rulebase):-	% question
	phrase(question(Query),Question),
	prove_rb(Query,Rulebase),!,
	transform(Query,Clauses),
	phrase(sentence(Clauses),Answer),
	show_answer(Answer),
	nl_shell(Rulebase).
handle_input(Question,Rulebase):- % illegal sentence or 
	show_answer('No'),               % no answer found
	nl_shell(Rulebase).

% show current rulebase
show_rules([]).
show_rules([Rule|Rules]):-
	phrase(sentence(Rule),Sentence),
	show_answer(Sentence),
	show_rules(Rules).

% meta-interpreter
prove_rb(true,Rulebase):-!.
prove_rb((A,B),Rulebase):-!,
	prove_rb(A,Rulebase),
	prove_rb(B,Rulebase).
prove_rb(A,Rulebase):-
	find_clause((A:-B),Rulebase),
	prove_rb(B,Rulebase).

% find applicable clause in rulebase
find_clause(Clause,[Rule|Rules]):-
	copy_element(Clause,Rule).	% don't instantiate Rule
find_clause(Clause,[Rule|Rules]):-
	find_clause(Clause,Rules).

%%% copy_element/2: see file 'library'

% transform query to answer
transform((A,B),[(A:-true)|Rest]):-!,
	transform(B,Rest).
transform(A,[(A:-true)]).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_answer(Answer):-
	write('! '),write(Answer),nl.



