:-module(alexa_mod,[alexa/1]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- consult(meta).	% meta-interpreter
:- consult(grammar).	% NLP grammar

:- dynamic sessionid_fact/2.

alexa(Request):-
	http_read_json_dict(Request,DictIn),
	RequestType = DictIn.request.type,
	( RequestType = "LaunchRequest" -> my_json_answer("I am Minerva, how can I help?",_DictOut)
    ; RequestType = "SessionEndedRequest" -> my_json_answer("Goodbye",_DictOut)
	; RequestType = "IntentRequest" -> 	IntentName = DictIn.request.intent.name,
										handle_intent(IntentName,DictIn,_DictOut)
	).

my_json_answer(Message,DictOut):-
	DictOut = _{
	      response: _{
	      				outputSpeech: _{
	      								type: "PlainText", 
	      								text: Message
	      							},
	      				shouldEndSession: false
	      			},
              version:"1.0"
	     },
	reply_json(DictOut).


%%% stuff for Prolog skill %%%

handle_intent("getANewFact",_,DictOut):-
	random_fact(Fact),
	my_json_answer(Fact,DictOut).

handle_intent("forget",_,DictOut):-
	retractall(alexa_mod:sessionid_fact(_,_)),
	my_json_answer("I am a blank slate",DictOut).

handle_intent("KBdump",DictIn,DictOut):-
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

handle_intent("remember",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Value=DictIn.request.intent.slots.mySlot.value,
	write_debug(Value),
	make_atomlist(Value,AtomList),
	( phrase(sentence(Rule),AtomList) ->
	  (assertz(alexa_mod:sessionid_fact(SessionId,Rule)),Answer=Value)
	; otherwise -> Answer='I am afraid I don\'t understand'
	),my_json_answer(Answer,DictOut).

handle_intent("question",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Value=DictIn.request.intent.slots.questionSlot.value,
	write_debug(Value),
	make_atomlist(Value,AtomList),
	( phrase(question(Query),AtomList),
	  prove_question(Query,SessionId,Answer) -> true
	; otherwise -> Answer='I am afraid I can\'t answer your question'
	),my_json_answer(Answer,DictOut).

%%% this one is for Minerva skill

handle_intent("utterance",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Utterance=DictIn.request.intent.slots.utteranceSlot.value,
	handle_utterance(SessionId,Utterance,Answer),
	my_json_answer(Answer,DictOut).

%%% this one is for Test skill

handle_intent("catchAllIntent",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Utterance=DictIn.request.intent.slots.utteranceSlot.value,
	handle_utterance(SessionId,Utterance,Answer),
	my_json_answer(Answer,DictOut).

handle_intent(_,_,DictOut):-
	my_json_answer('Please try again',DictOut).

random_fact(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).

handle_utterance(SessionId,Utterance,Answer):-
	write_debug(utterance(Utterance)),
	make_atomlist(Utterance,AtomList),
	( phrase(sentence(Rule),AtomList),
	  prove_rule(Rule,SessionId) ->
		write_debug(rule(Rule)),
		atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	; phrase(sentence(Rule),AtomList) ->
		write_debug(rule(Rule)),
		assertz(alexa_mod:sessionid_fact(SessionId,Rule)),
		atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	; phrase(question(Query),AtomList),
	  write_debug(query(Query)),
	  prove_question(Query,SessionId,Answer) -> true
	; phrase(command(g(Goal,Answer)),AtomList),
	  write_debug(goal(Goal)),
	  call(Goal) -> true
	; otherwise -> atomic_list_concat(['I heard you say: ',Utterance,', but I\'m afraid I don\'t understand what you want me to do'],' ',Answer)
	),
	write_debug(answer(Answer)).

make_atomlist(Value,AtomList):-
	split_string(Value," ","",StringList),
	maplist(string_lower,StringList,StringListLow),
	maplist(atom_string,AtomList,StringListLow).

write_debug(Atom):-
	writeln(user_error,Atom),flush_output(user_error).


%%% test %%%

test:-
	read(Input),
	( Input=stop -> true
	; otherwise ->
		handle_utterance(1,Input,Output),
		writeln(Output),
		test
	).


mr('In epidemiology, Mendelian randomization is a method of using measured variation in genes of known function to examine the causal effect of a modifiable exposure on disease in observational studies. The design was first proposed in 1986 and subsequently described by Gray and Wheatley as a method for obtaining unbiased estimates of the effects of a putative causal variable without conducting a traditional randomised trial. These authors also coined the term Mendelian randomization. The design has a powerful control for reverse causation and confounding which otherwise bedevil epidemiological studies. Its current hero is George Davey-Smith.').

