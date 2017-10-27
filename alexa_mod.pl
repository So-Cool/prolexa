:-module(alexa_mod,[alexa/1]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).

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
	get_intent(DictIn,IntentName),
	intent_dictOut(IntentName,DictIn,DictOut).

handle_dict(_DictIn,DictOut):-
	DictOut = _{
	      shouldEndSession: false,
	      response: _{outputSpeech:_{type: "PlainText", text: "Error Id did not match"}},
              version:"1.0"
	     }.

get_intent(DictIn,IntentName):-
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(name,IntentObject,IntentName).

/*
 *  Steps needed
* 1. check the app id
  2. Check the time stamp
* 3. Make the json responce
*/

intent_dictOut("getANewFact",_,DictOut):-
	answers(RandomMessage),
	my_json_answer(RandomMessage,DictOut).

intent_dictOut("forget",_,DictOut):-
	retractall(alexa_mod:sessionid_fact(_,_)),
	my_json_answer("I am a blank slate",DictOut).

intent_dictOut("KBdump",DictIn,DictOut):-
	%get_dict(session,DictIn,SessionObject),
	%get_dict(sessionId,SessionObject,SessionId),
	findall(Message,
			(alexa_mod:sessionid_fact(SessionId,Rule),
			 phrase(sentence(Rule),Sentence),
			 atomics_to_string(Sentence," ",Message)
			),
			Messages),
	atomic_list_concat(Messages,". ",Message),
	my_json_answer(Message,DictOut).

intent_dictOut("remember",DictIn,DictOut):-
	get_dict(session,DictIn,SessionObject),
	get_dict(sessionId,SessionObject,SessionId),
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(slots,IntentObject,SlotsObject),
	get_dict(mySlot,SlotsObject,MySlotObject),
	get_dict(value,MySlotObject,Value),
	portray_clause(user_error,Value),
	make_atomlist(Value,AtomList),
	( phrase(sentence(Rule),AtomList) ->
	  (assertz(alexa_mod:sessionid_fact(SessionId,Rule)),my_json_answer(Value,DictOut))
	).

intent_dictOut("question",DictIn,DictOut):-
	%writeln(user_error,walrus),
	get_dict(session,DictIn,SessionObject),
	get_dict(sessionId,SessionObject,SessionId),
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(slots,IntentObject,SlotsObject),
	get_dict(questionSlot,SlotsObject,MySlotObject),
	get_dict(value,MySlotObject,Value),
	portray_clause(user_error,Value),
	make_atomlist(Value,AtomList),
	( phrase(question(Query),AtomList),
	  prove_question(Query,SessionId,Answer) -> my_json_answer(Answer,DictOut)
	; otherwise -> my_json_answer('Unable to answer your question',DictOut)
	).

intent_dictOut(_,_,DictOut):-
	my_json_answer('Error parsing',DictOut).

prove_question(Query,SessionId,Answer):-
	portray_clause(user_error,Query),
	findall(Rule,alexa_mod:sessionid_fact(SessionId,Rule),Rulebase),
	prove_rb(Query,Rulebase),
	transform(Query,Clauses),
	phrase(sentence(Clauses),AnswerAtomList),
	atomics_to_string(AnswerAtomList," ",Answer).

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

answers(X):-
	random_member(X,["walruses can weigh up to 1900 kilograms", "There are two species of walrus - Pacific and Atlantic", "Walruses eat molluscs", "Walruses live in herds","Walruses have two large tusks"]).

string_rule(String,Rule):-
	string_lower(String,StringL),
	split_string(StringL," ","",Split),
	maplist(atom_string,AtomList,Split),
	phrase(sentence(Rule),AtomList).

make_atomlist(Value,AtomList):-
	split_string(Value," ","",StringList),
	maplist(string_lower,StringList,StringListLow),
	maplist(atom_string,AtomList,StringListLow).

sentence(C) --> determiner(N,M1,M2,C),
                noun(N,M1),
                verb_phrase(N,M2).

sentence([(L:-true)]) --> proper_noun(N,X),
                          verb_phrase(N,X=>L).

verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(p,M) --> [are], property(p,M).

property(s,M) --> [a], noun(s,M).
property(p,M) --> noun(p,M).

property(_N,X=>mortal(X)) --> [mortal].

determiner(s,X=>B,X=>H,[(H:-B)]) --> [every].
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

proper_noun(s,sam) --> [sam].
proper_noun(s,george) --> [george].
proper_noun(s,peter) --> [peter].
noun(s,X=>human(X)) --> [human].
noun(p,X=>human(X)) --> [humans].
noun(s,X=>living_being(X)) --> [living],[being].
noun(p,X=>living_being(X)) --> [living],[beings].

question(Q) --> [who],[is], property(s,_X=>Q).
question(Q) --> [is], proper_noun(N,X),
                property(N,X=>Q).
question((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),
	property(p,sk=>Q2).

prove_rb(true,_Rulebase):-!.
prove_rb((A,B),Rulebase):-!,
    prove_rb(A,Rulebase),
    prove_rb(B,Rulebase).

prove_rb(A,Rulebase):-
    find_clause((A:-B),Rulebase),
    prove_rb(B,Rulebase).

find_clause(Clause,[Rule|_Rules]):-
    my_copy_element(Clause,Rule).

find_clause(Clause,[_Rule|Rules]):-
    find_clause(Clause,Rules).

transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).

transform(A,[(A:-true)]).

get_input(Input):-
    write('? '), flush, read(Input).

show_answer(Answer):-
    write('! '), flush, write(Answer),nl.

my_copy_term(Old,New):-
    asserta('$copy'(Old)),
    retract('$copy'(New)),!.
my_copy_term(Old,_New):-
    retract('$copy'(Old)),
    !,fail.

my_copy_element(X,Ys):-
    member(X1,Ys),
    copy_term(X1,X).

testDict(InDict):-
InDict =
	_{
	  session: _SessionDict,
	  request: _{
		type: "IntentRequest",
		requestId: "EdwRequestId.0368645b-9a20-4c84-9fa4-57fa9ca96936",
		intent: _{
		  name: "forget",
		  slots: _{}
		},
		locale: "en-GB",
		timestamp: "2017-10-26T14:05:21Z"
	  },
	  context: _ContextDict,
	  version: "1.0"
	},
%%% details
SessionDict =
	_{
		new: false,
		sessionId: "SessionId.f9aae3a0-f8ce-4528-bf07-9c9a3f1922eb",
		application: _{
		  applicationId: "amzn1.ask.skill.a6672692-f5df-4e91-a17a-dfcc48f38919"
		},
		attributes: _{},
		user: _{
		  userId: "amzn1.ask.account.AGHOMUHPSYW7CI7XL5ZCQVH3RZVBUEGDVBTKKWWZQ35UQZDCZILOYCOLES4YXYO2RILIBTKF42YNHAUHTNSB5KLEDB7FJTUQQ2UQGDXVCFRMN3URKXEXMIAQUVGI2XA7HBINTZUNOYZGQG6QGSM4M67DSYRAWEDUFPEJQAXULWC3WGQ2SAKGABVCZJXRQMSESG755HXLYHCRVNI"
		}
	  },
ContextDict =
	_{
		'AudioPlayer': _{
		  playerActivity: "IDLE"
		},
		'System': _{
		  application: _{
			applicationId: "amzn1.ask.skill.a6672692-f5df-4e91-a17a-dfcc48f38919"
		  },
		  user: _{
			userId: "amzn1.ask.account.AGHOMUHPSYW7CI7XL5ZCQVH3RZVBUEGDVBTKKWWZQ35UQZDCZILOYCOLES4YXYO2RILIBTKF42YNHAUHTNSB5KLEDB7FJTUQQ2UQGDXVCFRMN3URKXEXMIAQUVGI2XA7HBINTZUNOYZGQG6QGSM4M67DSYRAWEDUFPEJQAXULWC3WGQ2SAKGABVCZJXRQMSESG755HXLYHCRVNI"
		  },
		  device: _{
			supportedInterfaces: _{}
		  }
		}
	  }.
