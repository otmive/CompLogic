:-module(prolexa,[prolexa/1]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- consult(meta).	% meta-interpreter
:- consult(grammar).	% NLP grammar

:- dynamic sessionid_fact/2.

prolexa(Request):-
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


handle_intent("utterance",DictIn,DictOut):-
	SessionId=DictIn.session.sessionId,
	Utterance=DictIn.request.intent.slots.utteranceSlot.value,
	handle_utterance(SessionId,Utterance,Answer),
	my_json_answer(Answer,DictOut).

handle_intent(_,_,DictOut):-
	my_json_answer('Please try again',DictOut).

handle_utterance(SessionId,Utterance,Answer):-
	write_debug(utterance(Utterance)),
	make_atomlist(Utterance,AtomList),
	( phrase(sentence(Rule),AtomList),
	  prove_rule(Rule,SessionId) ->
		write_debug(rule(Rule)),
		atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	; phrase(sentence(Rule),AtomList) ->
		write_debug(rule(Rule)),
		assertz(prolexa:sessionid_fact(SessionId,Rule)),
		atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	; phrase(question(Query),AtomList),
	  write_debug(query(Query)),
	  prove_question(Query,SessionId,Answer) -> true
	; phrase(command(g(Goal,Answer)),AtomList),
	  write_debug(goal(Goal)),
	  call(Goal) -> true
	; otherwise -> atomic_list_concat(['I heard you say, ',Utterance,', could you rephrase that please?'],' ',Answer)
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

