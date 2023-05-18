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
	( prove_rb(Query,Rulebase,Assertion,[],_) ->
		transform(Query,Assertion,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).	

% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,Assertion,[],_) ->
		transform(Query,Assertion,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).	


%%% Extended version of prove_question/3 that constructs a proof tree %%%
explain_question([Query],SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( 	numbervars(Query,0,_),
		Query=(H:-B),
		prove_rb(H,Rulebase,Assertion,[],Proof) ->
		(subsumes_term(B, Assertion) ->
              true ;
              fail),
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(H:-Assertion)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).

explain_question([Q1,Q2], SessionId, Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( 	Q1=(H1:-true),
		Q2=(H2:-Assertion),
		prove_rb((H1,H2),Rulebase,Assertion,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		list_to_set(Msg, Msg2),
		phrase(sentence1([(Q1),(Q2)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg2,[Last],Messages),
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
	     %add_body_to_rulebase(B,Rulebase,RB2), %! This seemed to be completely pointless except to remove the singleton variables warning
	     % Does there exists a proof for this rule, positively or negatively?
		 prove_rb(H,Rulebase, Assertion, _,_),
		 % If the assertion matches B then the rule is known
		(subsumes_term(B, Assertion) ->
              true ;
              fail)
	   )).

%? checks if a rule consisting of 2 parts (bird(sk):-true, penguin(sk):-true) is stored in rulebase
known_rule([R1,R2],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	Rule=[R1,R2],
	 try((numbervars(Rule,0,_),
		(member(Rule, Rulebase) ->
              true ;
              fail)
	   )).


%! Mine
rule_conflict([Rule],SessionId,Reason):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	numbervars(Rule,0,_),
	Rule=(H:-B),
	%! Bracketed terms here. Either prove_rb will complete, and the subsumes_term will say whether the rule matched a true/false term
	%! Or prove_rb will not complete, meaning that whatever the input rule could be parsed into was not represented in the rulebase
	%! at all. In this case, conflict_rb will check whether the input rule conflicts with any of the rules in the rulebase, such as
	%! if john is a teacher, and all teachers are happy, and the input rule is 'john is not happy', this will cause the conflict_rb
	%! to return true.
	((prove_rb(H,Rulebase,A1,[],Proof),
	% Should resolve to true when B does not match assertion, and there is a conflict
		(subsumes_term(B, A1) ->
				fail ;
				true)),(Res=A1);
	conflict_rb(H,Rulebase,[],Proof),Res=false),
	maplist(pstep2message,Proof,Msg),
	phrase(sentence1([(H:-Res)]),L),
	atomic_list_concat([therefore|L]," ",Last),
	append(Msg,[Last],Messages),
	atomic_list_concat(Messages,"; ",Reason).

conflict_rb(A,Rulebase,P0,P):-
	find_clause((B:-A),Rule,Rulebase),
	prove_rb(B,Rulebase,Assertion,[p(A,Rule)|P0],P), %! B = happy(peter), which will return false as the assertion
	(subsumes_term(true, Assertion) -> %! The result of the proof of the dependant term should be true.
				fail ;
				true).

%! Redundant
add_body_to_rulebase((A,B),Rs0,Rs):-!,
	add_body_to_rulebase(A,Rs0,Rs1),
	add_body_to_rulebase(B,Rs1,Rs).
add_body_to_rulebase(A,Rs0,[[(A:-true)]|Rs0]). 


%%% meta-interpreter that constructs proofs %%%

% 3d argument is accumulator for proofs
prove_rb([true,true],_Rulebase,P,P) :- !.
prove_rb((A,C),Rulebase,P0,P):-
	find_clause([(A:-B),D],Rule,Rulebase),
	(
		var(D) ->  		%%% D is uninstantiated
		prove_rb([B,C],Rulebase,[p(A,Rule)|P0],P) 
		; 
		D = (C:-E),
		prove_rb([B,E],Rulebase,[p(A,Rule),p(C,Rule)|P0],P)
	).
prove_rb((A,B),Rulebase, true, P, P):-
	Query = [(A:-true),(B:-true)],
	find_clause(Query, Rule, Rulebase),
	find_clause((B:-C), Rule, Rulebase),
	prove_rb(C,Rulebase).
	
prove_rb(true,_Rulebase,true,P,P):-!.
prove_rb(false,_Rulebase,false,P,P):-!.
prove_rb((A,B),Rulebase,Assertion,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,Assertion,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,Assertion,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,Assertion,[p(A,Rule)|P0],P).

% top-level version that ignores proof
prove_rb(Q,RB):-
	prove_rb(Q,RB,_,[],_P).

%%% Utilities from nl_shell.pl %%%

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).
	% add find clause with 2 parts
% find_clause(Clause,Rule,[Rule|_Rules]):-
% 	copy_term(Rule,Clause).	#
find_clause(Clause,Rule,[Rule|_Rules]):-
	Rule = [Rule1|Rule2],
	(
		Clause = [Clause1,Clause2] ->
		(
			copy_term([Rule1],[Clause1]) ->
			( Rule2 = [El|_] -> Clause2 = El ; true )
			; copy_term(Rule2,[Clause1]),
			Clause2 = Rule1
		)
		;
		(
			copy_term([Rule1],[Clause]) ->
			true
		; 	copy_term(Rule2,[Clause])
		)
	).

find_clause(Clause,Rule,[_Rule|Rules]):- 
	find_clause(Clause,Rule,Rules).

% transform instantiated, possibly conjunctive, query to list of clauses
transform((A,B),Val,[(A:-Val)|Rest]):-!,
    transform(B,Val,Rest).
transform(A,Val,[(A:-Val)]).


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

