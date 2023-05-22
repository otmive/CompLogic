Assignment for module Computational Logic for Artificial Intelligence to extend the reasoning capabilitiies of the Prolexa Q&A assistant to include negation and existential quantification. 
By Harry Field and Beth Pearson. 

## Implementing Negation (Harry Field)
### Introduction
In order to implement negation, the given program must be altered in a number of ways. Firstly, the grammatical framework for communicating to the user was implemented. This granted insight into the logical tokens that would be required by the rest of the program in order to construct such a communication. Namely, these were the stored rules, the methods of assessing the state of the stored rules and the methods of retrieving the rules necessary for sentence construction. 

The main challenge of this project was learning the nature of the program from a logical perspective. Being trained in procedural programming, this required first gaining an understanding of the program from my perspective, then converting this into a set of logical constructs that could be tailored to suit my requirements. 

To understand the program from a procedural perspective, predicates like spy/1 and trace/0 were helpful in understanding the flow of information through the program and determining the proper moment to intervene and make alterations.

### The Stored Rules
The base program did not have a method for implementing negation in the stored rules. To construct one, the requirement is that the programmer creates a method of signifying negation such that another area of the program can interpret it as negation effectively. With this in mind, the specific method of storing negation became centred on what made the negation readable and the program at large easy to implement. With the rules for truth statements being of the form *predicate(thing):-true*, it seemed logical to implement negation as *predicate(thing):-false*. This enabled easy implementation in the prolexa engine and permitted the sentence structure in the prolexa grammar to be copied from the truth statements. Another form of representation was considered (*:-predicate(thing)*), but this was decided against because of difficulties with handling the syntax in the prolexa engine.

### Assessing the State of the Stored Rules
The core element of the base program was the prove_rb predicate in the prolexa engine. This predicate was able to complete if a given rule was found to be true in the stored rules, and fail if the rule was not present in the stored rules. The ability of this predicate was extended to enable checking for explicit negation, stored in the same form as the truth predicate, swapping out *true* for *false*. As well as this, a true/false flag was added to the predicate which could be passed back to the element of the program which requested it. While passing flags around in code isn't optimal, the behaviour enabled the extension of functionality with minimal alterations to the existing program. With this implemented, the program could now check for explicit assertion/negation as well as the absence of a rule entirely. 

### Rules That Depend On Rules
Some of the stored rules are of the form *first_predicate(thing):-second_predicate(thing)*, meaning that if the second predicate is explicitly negated, the first predicate cannot be. In order for negation to be implemented properly, incoming statements from the user needed to be checked for conflicts with the rulebase before they were implemented. Take the example *happy(X):-teacher(X)*, combined with the truth statement *teacher(peter):-true*. According to this example, Peter is happy, deduced from the fact that he is a teacher. However, the explicit rule to express Peter's happiness is never stored in the rulebase. In order for the rulebase to be checked properly against new input, the rules that can be deduced from the rulebase must also be assessed. 

To assess incoming statements against all knowledge that can be deduced from the rulebase for conflict, a new predicate rule_conflict/3 was created. This achieved the desired behaviour and returned information to other areas of the program such that the proper reply could be presented back to the user.

### Example 
This example began with the following stored rules in the program. Not all rules were used in this example:
```
stored_rule(1,[(mortal(X):-human(X))]).
stored_rule(1,[(human(peter):-true)]).
stored_rule(1,[(happy(X):-teacher(X))]).
stored_rule(1,[(human(john):-true)]).
stored_rule(1,[(teacher(john):-true)]).
stored_rule(1,[(happy(peter):-false)]).
```

Q: 'is john a teacher'.

A: john is a teacher

Q: 'is john happy'.

A: john is happy

Q:  'john is not happy'.

A: It cannot be the case that john is not happy because john is a teacher; every teacher is happy; therefore john is happy

Q: 'forget that john is a teacher'.

A: I erased it from my memory

Q:  'is john a teacher'.

A: Sorry, I don't think this is the case

Q:  'is john happy'.

A: Sorry, I don't think this is the case

Q: 'john is not happy'.

A: I will remember that john is not happy

Q:  'john is a teacher'.

A: It cannot be the case that john is a teacher because john is not happy; every teacher is happy; therefore john is not a teacher

## Implementing Existential Quantification (Beth Pearson)

### Introduction
The implementation of existential quantification consisted of 3 main steps. The first step was to expand the vocabulary to allow Prolexa to understand sentences containing 'some', 'genius', 'prizes' and 'win'. Next changes were made to the meta-interpreter to allow prooving and understanding of rules made up of two parts eg. `(X:-true),(Y:-true)`. Finally an additional sentence structure was added to allow negation to be used in conjunction with existential quantification eg. "Some humans are not geniuses". Multiple changes to the Prolexa program were made to implement these new functionalities which are outlined below.

### Question Answering and Vocubulary

The following predicates were added to `prolexa_grammar.pl` to allow Prolexa to understand the nouns genius and prize and also the verb win.
```
pred(genius,1,[n/genius]).

pred(prize,1,[n/prize]).

pred(win,1,[v/win]).
```

A further addition to the list of verb phrases specifying prizes as being preceded by an intransitive verb. 
```
verb_phrase(N,M) --> iverb(N,M),[prizes].
```

We also defined the irregular plural of the noun 'genius'.
```
noun_s2p(Noun_s,Noun_p):-

    ( Noun_s=woman -> Noun_p=women

    ; Noun_s=man -> Noun_p=men

    ; Noun_s=genius -> Noun_p=geniuses

    ; atom_concat(Noun_s,s,Noun_p)

).
```

A new determiner was added defining sentences containing Skolem constants and an additional question form was then added to alow the parsing of questions beginning 'are some'.
```
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

question1((Q1,Q2)) --> [are,some],noun(p,sk=>Q1),property(p,sk=>Q2).
```
The final step needed for Prolexa to be able to handle existential quantification questions was to add a new instance of  `known_rule` which checks if a rule consisting of two parts eg.  (bird(sk):-true, penguin(sk):-true) is stored in the rulebase. 
```
known_rule([R1,R2],SessionId):-

    findall(R,prolexa:stored_rule(SessionId,R),Rulebase),

    Rule=[R1,R2],

    try((numbervars(Rule,0,_),

    (member(Rule, Rulebase) ->

    true ;

    fail)

)).
```

### Example 

Q: 'some humans are geniuses'.

A: I will remember that some humans are geniuses

Q: 'are some humans geniuses'.

A: some humans are geniuses

### Reasoning Explanation

The following changes were made to enable Prolexa to infer from the statements "some humans are geniuses" and "geniuses win prizes" that "some humans win prizes".

An additional `explain_question` rule was added which matches with sentences consisting of two parts. The utterance "explain why some humans win prizes" is first handled by `command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence(Q).` extracting the sentence `(human(sk):-true), (win(sk):-true)` which is input to the explain question rule. 

```
explain_question([Q1,Q2], SessionId, Answer):-

    findall(R,prolexa:stored_rule(SessionId,R),Rulebase),

    ( Q1=(H1:-true),

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
```

Additional `prove_rb` rules were also added to enable proving new sentences consisting of two parts. 
```
prove_rb((A,C),Rulebase,P0,P):-

    find_clause([(A:-B),D],Rule,Rulebase),

    (

    var(D) -> 

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
```

These new rules allow for the sentences of the form `(X:-true), (Y:-true)` to be handled and for the list of proof steps to be added accordingly to `P`.  A new `find_clause` rule was also required to complete the `prove_rb` update. 
```
find_clause(Clause, Rule, [Rule|_Rules]):-

    Rule = [R1|R2],

    (

    copy_term([R1],[Clause]) ->

    true

    ; copy_term(R2,[Clause])

).
```

This means sentences such as "some humans are geniuses" can be correctly located in the rulebase. 
Finally a determiner was added to enable "all geniuses win prizes" to be written simply as "geniuses win prizes".
```
determiner(p,X=>B,X=>H,[(H:-B)]) --> [].
```
### Example
The following example uses no initial stored rules. 

Q: 'some humans are geniuses'.

A: I will remember that some humans are genisues. 

Q:  'geniuses win prizes'.

A: I will remember that geniuses win prizes.

Q: 'explain why some humans win prizes'.

A: some humans are geniuses; every genius wins prizes; therefore some humans win prizes


### Existential Quantification with Negation

We also implemented the ability for Prolexa to handle sentences containing both existential quantification and negation for example "some humans are not geniuses". This was achieved by defining a new determiner using Skolem constants where one of the tails is `false`. 
```
determiner(p,sk=>H1,sk=>H2,[(H1:-true),(H2:-false)]) --> [some].
```

A limitation of this implementation is that it is not able to handle explanations of negated concepts. For example asking Prolexa "explain why some humans are not geniuses" will result in an incoherent answer. However, it can successfully hande the two notions of "some humans are geniuses" and "some humans are not geniuses" and store them as two separate logical rules. 

### Example

Q: 'some humans are not geniuses'.

A: I will remember that some humans are not geniuses

Q: 'are some humans geniuses'.

A: Sorry, I don't think this is the case 