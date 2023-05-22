This report outlines the methods used to extend the reasoning capabilitiies of the Prolexa Q&A assistant. 

## Implementing Existential Quantification

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

Q: 'some humans are geniuses'
A: I will remember that some humans are geniuses
Q: 'are some humans geniuses'
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

Q: 'some humans are not geniuses'
A: I will remember that some humans are not geniuses
Q: 'are some humans geniuses'
A: Sorry, I don't think this is the case 