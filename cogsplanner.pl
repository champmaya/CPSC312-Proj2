

% Project based on work from Abel Waller and Gareth Antle. Their code found here: https://github.com/unoctium1/COGS-Module-Recommender. We have added information about other courses in the cpsc cogs degree. Not just modules. Updated the knowledge base. Added maany predicates. 
% Furthermore we have improved the natural language capacity of the program so that more questions can be asked in more natural-sounding English.
% Devyani McLaren % Alexander Mountain
s
% This a program that helps cogs students in the computer science stream plan their degree. It includes answers to queries about the degree itself, the number of credits required etc. 
% The user makes a query, and the program uses the existing knowledge 
% base in order to answer the question.

% our system distinguishes between eligible courses (where the user meets all the prerequisites), and 
% possible courses (where the user meets some of the prerequisites)s

:- include("coursedict.pl").
:- include("coursefunctions.pl").

% The dynamic declaration prevents undefined predicate errors.
:- dynamic start/0, welcome/0. 

:- discontiguous isEquiv/2. 


% ******************************************************MAIN PROGRAM************************************************

start :- welcome.

%welcome is true if welcome message is written to the terminal
%example list to use while running program (copy & paste): [course(cpsc,110), course(cpsc,121), course(cogs,200), course(cogs,303), course(cogs,300), course(cpsc,312), course(cpsc,221), course(cpsc,322), course(cpsc,320), course(biol,361), course(psyc,101), course(biol,200), course(phil,220), course(phil,378), course(ling,100)]. 
welcome :- 
	write('Welcome to the COGS - COMP SCI Degree Planner! This program allows you to ask many questions about the COGS degree so you can better plan for your graduation. Follow the instructions to begin planning your degree!'),
	nl,
	askQuestions, nl, 
	write("Finished asking questions. " ), 
	nl.


%askQuestion is true if the user asks a question and is provided an answer. 
askQuestions :-
	nl, nl,
	write('Ask some questions below. If you are unsure what to ask, check out the README on github (https://github.com/champmaya/CPSC312-Proj2) for some inspiration!'), nl, nl, 
    write("Ask me: "),nl, 
	flush_output(current_output), 
	readln(Q),
	%write(Q), nl, %this line for debugging purposes
    ask(Q,Ans),
	write("The answer is: "),
	write(Ans). 
	%askQuestions.  -> this helps with looping through questions, but it wont allow the system to provide all the answers. 

% ask(Q,A) gives answer A to question Q, based on the List of courses of given
ask(Q,A) :-
	   question(Q,A).
	     
	  
% ******************************************************POTENTIAL QUERIES & QUESTIONS************************************************

% isEquiv(course(math, 100), X).
% totalNumberOfCredits([course(cpsc,110), course(cpsc,121)], X).
% question(['How',many,credits,is,course,cpsc,110,?], Ans).
% question(['Tell',me,more,about,the,cogs,degree], Ans).
% question(['What',are,the,core,courses,in,year,3,?], Ans).
% question(['What',are,the,basic,requirements,of,the,cogs,degree], Ans). 
% question(['What',faculty,is,course,biol,200,in,?], Ans).
% question(['What',are,the,pre,-,requisites,for,course,cpsc,210,?], Ans).
% question(['What',courses,require,course,cpsc,110,?], Ans).
% question(['What',courses,require,course,biol,200,?], Ans).
% question(['What',courses,require,course,psyc,101,?], Ans).
% question(['What',courses,require,course,phil,220,?], Ans).
% question(['What',courses,have,3,credits,in,faculty,arts,?], Ans). 
% question(['What',courses,have,4,credits,in,faculty,science,?], Ans).
% How many credits is course cpsc 312?
% Tell me more about the cogs degree
% What are the core courses in year 3? 
% What are the basic requirements of the cogs degree?
% What faculty is course phil 220 in?
% What are the pre-requisites for course cpsc 210?
% What are the pre-requisites for course psyc 304? 
% What courses require course biol 200?
% What courses require course psyc 101? 
% What courses have 4 credits in faculty science? 


%% ******************************************************NATURAL LANGUAGE PARSER************************************************


% question(Question,Ans) is true if Query provides an answer (Ans) the the question (Question)
question(['How',many,credits,is,course,X,Y,?], Ans) :- credits(course(X,Y), Ans).
question(['Tell'|_], "Go to calendar.ubc.ca/vancouver/index.cfm?tree=12,215,410,421 to learn more information.").
question(['What',are,the,core,courses,in,year,X,?], Ans) :- core(Ans), year(Ans, X). 
%question(['Why',?], [course(cpsc,110)]). This line was used for debugging purposess
question(['What',are,the,basic,requirements|_], 
    "Overall, 120 credits are required, 12 credits worth of module courses, 
    3 of which must be a 400-level CPSC module course, and 9 of which must be non-CPSC module courses at the 300 level or above. 
    All core courses must be completed before graduating.").
question(['What',faculty,is,course,X,Y,in,?], Ans) :- faculty(course(X,Y), Ans).
question(['What',are,the,pre,-,requisites,for,course,X,Y,?], Ans) :- requires(course(X,Y), Ans). 
question(['What',courses,require,course,X,Y,?], Ans)  :- coursesThatRequire(course(X,Y), Ans, []). 
question(['What',courses,have,X,credits,in,faculty,Y,?], Ans) :- faculty(Ans,Y), credits(Ans, X).




/*
question(['What',are | L0], L1, Entity) :-
    mp(L0,L1,Entity).
question(['What',are | L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).
question(['What' | L0],End,L2,Entity) :-
    noun_phrase(L0,L1,Entity),
    mp(L1,L2,Entity).
question(['How',many | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity).
question(['How',many | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity),
    mp(L1,L2,Entity).
	*/ 

% noun_phrase(L0,L4,Entity) is true if
%  L0 and L4 are list of words, such that
%        L4 is an ending of L0
%        the words in L0 before L4 (written L0-L4) form a noun phrase
%  Entity is an individual that the noun phrase is referring to

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,Entity) :-
    det(L0,L1,Entity),
    adjectives(L1,L2,Entity),
    noun(L2,L3,Entity),
    mp(L3,L4,Entity).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | L],L,_).
det([a | L],L,_).
det(L,L,_).

% adjectives(L0,L1,Entity) is true if 
% L0-L1 is a sequence of adjectives that true of Entity
adjectives(L0,L2,Entity) :-
    adj(L0,L1,Entity),
    adjectives(L1,L2,Entity).
adjectives(L,L,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(L0,L2,Subject) :-
    reln(L0,L1,Subject,Object),
    noun_phrase(L1,L2,Object).
mp([that|L0],L2,Subject) :-
    reln(L0,L1,Subject,Object),
    noun_phrase(L1,L2,Object).
mp(L,L,_).

% adj(L0,L1,Entity) is true if L0-L1 
% is an adjective that is true of Entity

%adj([module | L], L, Enitity):- isModule(Entity).
%adj([core | L], L, Enitity):- core(Entity).

%noun([credits | L], L, Entity):- credits(Entity, N).
% Do we really need a 'degree' noun? noun([degree | L], L, Entity):-
%noun([module, course | L], L, course(X, Y)).
%noun([course | L], L, course(X, Y)).
%noun([courses | L], L, course(X, Y)). %not sure about this one.
%noun([pre-requisites | L], L, Entity):- (isEligible(Entity, X)).
%noun([faculty | L], L, Entity):- course(Entity, _).

reln([requirments| L],L,course(X,Y),course(A,B)) :- requires(course(X,Y),course(A,B)).


%validateList(List) is true if List is a list of valid courses taken is not valid, based on the knowledge base in the course dict
% test case: validateList([]). -> should produce true
% test case: validateList([course(cpsc,110)]). -> should produce true
% test case: validateList([course(cpsc,110), course(cpsc,121), course(cogs,200), course(cogs,303), course(cogs,300), course(cpsc,312), course(cpsc,221), course(cpsc,322), course(cpsc,320), course(biol,361), course(psyc,101), course(biol,200), course(phil,220), course(phil,378), course(ling,100)]).
% test case: validateList([course(cpsc,110), course(cpsc,121)]).
validateList([]). 
validateList([course(Dept, Num)]) :- course(Dept, Num).
validateList([H|T]) :- validateList(H), validateList(T).
validateList([course(X,Y)|_]) :- 
	\+ course(X,Y),
	write(X), write(Y), 
	nl, 
	write('Is not a valid course name, please renter the courses you have taken'),
	nl.
	%welcome.
	
	
	

	
	
	


