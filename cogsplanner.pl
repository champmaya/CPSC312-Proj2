%:- include("coursedict.pl").
:- include("coursefunctions.pl").

%unsure if we actually need these:
:- use_module(library(aggregate)).
:- use_module(library(apply)).


% Original code from Abel Waller and Gareth Antle. Their code found here: https://github.com/unoctium1/COGS-Module-Recommender. We have added information about other courses in the cpsc cogs degree. Not just modules.
% Furthermore we have improved the natural language capacity of the program so that more questions can be asked in more natural-sounding English.
% :- module(cogsmodules,[course/2,faculty/2,isModule/1,requires/2,newUser/0,go/1,isEquiv/2]). <-????? TODO 


% This a program that helps cogs students in the computer science stream plan their degree. It includes answers to queries about the degree itself, the number of credits required etc. 
% It requires the user to enter a list of courses already taken
% The user makes a query, and the program uses inputted information in additon to the existing knowledge 
% base in order to answer the question.

% our system distinguishes between eligible courses (where the user meets all the prerequisites), and 
% possible courses (where the user meets some of the prerequisites) <- not sure about this yet - Dev

% to begin enter:
%?- start.
%	Is course cpsc 320?
%	Is course cpsc 123?
%	What is a module course?
%	
% after entering some courses, try asking
%	What are my possible courses?
%	What are my eligible courses?
%
% once you've found some courses you're interested in taking (say, phil 451), try asking
% 	What is required for course phil 451?


% useful for reading and writing: http://alumni.cs.ucr.edu/~vladimir/cs171/prolog_2.pdf
%start([]) is true if .... getting error and I dont know why :) 
start([]) :- welcome.

%welcome is true if welcome message is written to the terminal
%example list to use while running program (copy & paste): [course(cpsc,110), course(cpsc,121), course(cogs,200), course(cogs,303), course(cogs,300), course(cpsc,312), course(cpsc,221), course(cpsc,322), course(cpscs,320), course(biol,361), course(pysc,101), course(biol,200), course(phil,220), course(phil,378), course(ling,100)]. 
welcome :- 
	write ('Welcome to the COGS - COMP SCI Degree Planner! This program allows you to ask many questions about the COGS degree so you can better plan for your graduation. Follow the instructions to begin planning your degree! ... ADD MORE DETAILS LATER '),
	nl,
	write('To begin, please list the courses you have already taken in the list format of course(dept, course#). For example if you have taken the courses PHIL 220 and CPSC 110, you would enter ?- [course(phil,220), course(cpsc,110)].  Please be careful when writing out all your courses'),
	Read(ListOfCoursesTaken),
	validateList(ListOfCoursesTaken),
	askQuestions(ListOfCoursesTaken), 

%Question: Alex, do you think we even need this type of function
%validateList(List) is true if List is a list of valid courses taken is not valid, based on the knowledge base in the course dict
validateList([]). 
validateList([course(X,Y)]).
validateList([H|T]) :- validateList(H), validateList(T).
validateList([course(X,Y)|T]) :- 
	\+ course(X,Y)
	write(course(X,Y)), write('Is not a valid course name, please renter the courses you have taken'),
	welcome.

%askQuestion(List) is true if ....
askQuestion(List) :-
	write('Now that you have entered the courses you have taken feel free to ask me questions! Some example queries are: TODO (give proper syntax), for a comprehensive list of questions you can ask please see the README found on github. Thank you!'),
	write("Ask me anything: "), 
	read(Question), % not sure about syntax here! TODO 
	ask(Question, _),


	
% NATURAL LANGUAGE PARSER


% question(Question,QR,Object) is true if Query provides an answer about Object to Question
question(['Is' | T0],T2,Obj,St) :-
    noun_phrase(T0,T1,Obj,St),
    mp(T1,T2,Obj,St).
question(['What',is | T0], T1, Obj,St) :-
    mp(T0,T1,Obj,St).
question(['What',is | T0],T1,Obj,St) :-
    noun_phrase(T0,T1,Obj,St).
question(['What' | T0],T2,Obj,St) :-
    noun_phrase(T0,T1,Obj,St),
    mp(T1,T2,Obj,St).

% ask(Q,A) gives answer A to question Q. Is true if A is the answer to question Q. 
ask(Q,A,St) :-
    question(Q,[],A,St).


% States
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind,St) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind,St),
    noun(T2,T3,Ind,St),
    mp(T3,T4,Ind,St).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det([an | T], T, _).
det([are | T], T, _).
det(T,T,_).

% adjectives(T0,T1,Ind) is true if 
% T0-T1 is an adjective is true of Ind
adjectives(T0,T2,Ind,St) :-
    adj(T0,T1,Ind,St),
    adjectives(T1,T2,Ind,St).
adjectives(T,T,_,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,Subject,St) :-
    reln(T0,T1,Subject,Object,St),
    noun_phrase(T1,T2,Object,St).
mp([that|T0],T2,Subject,St) :-
    reln(T0,T1,Subject,Object,St),
    noun_phrase(T1,T2,Object,St).
mp(T,T,_,_).

% DICTIONARY

adj([faculty, of, Fac | T], T, Obj,_) :- faculty(Obj, Fac).
adj([Fac | T], T, Obj, _) :- faculty(Obj, Fac).

noun([eligible, course | T], T, Obj, St) :- isEligible(Obj, St).
noun([my, eligible, courses | T], T, Obj, St) :- isEligible(Obj, St).
noun([course, can, i, take | T], T, Obj, St) :- isEligible(Obj, St).

noun([course, Department, Number | T],T,course(Department, Number),_) :- course(Department,Number).
noun([module, course, Department, Number | T], T, course(Department, Number), _) :- isModule(course(Department, Number).
noun([course | T], T, course(Department, Number),_) :- course(Department,Number).
noun([module, course | T], T, Obj,_) :- isModule(Obj).
noun([my, possible, courses | T], T, Obj, []) :- noReqs(Obj).
noun([possible, courses | T], T, Obj, St) :- requires(Obj, ListCourses), member(Y, St), member(Y, ListCourses).
noun([my, possible, courses | T], T, Obj, St) :- requires(Obj, ListCourses), member(Y, St), member(Y, ListCourses).
noun([course, i, have, taken | T], T, Obj, St) :- member(Obj, St).

reln([required, for | T], T, Obj, Course,_) :- requires(Course, Obj).
reln([requires | T], T, Obj, Course,_) :- requires(Course, Obj).


	
