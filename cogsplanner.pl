:- include("coursedict.pl").


% Original code from Abel Waller and Gareth Antle. Their code found here: https://github.com/unoctium1/COGS-Module-Recommender. We have added information about other courses in the cpsc cogs degree. Not just modules.
% Furthermore we have improved the natural language capacity of the program so that more questions can be asked in more natural-sounding English.
% :- module(cogsmodules,[course/2,faculty/2,isModule/1,requires/2,newUser/0,go/1,isEquiv/2]). - Devyani and Alex 

:- use_module(library(aggregate)).
:- use_module(library(apply)).

% to use try:
%	go([]).

% This a program that helps cogs students in the computer science stream plan their degree. It includes answers to queries about the degree itself, the number of credits required etc. 
% It requires the user to enter a list of courses already taken
% The user makes a query, and the program uses inputted information in additon to the existing knowledge 
% base in order to answer the question.

% our system distinguishes between eligible courses (where the user meets all the prerequisites), and 
% possible courses (where the user meets some of the prerequisites) <- not sure about this yet - Dev

% try asking:
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
%start([]) is true if ....
start :- welcome.

%welcome is true if welcome message is written to the terminal
%example list to use while running program (copy & paste): [course(cpsc,110), course(cpsc,121), course(cogs,200), course(cogs,303), course(cogs,300), course(cpsc,312), course(cpsc,221), course(cpsc,322), course(cpscs,320), course(biol,361), course(pysc,101), course(biol,200), course(phil,220), course(phil,378), course(ling,100)]. 
welcome :- 
	write ('Welcome to the COGS - COMP SCI Degree Planner! This program allows you to ask many questions about the COGS degree so you can better plan for your graduation. Follow the instructions to begin planning your degree! ... ADD MORE DETAILS LATER ')
	nl,
	write('To begin, please list the courses you have already taken in the list format of course(dept, course#). For example if you have taken the courses PHIL 220 and CPSC 110, you would enter ?- [course(phil,220), course(cpsc,110)].  Please be careful when writing out all your courses')
	Read(ListOfCoursesTaken),
	validateList(ListOfCoursesTaken).

%Question: Alex, do you think we even need this type of function
%validateList(List) is true if List is a list of valid courses taken, based on the knowledge base in the course dict
validateList([]). 
validateList([course(X,Y)]).
validateList([H|T]) :- validateList(H), validateList(T). 
	

% main program loop. L are courses taken
go(L) :-
	write('Ask me: '), flush_output(current_output),
	readln(Q),
	write('Add courses taken? y/n '),
	read(X),nl,
	((X == 'yes';X == 'y') -> addCourses(LN),append(L,LN,Courses);
		Courses = L),
	question(Q,End,Ans,Courses),
    member(End,[[],['?'],['.']]),
	writeln(Ans),
	fail.
	
	
% Queries users for new courses
addCourses([X|L]) :-
	writeln('Type courses taken in the form course(dept,123), or done if no more courses'),
	read(X),
	dif(X,done),
	addCourses(L).
	addCourses([]).	

% faculty(course(A,B),C) is true if course(A,B) is in faculty C
faculty(course(anth, _),arts).
faculty(course(asia, _),arts).
faculty(course(econ, _),arts).
faculty(course(engl, _),arts).
faculty(course(ling, _),arts).
faculty(course(phil, _),arts).
faculty(course(psyc, _),arts).
faculty(course(biol, _),science).
faculty(course(caps, _),science).
faculty(course(cogs, _),science).
faculty(course(cpsc, _),science).
faculty(course(math, _),science).
faculty(course(stat, _),science).
faculty(course(mech, _),appliedscience).
faculty(course(eece, _),appliedscience).
faculty(course(elec, _),appliedscience).
faculty(course(X,_),X) :- 
	dif(X,Y),
	\+ faculty(course(X,_),Y).

%noReqs(X) is true if X has no prerequisites.
noReqs(X) :- \+ requires(X,_).
	
% Equivalent courses: hasTaken(X) is true if an equivalent course has been taken	TODO: figure out how to get this to work both ways
isEquiv(X,Y) :- \+ dif(X,Y).

% coursesToTake(X,C,L) is true if C are pre reqs needed for X that haven't been taken. CoursesToTake(X,_) is false if isEligible(X) is true.

coursesToTake(X,C,L) :- length(C,_),coursesToTake2(X,C,L).

coursesToTake2(X,C,L) :-
	requires(X,Y),
	subtract(Y,L,C0),
	append(C0,C1,C),
	coursesToTakeRec(C0,C1,L).
coursesToTake2(X,[],L) :-
	isEligible(X,L).

coursesToTake1(X,C,L) :-
	requires(X,Y),
	subtract(Y,L,C).

coursesToTakeRec([H|T],C,L) :-
		coursesToTake2(H,C0,L),
		append(C0,C1,C),
		coursesToTakeRec(T,C1,L).
coursesToTakeRec([H|T],C,L) :-
		isEligible(H,L),
		coursesToTakeRec(T,C,L).
coursesToTakeRec([],[],_).
	
% isEligible(X,L) is true if the user has taken all required courses.
isEligible(X,L) :-
	(requires(X,Y);isEquiv(X,Z),requires(Z,Y)),
	foreach(member(H,Y),(hasTaken(H,L); isEquiv(H,I),hasTaken(I,L))).
isEligible(X,_) :-
	noReqs(X).
hasTaken(C,L) :- member(C,L).

% filterFaculty(Fac,L,L2) is true if L2 is the elements of L for which they are in faculty Fac
filterFaculty(Fac,[H|T],[H|T1]) :-
	faculty(H,Fac),
	filterFaculty(Fac,T,T1).
filterFaculty(Fac,[H|T],[H1|T1]) :-
	 \+ faculty(H,Fac),
	 dif(H,H1),
	 filterFaculty(Fac,T,[H1|T1]).
filterFaculty(Fac,[H|T],[]) :-
	\+ faculty(H,Fac),
	filterFaculty(Fac,T,[]).
filterFaculty(_,[],[]).


	





% NATURAL LANGUAGE PARSER
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

% ask(Q,A) gives answer A to question Q
ask(Q,A,St) :-
    question(Q,[],A,St).
	
% To get the input from a line:

q(Ans,St) :-
    write('Ask me: '), flush_output(current_output),
	readln(Ln),
    question(Ln,End,Ans,St),
    member(End,[[],['?'],['.']]),
	writeln(Ans),
	flush_output.