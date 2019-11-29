

% Original code from Abel Waller and Gareth Antle. Their code found here: https://github.com/unoctium1/COGS-Module-Recommender. We have added information about other courses in the cpsc cogs degree. Not just modules.
% Furthermore we have improved the natural language capacity of the program so that more questions can be asked in more natural-sounding English.


% This a program that helps cogs students in the computer science stream plan their degree. It includes answers to queries about the degree itself, the number of credits required etc. 
% It requires the user to enter a list of courses already taken
% The user makes a query, and the program uses inputted information in additon to the existing knowledge 
% base in order to answer the question.

% our system distinguishes between eligible courses (where the user meets all the prerequisites), and 
% possible courses (where the user meets some of the prerequisites) <- not sure about this yet - Dev


%:- include("coursedict.pl").
:- include("coursefunctions.pl").

%unsure if we actually need these:
:- use_module(library(aggregate)).
:- use_module(library(apply)).

% The dynamic declaration prevents undefined predicate errors.
:- dynamic start/0, welcome/0. 

:- discontiguous isEquiv/2. 


% useful for reading and writing: http://alumni.cs.ucr.edu/~vladimir/cs171/prolog_2.pdf
%start() 
start :- welcome.

%welcome is true if welcome message is written to the terminal
%example list to use while running program (copy & paste): [course(cpsc,110), course(cpsc,121), course(cogs,200), course(cogs,303), course(cogs,300), course(cpsc,312), course(cpsc,221), course(cpsc,322), course(cpsc,320), course(biol,361), course(psyc,101), course(biol,200), course(phil,220), course(phil,378), course(ling,100)]. 
welcome :- 
	write('Welcome to the COGS - COMP SCI Degree Planner! This program allows you to ask many questions about the COGS degree so you can better plan for your graduation. Follow the instructions to begin planning your degree! ... ADD MORE DETAILS LATER '),
	nl,
	write('To begin, please list the courses you have already taken in the list format of course(dept, course#). For example if you have taken the courses PHIL 220 and CPSC 110, you would enter ?- [course(phil,220), course(cpsc,110)].  Please be careful when writing out all your courses.'), flush_output(current_output), nl,
	readln(ListOfCoursesTaken),
	askQuestions(ListOfCoursesTaken), nl, 
	write("Finished asking questions: " ), 
	nl.

%Question: Alex, do you think we even need this type of function
%validateList(List) is true if List is a list of valid courses taken is not valid, based on the
 knowledge base in the course dict
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
	welcome().


%askQuestion(List) is true if ....
askQuestions(List) :-
	write('Now that you have entered the courses you have taken feel free to ask me questions! Some example queries are: TODO (give proper syntax), for a comprehensive list of questions you can ask please see the README found on github. Thank you!'), nl, nl, 
    write("Ask me: "), flush_output(current_output), nl,
    readln(Ln), nl, 
    ask(Ln,Ans,List),
	write("The answer is: "), nl,
	write(Ans). 


% ask(Q,A, ListOfCourses) gives answer A to question Q, based on the List of courses of given
ask(Q,A,ListOfCourses) :-
	   question(Q,A,ListOfCourses).


% NATURAL LANGUAGE PARSER


% question(Question,QR,Entity,ListOfCourses) is true if Query provides an answer about Entity to Question, given %the list of courses
question(['What',are | L0], L1, Entity) :-
    mp(L0,L1,Entity).
question(['What',are | L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).
question(['What' | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity),
    mp(L1,L2,Entity).
question(['How',many | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity).
question(['How',many | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity),
    mp(L1,L2,Entity).

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
adj([core | L], L, Enitity):- core(Entity).

noun([credits | L], L, Entity):- credits(Entity, N).
% Do we really need a 'degree' noun? noun([degree | L], L, Entity):-
noun([module, course | L], L, course(X, Y)).
noun([course | L], L, course(X, Y)).
noun([courses | L], L, course(X, Y)). %not sure about this one.
noun([pre-requisites | L], L, Entity):- (isEligible(Entity, X)).
noun([faculty | L], L, Entity):- course(Entity, _).

reln([requirments| L],L,course(X,Y),course(A,B)) :- requires(course(X,Y),course(A,B)).


