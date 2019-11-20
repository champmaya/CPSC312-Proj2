:- include("coursedict.pl").

%% Look, I have a made a change!!!! 

% Original code from Abel Waller and Gareth Antle. We have added information about other courses in the cpsc cogs degree. Not just modules.
% Furthermore we have improved the natural language capacity of the program so that more questions can be asked in more natural-sounding English.
% :- module(cogsmodules,[course/2,faculty/2,isModule/1,requires/2,newUser/0,go/1,isEquiv/2]).

:- use_module(library(aggregate)).
:- use_module(library(apply)).

% to use try:
%	go([]).

% A system for recommending Cogs Module courses, based on what courses a student has already taken.
% The user makes a query, and the program uses inputted information in additon to the existing knowledge 
% base in order to answer the question.

% our system distinguishes between eligible courses (where the user meets all the prerequisites), and 
% possible courses (where the user meets some of the prerequisites)

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

% ALL MODULES:
% Course    Faculty     Prereqs											Equivalents
% ANTH 417  ARTS		ANTH 100, LING 200
% ASIA 371  ARTS 														PHIL 371
% ASIA 378	ARTS														PHIL 378
% ASIA 470	ARTS														PHIL 470
% AUDI 402  AUDIOLOGY	ONE OF: PSYC 367, PSYC 368, LING 313, LING 314								
% AUDI 403	AUDIOLOGY	LING 200, LING 201, AUDI 402
% BIOL 361  SCIENCE		BIOL 200										
% BIOL 455  SCIENCE		ONE OF: BIOL 361, 362, 364, CAPS 301, PSYC 360
% BIOL 458  SCIENCE 	
% BIOL 459	SCIENCE		BIOL 455
% CPSC 304	SCIENCE		CPSC 221 or CPSC 260 and EECE 320, and ONE OF: CPSC 210, EECE 210, 309
% CPSC 310	SCIENCE		CPSC 210
% CPSC 311	SCIENCE		CPSC 210
% CPSC 312	SCIENCE		ONE OF: CPSC 210, EECE 210, EECE 309, CPEN 221
% CPSC 313 	SCIENCE 	CPSC 213, 221, or CPSC 210, 213, 260 EECE 320
% CPSC 314 	SCIENCE		ONE OF: MATH 200, 253, and ONE OF: MATH 152, 221,223 and CPSC 221 or CPSC 260 and EECE 320
% CPSC 317 	SCIENCE		CPSC 213 and CPSC 221 or CPSC 210, 260, EECE 320
% CPSC 319	SCIENCE		CPSC 310
% CPSC 320	SCIENCE		CPSC 221 or CPSC 260, EECE 320 and 2 MATH or STATS 200+ level courses
% CPSC 322	SCIENCE		CPSC 221 or CPSC 260, EECE 320, and ONE OF: CPSC 210, EECE 210, EECE 309
% CPSC 340  SCIENCE		ONE OF: MATH 152, 221, 223 and ONE OF: STAT 200, 203, 241, 251, 302, MATH 302, 318, BIOL 300 and CPSC 221 or CPSC 260, EECE 320 and ONE OF: CPSC 210, EECE 210, EECE 309
% CPSC 344	SCIENCE		ONE OF: CPSC 213, EECE 210, 309, CPEN 221
% CPSC 404	SCIENCE		CPSC 310 or ONE OF: EECE 310, CPEN 321, and ONE OF: EECE 315, CPEN 331
% CPSC 416	SCIENCE		ONE OF: CPSC 313, EECE 315, CPEN 331, and ONE OF: CPSC 317, EECE 358, ELEC 331
% CPSC 420	SCIENCE		CPSC 320
% CPSC 421	SCIENCE		CPSC 221 or CPSC 260, EECE 320
% CPSC 422	SCIENCE		CPSC 312, CPSC 322
% CPSC 425	SCIENCE		MATH 200, MATH 221, and CPSC 221 or CPSC 360, EECE 320
% CPSC 430	SCIENCE		3 credits of CPSC, and 3rd year standing
% CPSC 444	SCIENCE		CPSC 310, CPSC 344, ONE OF: STAT 200, 241
% CPSC 445	SCIENCE		CPSC 320, 6 credits BIOL, not BIOL 111
% LING 300	ARTS		LING 201
% LING 311	ARTS		LING 200
% LING 313	ARTS		LING 200
% LING 314	ARTS		LING 313
% LING 319	ARTS		LING 200, LING 201
% LING 327	ARTS		LING 201
% LING 345	ARTS		ONE OF: LING 201, ENGL 331, PHIL 220
% LING 405	ARTS		LING 300, LING 311
% LING 410	ARTS		LING 311
% LING 421	ARTS		LING 300
% LING 425	ARTS		LING 327 or PHIL 220
% LING 431	ARTS		LING 300, LING 311
% LING 432	ARTS		LING 431
% LING 447	ARTS
% LING 451	ARTS		LING 222, LING 311
% LING 452	ARTS		LING 222, LING 300
% MATH 302	SCIENCE		ONE OF: MATH 200, 217, 226, 253, 263			STAT 302
% MATH 303	SCIENCE		ONE OF: MATH 302, STAT 302
% MATH 344	SCIENCE		(MATH 152 or MATH 221 and ONE OF: MATH 220, 226, CPSC 121) or MATH 223
% MATH 443	SCIENCE		ONE OF: MATH 220, 226, CPSC 121 and 6 MATH 300-level credits
% MECH 421	APL. SCI 	MECH 366, COREQ: MECH 420
% MUSC 320	MUSC		MUSC 319
% MUSC 415	MUSC		MUSC 201, MUSC 210
% PHIL 320	ARTS		PHIL 220
% PHIL 321	ARTS		ONE OF: PHIL 125, 220
% PHIL 322	ARTS		PHIL 220
% PHIL 323	ARTS		PHIL 220
% PHIL 326	ARTS		PHIL 220 or LING 201 and 6 PHIL or LING 200-level credits
% PHIL 333	ARTS		
% PHIL 340	ARTS
% PHIL 369	ARTS
% PHIL 441	ARTS		PHIL 240 or COGS 200 and 3 200-level PHIL credits
% PHIL 450	ARTS		9 200-level PHIL credits
% PHIL 451	ARTS		PHIL 240 or COGS 200 and 3 200-level PHIL credits
% PHIL 455	ARTS		PHIL 240 or COGS 200 and 3 200-level PHIL credits
% PHIL 470	ARTS															ASIA 470
% PSYC 304	ARTS		PSYC 100 or PSYC 101, 102, or TWO OF: PSYC 207, 208, 217, 218, or PSYC 260
% PSYC 309	ARTS		PSYC 100 or PSYC 101, 102, or 6 200-level PSYC credits
% PSYC 321	ARTS		PSYC 100 or TWO OF: PSYC 101, 102, 205, 207, 208, 216, 217
% PSYC 333	ARTS		PSYC 100 or PSYC 101, 102 or 6 200-level PSYC credits
% PSYC 336	ARTS		PYSC 100 or PSYC 101, 102 or ENGL 329 or LING 420 or LING 200, 201
% PSYC 359	ARTS		PSYC 217, 218 or PSYC 366
% PSYC 366	ARTS		PYSC 260
% PSYC 367	ARTS		PSYC 100 or PSYC 101, 102, or 6 200-level PSYC credits
% PSYC 368	ARTS		PSYC 367
% PSYC 370	ARTS		ONE OF: PSYC 260, 270 and ONE OF: PSYC 217, 277, and ONE OF: PSYC 218, 278
% PSYC 371	ARTS		PSYC 370
% PSYC 460	ARTS		ONE OF: PSYC 304, 360
% PSYC 461	ARTS		ONE OF: PSYC 304, 360, 460
% PSYC 462	ARTS		ONE OF: PSYC 304, 360
% STAT 302	SCIENCE		ONE OF: MATH 200, 226, 217, 253, 263				MATH 302
% STAT 306	SCIENCE		ONE OF: MATH 152, 221, 223, and ONE OF: MATH 302, STAT 302 and ONE OF: STAT 200, 241, 251, 300, BIOL 300, COMM 291, ECON 325, 327, FRST 231, PYSC 218, 278, 366
% STAT 344	SCIENCE		ONE OF: STAT 200, 241, 251, BIOL 300, COMM 291, ECON 325, 327, FRST 231, PSYC 218, 278, 366 COREQS: MATH 302, or STAT 302
% STAT 406	SCIENCE		STAT 306 or CPSC 340

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

% run newUser. to start a new command
newUser :- go([]).

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