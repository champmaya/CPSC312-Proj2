% functions that work on courses

:- include("coursedict.pl").

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

%totalNumberOfCredits([H|T], s) is true if X is the sum of all credits in the list of courses 
%test case: totalNumberOfCredits([], X). X = 0;
%test case: totalNumberOfCredits([course(phil,220)], X). X = 3
%test case: totalNumberOfCredits([course(cpsc,110), course(cpsc,121)], X). X = 8 
totalNumberOfCredits([], 0). 
totalNumberOfCredits([course(Name, Number)| T], S) :-
	credits(course(Name,Number), Credits),
	totalNumberOfCredits(T, ST),
	S is Credits+ST.
% findEligibleCourses(L,Ans) is true when Ans is a list of all the courses for which everything in L is a prerequisite.
findEligibleCourses([H|T], [H1|T1]) :- requires(H1, [H]), findEligibleCourses(T, T1).
findEligibleCourses([H|T], [H1|T1]) :- findEligibleCourses(T, [H1|T1]).
findEligibleCourses([H], [H1]) :- requires(H1,[H]).





