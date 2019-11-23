% Dictionary of basic course information for use by the cogs planner.

% credits(X, N) is true if N is the number of credits one gets from course X.

% core course credits
credits(course(engl, 100), 3).
credits(course(engl, 111), 3).
credits(course(engl, 112), 3).
credits(course(engl, 120), 3).
credits(course(engl, 121), 3).
credits(course(cpsc, 110), 4).
credits(course(cpsc, 103), 3).
credits(course(cpsc, 107), 3).
credits(course(cpsc, 121), 4).
credits(course(math, 100), 3).
credits(course(math, 102), 3).
credits(course(math, 120), 3).
credits(course(math, 180), 3).
credits(course(math, 184), 3).
credits(course(math, 101), 3).
credits(course(math, 103), 3).
credits(course(math, 105), 3).
credits(course(math, 121), 3).
credits(course(cogs, 200), 3).
credits(course(cpsc, 210), 4).
credits(course(cpsc, 221), 4).
credits(course(ling, 100), 3).
credits(course(phil, 220), 3).
credits(course(phil, 320), 3).
credits(course(stat, 200), 3).
credits(course(stat, 241), 3).
credits(course(cogs, 300), 4).
credits(course(cogs, 303), 3).
credits(course(cogs, 401), 3).
credits(course(cogs, 402), 3).
credits(course(cpsc, 312), 3).
credits(course(cpsc, 320), 3).
credits(course(cpsc, 322), 3).
credits(course(phil, 326), 3).
credits(course(phil, 351), 3).
credits(course(phil, 441), 3).
credits(course(phil, 451), 3).
credits(course(phil, 455), 3).
credits(course(psyc, 365), 3).

% modules
credits(course(anth, 100), 3).
credits(course(anth, 417), 3).
credits(course(asia, 371), 3).
credits(course(asia, 378), 3).
credits(course(asia, 470), 3).
credits(course(audi, 402), 3).
credits(course(audi, 403), 2).
credits(course(biol, 111), 3).
credits(course(biol, 200), 3).
credits(course(biol, 300), 3).
credits(course(biol, 361), 3).
credits(course(biol, 362), 2).
credits(course(biol, 364), 3).
credits(course(biol, 455), 3).
credits(course(biol, 458), 3).
credits(course(biol, 359), 3).
credits(course(caps, 301), 6).
credits(course(comm, 291), 3).
credits(course(cpen, 221), 4).
credits(course(cpen, 321), 4).
credits(course(cpen, 331), 4).
credits(course(cpsc, 304), 3).
credits(course(cpsc, 310), 3).
credits(course(cpsc, 311), 3).
credits(course(cpsc, 312), 3).
credits(course(cpsc, 314), 3).
credits(course(cpsc, 317), 3).
credits(course(cpsc, 319), 3).
credits(course(cpsc, 340), 3).
credits(course(cpsc, 344), 3).
credits(course(cpsc, 404), 3).
credits(course(cpsc, 416), 3).
credits(course(cpsc, 420), 3).
credits(course(cpsc, 421), 3).
credits(course(cpsc, 422), 3).
credits(course(cpsc, 425), 3).
credits(course(cpsc, 430), 3).
credits(course(cpsc, 444), 3).
credits(course(cpsc, 445), 3).
credits(course(ling, 430), 3).
% still lots of these to do (if we can stand it)

% course(X,Y) is true if Y is a valid course code in a department.
course(anth,100).
course(anth,417).
course(asia,371).
course(asia,378).
course(asia,470).
course(audi,402).
course(audi,403).
course(biol,111).
course(biol,200).
course(biol,300).
course(biol,361).
course(biol,362).
course(biol,364).
course(biol,455).
course(biol,458).
course(biol,459).
course(caps,301).
course(cogs,200). %new
course(cogs,300). %new 
course(cogs,303). %new
course(cogs,303). %new
course(cogs,401). %new
course(cogs,402). %new
course(comm,291).
course(cpen,221).
course(cpen,321).
course(cpen,331).
course(cpsc,103). %new
course(cpsc,107). %new
course(cpsc,110).
course(cpsc,121).
course(cpsc,210).
course(cpsc,213).
course(cpsc,221).
course(cpsc,260).
course(cpsc,304).
course(cpsc,310).
course(cpsc,311).
course(cpsc,312).
course(cpsc,313).
course(cpsc,314).
course(cpsc,317).
course(cpsc,319).
course(cpsc,320).
course(cpsc,322).
course(cpsc,340).
course(cpsc,344).
course(cpsc,404).
course(cpsc,416).
course(cpsc,420).
course(cpsc,421).
course(cpsc,422).
course(cpsc,425).
course(cpsc,430).
course(cpsc,444).
course(cpsc,445).
course(engl,100).
course(engl,111).
course(engl,112).
course(engl,120).
course(engl,121).
course(econ,325).
course(econ,327).
course(eece,210).
course(eece,309).
course(eece,310).
course(eece,315).
course(eece,320).
course(eece,358).
course(elec,331).
course(engl,329).
course(engl,331).
course(frst,231).
course(ling,100).
course(ling,101).
course(ling,200).
course(ling,201).
course(ling,222).
course(ling,300).
course(ling,311).
course(ling,313).
course(ling,314).
course(ling,319).
course(ling,327).
course(ling,345).
course(ling,405).
course(ling,410).
course(ling,420).
course(ling,421).
course(ling,425).
course(ling,431).
course(ling,432).
course(ling,447).
course(ling,451).
course(ling,452).
course(math,100). %new
course(math,101). %new
course(math,102). %new
course(math,103). %new
course(math,104). %new
course(math,105). %new
course(math,120). %new
course(math,121). %new
course(math,180). %new
course(math,184). %new
course(math,152).
course(math,200).
course(math,217).
course(math,220).
course(math,221).
course(math,223).
course(math,226).
course(math,253).
course(math,263).
course(math,302).
course(math,303).
course(math,318).
course(math,344).
course(math,443).
course(mech,366).
course(mech,420).
course(mech,421).
course(musc,201).
course(musc,210).
course(musc,319).
course(musc,320).
course(musc,415).
course(phil,125).
course(phil,220).
course(phil,240).
course(phil,260).
course(phil,320).
course(phil,321).
course(phil,322).
course(phil,323).
course(phil,326).
course(phil,333).
course(phil,340).
course(phil,351). %new
course(phil,369).
course(phil,371).
course(phil,378).
course(phil,441).
course(phil,450).
course(phil,451).
course(phil,455).
course(phil,470).
course(psyc,100).
course(psyc,101).
course(psyc,102).
course(psyc,205).
course(psyc,207).
course(psyc,208).
course(psyc,216).
course(psyc,217).
course(psyc,218).
course(psyc,260).
course(psyc,270).
course(psyc,277).
course(psyc,278).
course(psyc,304).
course(psyc,309).
course(psyc,321).
course(psyc,333).
course(psyc,336).
course(psyc,359).
course(psyc,360).
course(psyc,365). %new
course(psyc,366).
course(psyc,367).
course(psyc,368).
course(psyc,370).
course(psyc,371).
course(psyc,460).
course(psyc,461).
course(psyc,462).
course(stat,200).
course(stat,203).
course(stat,241).
course(stat,251).
course(stat,300).
course(stat,302).
course(stat,306).
course(stat,344).
course(stat,406).

% isModule(course(A,B)) is true if course(A,B) is a module
isModule(course(anth,417)).
isModule(course(asia,371)).
isModule(course(asia,378)).
isModule(course(asia,470)).
isModule(course(audi,402)).
isModule(course(audi,403)).
isModule(course(biol,361)).
isModule(course(biol,455)).
isModule(course(biol,458)).
isModule(course(biol,459)).
isModule(course(cpsc,304)).
isModule(course(cpsc,310)).
isModule(course(cpsc,311)).
isModule(course(cpsc,312)).
isModule(course(cpsc,313)).
isModule(course(cpsc,314)).
isModule(course(cpsc,317)).
isModule(course(cpsc,319)).
isModule(course(cpsc,320)).
isModule(course(cpsc,322)).
isModule(course(cpsc,340)).
isModule(course(cpsc,344)).
isModule(course(cpsc,404)).
isModule(course(cpsc,416)).
isModule(course(cpsc,420)).
isModule(course(cpsc,421)).
isModule(course(cpsc,422)).
isModule(course(cpsc,425)).
isModule(course(cpsc,430)).
isModule(course(cpsc,444)).
isModule(course(cpsc,445)).
isModule(course(ling,300)).
isModule(course(ling,311)).
isModule(course(ling,313)).
isModule(course(ling,314)).
isModule(course(ling,319)).
isModule(course(ling,327)).
isModule(course(ling,345)).
isModule(course(ling,405)).
isModule(course(ling,410)).
isModule(course(ling,421)).
isModule(course(ling,425)).
isModule(course(ling,431)).
isModule(course(ling,432)).
isModule(course(ling,447)).
isModule(course(ling,451)).
isModule(course(ling,452)).
isModule(course(math,302)).
isModule(course(math,303)).
isModule(course(math,344)).
isModule(course(math,443)).
isModule(course(mech,421)).
isModule(course(musc,320)).
isModule(course(musc,415)).
isModule(course(phil,320)).
isModule(course(phil,321)).
isModule(course(phil,322)).
isModule(course(phil,323)).
isModule(course(phil,326)).
isModule(course(phil,333)).
isModule(course(phil,340)).
isModule(course(phil,369)).
isModule(course(phil,441)).
isModule(course(phil,450)).
isModule(course(phil,451)).
isModule(course(phil,455)).
isModule(course(phil,470)).
isModule(course(psyc,304)).
isModule(course(psyc,309)).
isModule(course(psyc,321)).
isModule(course(psyc,333)).
isModule(course(psyc,336)).
isModule(course(psyc,359)).
isModule(course(psyc,366)).
isModule(course(psyc,367)).
isModule(course(psyc,368)).
isModule(course(psyc,370)).
isModule(course(psyc,371)).
isModule(course(psyc,460)).
isModule(course(psyc,461)).
isModule(course(psyc,462)).
isModule(course(stat,302)).
isModule(course(stat,306)).
isModule(course(stat,344)).
isModule(course(stat,406)).

% core(X) is true if course X is a core course for the cpsc cogs degree.
core(course(cpsc, 110)). % equivalent to taking 103 and 107. new
core(course(cpsc, 121)). %new
core(course(math, 100)). %new
core(course(math, 101)). %new
core(course(cogs, 200)). %new
core(course(cpsc, 210)). %new
core(course(cpsc, 221)). %new
core(course(ling, 100)). %new
core(course(phil, 220)). %new
core(course(stat, 200)). %new
core(course(cogs, 300)). %new
core(course(cogs, 303)). %new
core(course(cogs, 401)). %new
core(course(cogs, 402)). %new
core(course(cpsc, 312)). %new
core(course(cpsc, 320)). %new
core(course(cpsc, 322)). %new
core(course(phil, 326)). %new
core(course(psyc, 365)). %new

% requires(X,Y) is true if course X requires courses Y
requires(course(cpsc, 210),[course(cpsc,110)]).
requires(course(anth,417),[course(ling,200),course(anth,100)]).
requires(course(audi,402),[Y]) :- member(Y,[course(psyc,367),course(psyc,368),course(ling,313),course(ling,314)]).
requires(course(audi,403),[course(ling,200),course(ling,201),course(audi,402)]).
requires(course(biol,361),[course(biol,200)]).
requires(course(biol,455),[Y]) :- member(Y,[course(biol,361),course(biol,362),course(biol,364),course(caps,301),course(psyc,360)]).					
requires(course(biol,459),[course(psyc,455)]).
requires(course(cpsc,304),[course(cpsc,221),Y]) :- member(Y,[course(cpsc,210),course(eece,210),course(eece,309)]).
requires(course(cpsc,304),[course(cpsc,260),course(eece,320),Y]) :- member(Y,[course(cpsc,210),course(eece,210),course(eece,309)]).
requires(course(cpsc,310),[course(cpsc,210)]).
requires(course(cpsc,311),[course(cpsc,210)]).
requires(course(cpsc,312),[Y]) :- member(Y,[course(cpsc,210),course(eece,210),course(eece,309),course(cpen,221)]).
requires(course(cpsc,313),[course(cpsc,213),course(cpsc,221)]).
requires(course(cpsc,313),[course(cpsc,210),course(cpsc,213),cpurse(cpsc,260),course(eece,320)]).
requires(course(cpsc,314),[course(cpsc,221),X,Y]) :- 
	member(X,[course(math,200),course(math,253)]),
	member(Y,[course(math,152),course(math,221),course(math,223)]).
requires(course(cpsc,314),[course(cpsc,260),course(eece,320),X,Y]) :- 
	member(X,[course(math,200),course(math,253)]),
	member(Y,[course(math,152),course(math,221),course(math,223)]).
requires(course(cpsc,317),[course(cpsc,213),course(cpsc,221)]).
requires(course(cpsc,317),[course(cpsc,213),course(cpsc,210),course(cpsc,260),course(eece,320)]).
requires(course(cpsc,319),[course(cpsc,310)]).
requires(course(cpsc,320),[course(cpsc,221),course(X,Y),course(A,B)]) :-
	member(X,[stat,math]),
	member(A,[stat,math]),
	course(X,Y),
	course(A,B),
	dif(course(X,Y),course(A,B)),
	number(Y),
	number(B),
	Y >= 200,
	B >= 200.
requires(course(cpsc,320),[course(cpsc,260),course(eece,320),course(X,Y),course(A,B)]) :-
	member(X,[stat,math]),
	member(A,[stat,math]),
	course(X,Y),
	course(A,B),
	dif(course(X,Y),course(A,B)),
	number(Y),
	number(B),
	Y >= 200,
	B >= 200.
requires(course(cpsc,322),[course(cpsc,221),Y]) :- member(Y,[course(cpsc,210),course(eece,210),course(eece,309)]).
requires(course(cpsc,322),[course(cpsc,260),course(eece,320),Y]) :- member(Y,[course(cpsc,210),course(eece,210),course(eece,309)]).		
requires(course(cpsc,340),[course(cpsc,221),X,Y]) :-
	member(X,[course(math,152),course(math,221),course(math,223)]),
	member(Y,[course(math,302),course(math,318),course(biol,300),course(stat,200),course(stat,203),course(stat,241),course(stat,251),course(stat,302)]).
requires(course(cpsc,340),[course(cpsc,260),course(eece,320),X,Y,Z]) :-
	member(X,[course(math,152),course(math,221),course(math,223)]),
	member(Y,[course(math,302),course(math,318),course(biol,300),course(stat,200),course(stat,203),course(stat,241),course(stat,251),course(stat,302)]),
	member(Z,[course(cpsc,210),course(eece,210),course(eece,309)]).
requires(course(cpsc,344),[Y]) :- member(Y,[course(cpsc,213),course(eece,210),course(eece,309),course(cpen,221)]).
requires(course(cpsc,404),[course(cpsc,304),course(cpsc,213)]).	
requires(course(cpsc,404),[course(cpsc,304),course(cpsc,261)]).	
requires(course(cpsc,404),[X,Y]) :-
	member(X,[course(cpsc,313),course(eece,315),course(cpen,331)]),
	member(Y,[course(cpsc,317),course(eece,358),course(elec,331)]).
requires(course(cpsc,420),[course(cpsc,320)]).
requires(course(cpsc,421),[course(cpsc,221)]).
requires(course(cpsc,421),[course(cpsc,260),course(eece,320)]).
requires(course(cpsc,422),[course(cpsc,312),course(cpsc,322)]).
requires(course(cpsc,425),[course(math,200),course(math,221),course(cpsc,221)]).
requires(course(cpsc,425),[course(math,200),course(math,221),course(cpsc,360),course(eece,320)]).

% also 3rd year standing, prolly just shouldn't bother with that tho
requires(course(cpsc,430),[course(cpsc,X)]) :-
	course(cpsc,X).

requires(course(cpsc,444),[course(cpsc,310),course(cpsc,344),course(stat,200)]).
requires(course(cpsc,444),[course(cpsc,310),course(cpsc,344),course(stat,241)]).
requires(course(cpsc,445),[course(cpsc,320),course(biol,X),course(biol,Y)]) :-
	course(biol,X),
	course(biol,Y),
	dif(X,Y).
requires(course(ling,300),[course(ling,201)]).
requires(course(ling,311),[course(ling,200)]).
requires(course(ling,313),[course(ling,200)]).
requires(course(ling,314),[course(ling,313)]).
requires(course(ling,319),[course(ling,200),course(ling,201)]).
requires(course(ling,327),[course(ling,201)]).
requires(course(ling,345),[Y]) :- member(Y,[course(ling,201),course(engl,331),course(phil,220)]).
requires(course(ling,405),[course(ling,300),course(ling,311)]).
requires(course(ling,410),[course(ling,311)]).
requires(course(ling,421),[course(ling,300)]).
requires(course(ling,425),[course(ling,327)]).
requires(course(ling,425),[course(phil,220)]).
requires(course(ling,431),[course(ling,300),course(ling,311)]).
requires(course(ling,432),[course(ling,431)]).
requires(course(ling,451),[course(ling,222),course(ling,311)]).
requires(course(ling,452),[course(ling,222),course(ling,300)]).
requires(course(math,302),[Y]) :- member(Y,[course(math,200),course(math,217),course(math,226),course(math,253),course(math,263)]).
requires(course(math,303),[course(math,302)]).
requires(course(math,303),[course(stat,302)]).
requires(course(math,344),[course(math,223)]).
requires(course(math,344),[X,Y]) :-
	member(X,[course(math,152),course(math,221)]),
	member(Y,[course(math,220),course(math,226),course(cpsc,121)]).
requires(course(math,443),[X,course(math,Y),course(math,Z)]) :-
	member(X,[course(math,220),course(math,226),course(cpsc,121)]),
	course(math,Y),
	course(math,Z),
	dif(Y,Z),
	number(Z),
	Y >= 300,
	Z >= 300.
requires(course(mech,421),[course(mech,366),course(mech,420)]).
requires(course(musc,320),[course(musc,319)]).
requires(course(musc,415),[course(musc,201),course(musc,210)]).
requires(course(phil,320),[course(phil,220)]).
requires(course(phil,321),[course(phil,220)]).
requires(course(phil,321),[course(phil,125)]).
requires(course(phil,322),[course(phil,220)]).
requires(course(phil,323),[course(phil,220)]).
requires(course(phil,326),[course(phil,220),course(A,B),course(C,D)]) :-
	member(A,[phil,ling]),
	member(C,[phil,ling]),
	course(A,B),
	course(C,D),
	dif(course(A,B),course(C,D)),
	B >= 200,
	D >= 200.
requires(course(phil,326),[course(ling,201),course(A,B),course(C,D)]) :-
	member(A,[phil,ling]),
	member(C,[phil,ling]),
	course(A,B),
	course(C,D),
	dif(course(A,B),course(C,D)),
	B >= 200,
	D >= 200.
requires(course(phil,441),[course(phil,240)]).
requires(course(phil,441),[course(cogs,200),course(phil,X)]) :-
	course(phil,X),
	X >= 200.
requires(course(phil,450),[course(phil,X),course(phil,Y),course(phil,Z)]) :-
	course(phil,X),
	course(phil,Y),
	course(phil,Z),
	dif(X,Y),
	dif(Y,Z),
	dif(X,Z),
	Y >= 200,
	Z >= 200,
	X >= 200.
requires(course(phil,451),[course(cogs,200),course(phil,X)]) :-
	course(phil,X),
	X >= 200.
requires(course(phil,451),[course(phil,240)]).
requires(course(phil,455),[course(cogs,200),course(phil,X)]) :-
	course(phil,X),
	X >= 200.
requires(course(phil,455),[course(phil,240)]).
requires(course(psyc,304),[course(psyc,100)]).
requires(course(psyc,304),[X,Y]) :-
	dif(X,Y),
	member(X,[course(psyc,101),course(psyc,102),course(psyc,205),course(psyc,207),course(psyc,208),course(psyc,216),course(psyc,217)]),
	member(Y,[course(psyc,101),course(psyc,102),course(psyc,205),course(psyc,207),course(psyc,208),course(psyc,216),course(psyc,217)]).
requires(course(psyc,309),[course(psyc,100)]).
requires(course(psyc,309),[course(psyc,101),course(psyc,102)]).
requires(course(psyc,309),[course(psyc,X),course(psyc,Y)]) :-
	dif(X,Y),
	course(psyc,X),
	course(psyc,Y),
	X >= 200,
	Y >= 200.
requires(course(psyc,321),[course(psyc,100)]).
requires(course(psyc,321),[X,Y]) :-
	dif(X,Y),
	member(X,[course(psyc,101),course(psyc,102),course(psyc,205),course(psyc,207),course(psyc,208),course(psyc,216),course(psyc,217)]),
	member(Y,[course(psyc,101),course(psyc,102),course(psyc,205),course(psyc,207),course(psyc,208),course(psyc,216),course(psyc,217)]).
requires(course(psyc,333),[course(psyc,100)]).
requires(course(psyc,333),[course(psyc,101),course(psyc,102)]).
requires(course(psyc,333),[course(psyc,X),course(psyc,Y)]) :-
	dif(X,Y),
	course(psyc,X),
	course(psyc,Y),
	X >= 200,
	Y >= 200.	
requires(course(psyc,336),[course(psyc,100)]).
requires(course(psyc,336),[course(psyc,101),course(psyc,102)]).
requires(course(psyc,336),[course(engl,329)]).	 
requires(course(psyc,336),[course(ling,420)]).
requires(course(psyc,336),[course(ling,200),course(ling,201)]).
requires(course(psyc,359),[course(psyc,366)]).
requires(course(psyc,359),[course(psyc,217),course(psyc,218)]).
requires(course(psyc,366),[course(psyc,260)]).	
requires(course(psyc,367),[course(psyc,100)]).
requires(course(psyc,367),[course(psyc,101),course(psyc,102)]).
requires(course(psyc,367),[course(psyc,X),course(psyc,Y)]) :-
	dif(X,Y),
	course(psyc,X),
	course(psyc,Y),
	X >= 200,
	Y >= 200.	
requires(course(psyc,368),[course(psyc,367)]).
requires(course(psyc,370),[X,Y,Z]) :-
	member(X,[course(psyc,260),course(psyc,270)]),
	member(Y,[course(psyc,217),course(psyc,277)]),
	member(Z,[course(psyc,218),course(psyc,278)]).
requires(course(psyc,371),[course(psyc,370)]).
requires(course(psyc,460),[X]) :-
	member(X,[course(psyc,304),course(psyc,360)]).
requires(course(psyc,461),[X]) :-
	member(X,[course(psyc,304),course(psyc,360),course(psyc,460)]).
requires(course(psyc,462),[X]) :-
	member(X,[course(psyc,304),course(psyc,360)]).
requires(course(stat,302),X) :- requires(course(math,302),X).
requires(course(stat,306),[X,course(math,302),Z]) :-
	member(X,[course(math,152),course(math,221),course(math,223)]),
	member(Z,[course(stat,200),course(stat,241),course(stat,251),course(stat,300),course(biol,300),course(comm,291),course(econ,325),course(econ,327),course(frst,231),course(psyc,218),course(psyc,278),course(psyc,366)]).
requires(course(stat,344),[X,course(math,302)]) :-
	member(X,[course(stat,200),course(stat,241),course(stat,251),course(stat,300),course(biol,300),course(comm,291),course(econ,325),course(econ,327),course(frst,231),course(psyc,218),course(psyc,278),course(psyc,366)]).
requires(course(stat,406),[course(math,306)]).
requires(course(stat,406),[course(cpsc,340)]).

% Equivalent courses: hasTaken(X) is true if an equivalent course has been taken	TODO: figure out how to get this to work both ways
isEquiv(course(math,302),course(stat,302)).
isEquiv(course(stat,302),course(math,302)).
isEquiv(course(asia,371),course(phil,371)).
isEquiv(course(phil,371),course(asia,371)).
isEquiv(course(asia,378),course(phil,378)).
isEquiv(course(phil,378),course(asia,378)).
isEquiv(course(asia,470),course(phil,470)).
isEquiv(course(phil,470),course(asia,470)).
isEquiv(course(cpsc, 110), course(cpsc, 107)):- hasTaken(course(cpsc, 103)). %new
isEquiv(course(cpsc, 110), course(cpsc, 103)):- hasTaken(course(cpsc, 107)). %new
isEquiv(course(math, 100), course(math, 102)). %new
isEquiv(course(math, 100), course(math, 104)). %new
isEquiv(course(math, 100), course(math, 120)). %new
isEquiv(course(math, 100), course(math, 180)). %new
isEquiv(course(math, 100), course(math, 184)). %new
isEquiv(course(math, 101), course(math, 103)). %new
isEquiv(course(math, 101), course(math, 105)). %new
isEquiv(course(math, 101), course(math, 121)). %new
isEquiv(course(phil, 220), course(phil, 320)). %new
isEquiv(course(stat, 200), course(stat, 241)). %new
isEquiv(course(stat, 200), course(stat, 251)). %new
isEquiv(course(phil, 326), course(phil, 351)). %new
isEquiv(course(phil, 326), course(phil, 441)). %new
isEquiv(course(phil, 326), course(phil, 451)). %new
isEquiv(course(phil, 326), course(phil, 351)). %new