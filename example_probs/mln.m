:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred clause(string::out) is multi.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module int.

%----------------------------------------------------------------------%

% define atom type

:- type person ---> alice; bob; charlie; dean; ed; fre.

:- type atom --->
	friends(person,person)
	; smokes(person)
	; cancer(person)
	; cb1(person)
	; cb2(person,person).

:- pred person(person::out) is multi.

person(alice).
person(bob).
person(charlie).
person(dean).
person(ed).
person(fre).

% provide non-zero objective values for each atom-variable

objective(cb1(_),2.0).
objective(cb2(_,_),3.0).


initial_clause("data1") -->
	initial_poslit(smokes(bob)).

initial_clause("data2") -->
	initial_neglit(cancer(bob)).

initial_clause("data3") -->
	initial_neglit(smokes(ed)).


clause("fo1").
clause("fo1") -->
 	{person(Y), bob @< Y},
 	poslit(friends(bob,Y)).
poslit("fo1",friends(bob,Y)) :- person(Y), bob @< Y.

clause("fo2").
clause("fo2") -->
	insol(smokes(X)),
 	neglit(smokes(X)),
 	poslit(cancer(X)),
 	poslit(cb1(X)).
 neglit("fo2",smokes(_)).
 poslit("fo2",cancer(_)).
 poslit("fo2",cb1(_)).

clause("fo3").
clause("fo3") -->
	insol(smokes(X)),
 	neglit(smokes(X)),
 	insol(friends(X,Y)),
 	neglit(friends(X,Y)),
 	{X @<  Y},
 	poslit(smokes(Y)),
 	poslit(cb2(X,Y)).
neglit("fo3",smokes(_)).
neglit("fo3",friends(X,Y)) :- X @< Y.
neglit("fo3",smokes(_)). 
poslit("fo3",cb2(_,_)).

