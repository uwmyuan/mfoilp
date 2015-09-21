:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred clause(string::out,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::in,clause_lits::in,clause_lits::out) is multi.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred is_neglit(string::in,atom::in) is semidet.
:- pred is_poslit(string::in,atom::in) is semidet.

:- func objective(atom) = float.

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
	; cb1(int,person)
	; cb2(int,person,person).

:- pred person(person::out) is multi.

person(alice).
person(bob).
person(charlie).
person(dean).
person(ed).
person(fre).

% provide objective for each atom-variable

objective(Atom) = Cost :-
	(
	  Atom = cb1(1,_) ->
	  Cost = 2.0;
	  (
	    Atom = cb2(2,_,_) ->
	    Cost = 3.0;
	    (
	      Atom = smokes(alice) ->
	      Cost = 0.0;
	      Cost = 0.0
	    )
	  )
	).


initial_clause("data1") -->
	initial_poslit(smokes(bob)).

initial_clause("data2") -->
	initial_neglit(cancer(bob)).

initial_clause("data3") -->
	initial_neglit(smokes(ed)).

clause("fo1") -->
	{person(Y), bob @< Y},
	poslit(friends(bob,Y)).

clause("fo2") -->
	insol(smokes(X)),
	neglit(smokes(X)),
	poslit(cancer(X)),
	poslit(cb1(1,X)).

clause("fo3") -->
	insol(smokes(X)),
	neglit(smokes(X)),
	insol(friends(X,Y)),
	neglit(friends(X,Y)),
	{X @<  Y},
	poslit(smokes(Y)),
	poslit(cb2(2,X,Y)).

% essentially meta-information used to quickly compute locks for variables.

is_poslit(cancer(_)).
is_poslit(cb1(1,_)).
is_poslit(cb2(2,_,_)).
is_poslit(smokes(_)).
is_poslit(friends(bob,X)) :- not X = bob.

is_neglit(cancer(bob)).
is_neglit(smokes(_)).
is_neglit(friends(X,Y)) :- not X = Y.



	
