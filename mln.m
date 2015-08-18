:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.
:- import_module list.

:- type atom.

:- pred initial_variable(atom::out) is nondet.
:- pred initial_constraint(lincons::out) is nondet.
:- pred delayed_constraint(lincons::out) is nondet.
:- pred cuts(sol::in,list(lincons)::out) is nondet.

:- pred clause(clause_cut::in,clause_cut::out) is nondet.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%

:- implementation.

:- import_module int.

%----------------------------------------------------------------------%

% meta information, so to speak

% no specialised cutting plane algorithm
cuts(_,_) :- fail.

% no general initial constraints
initial_constraint(_) :- fail.

% no general delayed constraints
delayed_constraint(_) :- fail.

%----------------------------------------------------------------------%

% define atom type

:- type person ---> alice; bob; charlie; dean; ed; fre.

:- type atom --->
	friends(person,person)
	; smokes(person)
	; cancer(person)
	; cb1(int,person)
	; cb2(int,person,person).

% define atom-variable generator

initial_variable(friends(X,Y)) :- person(X), person(Y), not X = Y.
initial_variable(smokes(X)) :- person(X).
initial_variable(cancer(X)) :- person(X).
initial_variable(cb1(1,X)) :- person(X).
initial_variable(cb2(2,X,Y)) :- person(X), person(Y), not X = Y.

:- pred person(person::out) is multi.

person(alice).
person(bob).
person(charlie).
person(dean).
person(ed).
person(fre).

% provide properties for each atom-variable

objective(Atom) = Cost :-
	(
	  Atom = cb1(1,_) ->
	  Cost = 2.0;
	  (
	    Atom = cb2(2,_,_) ->
	    Cost = 3.0;
	    Cost = 0.0
	  )
	).
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.



clause -->
	{person(X)},
	neglit(smokes(X)),
	poslit(cancer(X)),
	poslit(cb1(1,X)).

clause -->
	{person(X)},
	neglit(smokes(X)),
	{person(Y), not X = Y},
	neglit(friends(X,Y)),
	poslit(smokes(Y)),
	poslit(cb2(2,X,Y)).


% clause -->
% 	{person(X)},
% 	if_this(smokes(X)),
% 	{person(Y), not X = Y},
% 	and_this(friends(X,Y)),
% 	then_this(smokes(Y)),
% 	or_this(cb(2,X,Y)).


	