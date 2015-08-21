:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred initial_variable(atom::out) is nondet.
:- pred delayed_variable(atom::out) is nondet.
:- pred initial_constraint(lincons::out) is nondet.
:- pred delayed_constraint(lincons::out) is nondet.
%:- pred cuts(sol::in,list(lincons)::out) is nondet.

:- pred clause(clause_info::in,clause_info::out) is nondet.

:- pred is_neglit(atom::in) is semidet.
:- pred is_poslit(atom::in) is semidet.

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
%cuts(_,_) :- fail.

% no general initial constraints
initial_constraint(_) :- fail.

% no general delayed constraints
delayed_constraint(_) :- fail.

% no general delayed variables
% delayed_variable(_) :- fail.


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
initial_variable(smokes(X)) :- person(X), not X = alice, not X = bob.
initial_variable(cancer(X)) :- person(X).
initial_variable(cb1(1,X)) :- person(X).
initial_variable(cb2(2,X,Y)) :- person(X), person(Y), not X = Y.

delayed_variable(smokes(alice)).
delayed_variable(smokes(bob)).

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
	    (
	      Atom = smokes(alice) ->
	      Cost = -0.0;
	      Cost = 0.0
	    )
	  )
	).
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.

clause -->
	poslit(smokes(bob)).

clause -->
	neglit(cancer(bob)).


clause -->
	neglit(smokes(X)),
	poslit(cancer(X)),
	poslit(cb1(1,X)).

clause -->
	neglit(smokes(X)),
	neglit(friends(X,Y)),
	{not X = Y},
	poslit(smokes(Y)),
	poslit(cb2(2,X,Y)).

% essentially meta-information used to quickly compute locks for variables.

is_poslit(smokes(bob)).
is_poslit(cancer(_)).
is_poslit(cb1(1,_)).
is_poslit(cb2(2,_,_)).
is_poslit(smokes(_)).

is_neglit(cancer(bob)).
is_neglit(smokes(_)).
is_neglit(friends(X,Y)) :- not X = Y.

% clause -->
% 	{person(X)},
% 	if_this(smokes(X)),
% 	{person(Y), not X = Y},
% 	and_this(friends(X,Y)),
% 	then_this(smokes(Y)),
% 	or_this(cb(2,X,Y)).


	
