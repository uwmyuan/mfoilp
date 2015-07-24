:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.
:- import_module list.

:- type atom.

:- pred initial_variable(atom::out) is nondet.
:- pred initial_constraint(lincons::out) is nondet.
:- pred delayed_constraint(lincons::out) is nondet.

:- pred cuts(sol::in,list(lincons)::out) is semidet.
:- pred claual_cuts(sol::in,list(lincons)::out) is det.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%


:- import_module int.

% define atom type

:- type person ---> alice; bob; charlie; dean; ed; fre.

:- type atom --->
	friends(person,person)
	; smokes(person)
	; cancer(person)
	; cb1(int,person)
	; cb2(int,person,person).

% define atom-variable generator

atom(friends(X,Y)) :- person(X), person(Y), not X = Y.
atom(smokes(X)) :- person(X).
atom(cancer(X)) :- person(X).
atom(cb1(1,X)) :- person(X).
atom(cb1(2,X)) :- person(X).

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
	  Atom = cb(1,_) ->
	  Cost = 2;
	  (
	    Atom = cb(2,_) ->
	    Cost = 3;
	    Cost = 0
	  )
	).
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.



clausal_cuts(Sol,Cuts) :-
	solutions(clausal_cut(Sol),Cuts).

:- pred clause(state::in,state::out) is nondet.

clause -->
	{person(X)},
	neglit(smokes(X)),
	poslit(cancer(X)),
	poslit(cb(1,X)).

clause -->
	{person(X)},
	neglit(smokes(X)),
	{person(Y), not X = Y},
	neglit(friends(X,Y)),
	poslit(smokes(Y)),
	poslit(cb(2,X,Y)).


clause -->
	{person(X)},
	if(smokes(X)),
	{person(Y), not X = Y},
	and(friends(X,Y)),
	then(smokes(Y)),
	or(cb(2,X,Y)).


	