:- module prob.
:- interface.

%----------------------------------------------------------------------%



:- import_module mercury_lib.
:- import_module float.

:- type atom.

:- pred atom(atom::out) is multi.
:- pred lincons(lincons::out) is nondet.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%

:- implementation.

:- import_module list.

% define atom type

:- type person ---> alice; bob; charlie; dean; ed.

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

% provide properties for each atom-variable

objective(_Atom) = 50.5.
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.

% constraints

lincons(lincons(0.0,[1.0 * friends(X,Y), 1.0 * friends(X,Z)],1.0))  :-
	atom(friends(X,Y)), atom(friends(X,Z)), not Y=Z. 




