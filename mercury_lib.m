%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module mercury_lib.
:- interface.


%-----------------------------------------------------------------------------%

:- import_module io.
:- import_module int.
:- import_module list.
:- import_module float.
:- import_module string.

:- type atom_store.

:- pred makevars(atom_store::out,
		 list(int)::out,
		 list(string)::out,
		 list(float)::out,
		 list(float)::out,
		 list(int)::out,
		 list(float)::out) is det.
:- pred usevars(atom_store::in,io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module solutions.

:- type person ---> alice; bob; charlie; dean; ed.

:- type atom --->
	friends(person,person)
	; smokes(person)
	; cancer(person)
	; cb1(int,person)
	; cb2(int,person,person).

:- type atom_store == map(int,atom).


%-----------------------------------------------------------------------------%


:- pred atom(atom::out) is multi.

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

:- func objective(atom) = float.
objective(_) = 0.5.

:- func lb(atom) = float.
lb(_) = 0.0.

:- func ub(atom) = float.
ub(_) = 1.0.

:- func vartype(atom) = int.
vartype(_) = 1.

:- func name(atom) = string.
name(_) = "todo".
%name(Atom) = Name :- Name = "", io.write(Atom,Name,!IO).

:- pragma foreign_export("C", makevars(out,out,out,out,out,out,out), "makevars").

makevars(AtomStore,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
	solutions(atom,AllAtoms),
	map.init(AS0),
	store_atoms(AllAtoms,Idents,Names,Lbs,Ubs,VarTypes,Objs,0,AS0,AtomStore).

:- pred store_atoms(list(atom)::in,
		    list(int)::out,
		    list(string)::out,
		    list(float)::out,
		    list(float)::out,
		    list(int)::out,
		    list(float)::out,
		    int::in,atom_store::in,atom_store::out) is det.

store_atoms([],[],[],[],[],[],[],_,!AS).
store_atoms([H|T],[I|IT],[name(H)|NT],[lb(H)|LT],[ub(H)|UT],
	    [vartype(H)|VT],[objective(H)|OT],I,!AS) :-
	map.det_insert(I,H,!AS),
	store_atoms(T,IT,NT,LT,UT,VT,OT,I+1,!AS).


:- pragma foreign_export("C", usevars(in,di,uo), "usevars").

usevars(AtomStore,!IO) :-
	map.keys_and_values(AtomStore, Keys, Values),
	write_all(Keys,!IO),
	write_all(Values,!IO).

:- pred write_all(list(T)::in,io::di,io::uo) is det.

write_all([],!IO).
write_all([H|T],!IO) :-
	write(H,!IO),nl(!IO),
	write_all(T,!IO).
	


%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO) :-
    io.write_string("mercury_lib: the initialiser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.

finaliser(!IO) :-
    io.write_string("mercury_lib: the finaliser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
