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
:- type lexp_int.

:- pred makevars(atom_store::out,
		 list(int)::out,
		 list(string)::out,
		 list(float)::out,
		 list(float)::out,
		 list(int)::out,
		 list(float)::out) is det.

:- pred usevars(atom_store::in,io::di, io::uo) is det.

:- pred makelincons(atom_store::in,
		    list(float)::out,
		    list(list(float))::out,
		    list(list(int))::out,
		    list(float)::out) is det.
		    
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module solutions.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type person ---> alice; bob; charlie; dean; ed.

:- type atom --->
	friends(person,person)
	; smokes(person)
	; cancer(person)
	; cb1(int,person)
	; cb2(int,person,person).

:- type atom_store == bimap(int,atom).

:- type lterm ---> (float * atom).
:- type lterm_int ---> (float * int).
:- type lexp == list(lterm).
:- type lexp_int == list(lterm_int).
%:- type lpsol == map(atom,float).

:- type lincons ---> lincons(float,lexp,float).
:- type lincons_int ---> lincons(float,lexp_int,float).


%-----------------------------------------------------------------------------%

:- pred lincons(lincons::out) is multi.

lincons(lincons(-100.0,[-1.0 * cb1(1,X), 1.0 * smokes(X), -1.0 * cancer(X)],0.0))  :-
	person(X).


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
name(Atom) = Name :-
	State0 = string.builder.init,
	stream.string_writer.write(string.builder.handle,Atom,State0,State),
	Name = string.builder.to_string(State).

:- pragma foreign_export("C", makelincons(in,out,out,out,out), "makelincons").

makelincons(AtomStore,Lbs,Coeffss,Varss,Ubs) :-
	solutions(lincons,AllLinCons),
	Convert = (pred(lincons(Lb,LinExpr,Ub)::in,Lb::out,Coeffs::out,Vars::out,Ub::out) is det :-
		  list.map2((pred(F*Atom::in,F::out,I::out) is det :- I = bimap.reverse_lookup(AtomStore,Atom)),LinExpr,Coeffs,Vars)),
	list.map4(Convert,AllLinCons,Lbs,Coeffss,Varss,Ubs).

:- pragma foreign_export("C", makevars(out,out,out,out,out,out,out), "makevars").

makevars(AtomStore,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
	solutions(atom,AllAtoms),
	bimap.init(AS0),
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
	bimap.det_insert(I,H,!AS),
	store_atoms(T,IT,NT,LT,UT,VT,OT,I+1,!AS).


:- pragma foreign_export("C", usevars(in,di,uo), "usevars").

usevars(AtomStore,!IO) :-
	bimap.ordinates(AtomStore, Values),
	bimap.coordinates(AtomStore, Keys),
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
