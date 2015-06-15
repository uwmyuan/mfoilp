%-----------------------------------------------------------------------------%

:- module mercury_lib.
:- interface.


%-----------------------------------------------------------------------------%

:- import_module int.
:- import_module list.
:- import_module float.
:- import_module string.
:- import_module prob.


:- type atom_store.
:- type lterm ---> (float * atom).
:- type lexp == list(lterm).
:- type lincons ---> lincons(float,lexp,float).
:- type vartype ---> binary ; integer ; implint ; continuous.

:- pred makevars(atom_store::out,
		 list(int)::out,
		 list(string)::out,
		 list(float)::out,
		 list(float)::out,
		 list(int)::out,
		 list(float)::out) is det.

:- pred makelincons(atom_store::in,
		    list(string)::out,
		    list(float)::out,
		    list(list(float))::out,
		    list(list(int))::out,
		    list(float)::out) is det.
		    
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type atom_store == bimap(int,prob.atom).
:- type lterm_int ---> (float * int).
:- type lexp_int == list(lterm_int).
:- type lincons_int ---> lincons(float,lexp_int,float).

%-----------------------------------------------------------------------------%

:- func scip_vartype(vartype) = int.
scip_vartype(binary) = 0.
scip_vartype(integer) = 1.
scip_vartype(implint) = 2.
scip_vartype(continuous) = 3.

:- pragma foreign_export("C", makevars(out,out,out,out,out,out,out), "makevars").

makevars(AtomStore,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
	solutions(prob.atom,AllAtoms),
	bimap.init(AS0),
	store_atoms(AllAtoms,Idents,Names,Lbs,Ubs,VarTypes,Objs,0,AS0,AtomStore).

:- pred store_atoms(list(prob.atom)::in,
		    list(int)::out,
		    list(string)::out,
		    list(float)::out,
		    list(float)::out,
		    list(int)::out,
		    list(float)::out,
		    int::in,atom_store::in,atom_store::out) is det.

store_atoms([],[],[],[],[],[],[],_,!AS).
store_atoms([H|T],[I|IT],[name(H)|NT],[prob.lb(H)|LT],[prob.ub(H)|UT],
	    [scip_vartype(vartype(H))|VT],[prob.objective(H)|OT],I,!AS) :-
	bimap.det_insert(I,H,!AS),
	store_atoms(T,IT,NT,LT,UT,VT,OT,I+1,!AS).


:- pragma foreign_export("C", makelincons(in,out,out,out,out,out), "makelincons").

makelincons(AtomStore,Names,Lbs,Coeffss,Varss,Ubs) :-
	solutions(prob.lincons,AllLinCons),
	Convert = (pred(lincons(Lb,LinExpr,Ub)::in,name(LinExpr)::out,Lb::out,Coeffs::out,Vars::out,Ub::out) is det :-
		  list.map2((pred(F*Atom::in,F::out,I::out) is det :- I = bimap.reverse_lookup(AtomStore,Atom)),LinExpr,Coeffs,Vars)),
	list.map5(Convert,AllLinCons,Names,Lbs,Coeffss,Varss,Ubs).

:- func name(T) = string.

name(X) = Name :-
	State0 = string.builder.init,
	stream.string_writer.write(string.builder.handle,X,State0,State),
	Name = string.builder.to_string(State).

%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO).

% initialiser(!IO) :-
%     io.write_string("mercury_lib: the initialiser has now been invoked.\n",
%         !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.
finaliser(!IO).
% finaliser(!IO) :-
%     io.write_string("mercury_lib: the finaliser has now been invoked.\n",
%         !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
