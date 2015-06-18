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
:- type lb ---> finite(float) ; neginf.
:- type ub ---> finite(float) ; posinf.
:- type lincons ---> lincons(lb,lexp,ub).
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
		    list(int)::out,
		    list(list(float))::out,
		    list(list(int))::out,
		    list(float)::out,
		    list(int)::out) is det.

:- pred conscheck(atom_store::in,list(int)::in,list(float)::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module map.
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type atom_store == bimap(int,prob.atom).
:- type lterm_int ---> (float * int).
:- type lexp_int == list(lterm_int).
:- type lincons_int ---> lincons(float,lexp_int,float).

:- type sol == map(atom,float).

%-----------------------------------------------------------------------------%

:- func scip_vartype(vartype) = int.
scip_vartype(binary) = 0.
scip_vartype(integer) = 1.
scip_vartype(implint) = 2.
scip_vartype(continuous) = 3.

:- pragma foreign_export("C", conscheck(in,in,in), "mconscheck").

conscheck(AtomStore,Indices,Values) :-
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	not consfail(Sol,_Cons).

:- pred makesol(list(int)::in,list(float)::in,atom_store::in,sol::in,sol::out) is det.

makesol([],_Vals,_AtomStore,Out,Out).
makesol([_H|_T],[],_AtomStore,Out,Out).
makesol([H|T],[VH|VT],AtomStore,In,Out) :-
	bimap.lookup(AtomStore,H,Atom),
	map.det_insert(Atom,VH,In,Mid),
	makesol(T,VT,AtomStore,Mid,Out).


:- pred consfail(sol::in,lincons::out) is nondet.

consfail(Sol,Cons) :-
	prob.lincons(Cons),
	Cons = lincons(Lb,LExp,Ub),
	value(LExp,Sol,0.0,ConsVal),
	((Ub=finite(Ubf),ConsVal > Ubf) ; (Lb=finite(Lbf),ConsVal < Lbf)).

:- pred value(lexp::in,sol::in,float::in,float::out) is det.

value([],_Sol,ConsVal,ConsVal).
value([Coeff * Atom|T],Sol,In,Out) :-
	(
	  map.search(Sol,Atom,Val) ->
	  value(T,Sol,In+Coeff*Val,Out);
	  value(T,Sol,In,Out)
	).


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


:- pragma foreign_export("C", makelincons(in,out,out,out,out,out,out,out), "makelincons").

makelincons(AtomStore,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs) :-
	solutions(prob.lincons,AllLinCons),
	Convert = (pred(lincons(Lb,LinExpr,Ub)::in,name(LinExpr)::out,LbF::out,FinLb::out,Coeffs::out,Vars::out,UbF::out,FinUb::out) is det :-
		  list.map2((pred(F*Atom::in,F::out,I::out) is det :- I = bimap.reverse_lookup(AtomStore,Atom)),LinExpr,Coeffs,Vars),
		      (Lb=finite(LbFX) -> LbF=LbFX, FinLb=1; LbF=0.0, FinLb=0),
		      (Ub=finite(UbFX) -> UbF=UbFX, FinUb=1; UbF=0.0, FinUb=0)),
	list.map7(Convert,AllLinCons,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs).

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
