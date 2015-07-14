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

:- pred locks(atom_store::in,int::in,int::out,int::out) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module map.
:- import_module bool.
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type atom_store == bimap(int,prob.atom).
:- type lterm_int ---> (float * int).
:- type lexp_int == list(lterm_int).
:- type lincons_int ---> lincons(float,lexp_int,float).

:- type sol == map(atom,float).
:- type locktype ---> neither ; down_only ; up_only ; both.
:- type lockinfo ---> lockinfo(lb,float,ub).

%-----------------------------------------------------------------------------%

:- func scip_vartype(vartype) = int.
scip_vartype(binary) = 0.
scip_vartype(integer) = 1.
scip_vartype(implint) = 2.
scip_vartype(continuous) = 3.

:- pragma foreign_export("C", locks(in,in,out,out), "MR_consLock").

% Used by consLockFolinear
% only need to add locks due to delayed constraints
% since initial constraints generate SCIP linear constraints
% which get their var locks from SCIP
locks(AtomStore,Index,Up,Down) :-
	bimap.lookup(AtomStore,Index,Atom),
	Call = (
		 pred(Out::out) is nondet :- prob.delayed_constraint(Cons),
		 Cons = lincons(Lb,LExp,Ub),
		 list.member(F*Atom,LExp),
		 Out = lockinfo(Lb,F,Ub)
	       ),
	do_while(Call,filter,neither,Locks),
	locknum(Locks,Up,Down).

:- pred locknum(locktype::in,int::out,int::out) is det.
locknum(neither,0,0).
locknum(down_only,0,1).
locknum(up_only,1,0).
locknum(both,1,1).


:- pred filter(lockinfo::in,bool::out,locktype::in,locktype::out) is det.

filter(_Lockinfo,no,both,both).
filter(Lockinfo,More,neither,Out) :-
	(
	  up_lock(Lockinfo) ->
	  (
	    down_lock(Lockinfo) ->
	    More = no,
	    Out = both;
	    More = yes,
	    Out = up_only
	  )
	;
	  More = yes,
	  (
	    down_lock(Lockinfo) ->
	    Out = down_only;
	    Out = neither
	  )
	).
filter(Lockinfo,More,down_only,Out) :-
	(
	  up_lock(Lockinfo) ->
	  More = no,
	  Out = both;
	  More = yes,
	  Out = down_only
	).
filter(Lockinfo,More,up_only,Out) :-
	(
	  down_lock(Lockinfo) ->
	  More = no,
	  Out = both;
	  More = yes,
	  Out = up_only
	).
	  
:- pred up_lock(lockinfo::in) is semidet.

up_lock(lockinfo(Lb,F,Ub)) :-
	(
	  F > 0.0 ->
	  not Ub = posinf;
	  not Lb = neginf
	).

:- pred down_lock(lockinfo::in) is semidet.

down_lock(lockinfo(Lb,F,Ub)) :-
	(
	  F > 0.0 ->
	  not Lb = neginf;
	  not Ub = posinf
	).

:- pragma foreign_export("C", conscheck(in,in,in), "MR_consCheck").

conscheck(AtomStore,Indices,Values) :-
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	not consfail(Sol,_Cons).

:- pred makesol(list(int)::in,list(float)::in,atom_store::in,sol::in,sol::out) is det.

makesol([],_Vals,_AtomStore,!Sol).
makesol([_H|_T],[],_AtomStore,!Sol).
makesol([H|T],[VH|VT],AtomStore,!Sol) :-
	bimap.lookup(AtomStore,H,Atom),
	map.det_insert(Atom,VH,!Sol),
	makesol(T,VT,AtomStore,!Sol).


:- pred consfail(sol::in,lincons::out) is nondet.

consfail(Sol,Cons) :-
	prob.delayed_constraint(_,Cons),
	Cons = lincons(Lb,LExp,Ub),
	activity(LExp,Sol,0.0,ConsVal),
	((Ub=finite(Ubf),ConsVal > Ubf) ; (Lb=finite(Lbf),ConsVal < Lbf)).

% evaluate the value of linear expression in constraint for given solution Sol
% only non-zero values recorded in Sol

:- pred activity(lexp::in,sol::in,float::in,float::out) is det.

activity([],_Sol,!ConsVal).
activity([Coeff * Atom|T],Sol,!ConsVal) :-
	(
	  map.search(Sol,Atom,Val) ->
	  activity(T,Sol,!.ConsVal+Coeff*Val,!:ConsVal);
	  % Atom not found, so Val is zero
	  activity(T,Sol,!ConsVal)
	).


:- pragma foreign_export("C", makevars(out,out,out,out,out,out,out), "MR_initial_variables").

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


:- pragma foreign_export("C", makelincons(in,out,out,out,out,out,out,out), "MR_initial_constraints").

makelincons(AtomStore,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs) :-
	solutions(prob.initial_constraint,AllLinCons),
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
