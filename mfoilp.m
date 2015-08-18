%-----------------------------------------------------------------------------%

:- module mfoilp.
:- interface.


%-----------------------------------------------------------------------------%

:- import_module int.
:- import_module list.
:- import_module float.
:- import_module string.
:- import_module map.
:- import_module prob.


:- type atom_store.
:- type lterm ---> (float * atom).
:- type lexp == list(lterm).
:- type lb ---> finite(float) ; neginf.
:- type ub ---> finite(float) ; posinf.
:- type lincons ---> lincons(lb,lexp,ub).
:- type vartype ---> binary ; integer ; implint ; continuous.

:- type sol == map(atom,float).
:- type clause_info ---> clause_cut(sol,float,list(atom),list(atom))
	; clause_build(list(atom),list(atom)).


:- pred makevars(atom_store::out,
		 list(int)::out,
		 list(string)::out,
		 list(float)::out,
		 list(float)::out,
		 list(int)::out,
		 list(float)::out) is det.

:- pred makelincons(atom_store::in,
		    cons_store::out,
		    list(int)::out,
		    list(string)::out,
		    list(float)::out,
		    list(int)::out,
		    list(list(float))::out,
		    list(list(int))::out,
		    list(float)::out,
		    list(int)::out) is det.

:- pred cuts(atom_store::in,             % mapping
	    list(int)::in,               % solution
	    list(float)::in,             % solution
	    list(string)::out,           % cuts
	    list(float)::out,            % cuts
	    list(int)::out,              % cuts
	    list(list(float))::out,      % cuts
	    list(list(int))::out,        % cuts
	    list(float)::out,            % cuts
	    list(int)::out) is cc_multi. % cuts


:- pred consfail(atom_store::in,list(int)::in,list(float)::in) is semidet.

:- pred locks(atom_store::in,int::in,int::out,int::out) is cc_multi.

:- func solval(sol,atom) = float.

:- pred poslit(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred neglit(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred if_this(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred and_this(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred then_this(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred or_this(atom::in,clause_info::in,clause_info::out) is semidet.


%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module bool.
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type atom_store == bimap(int,prob.atom).
:- type cons_store == bimap(int,lincons).
:- type lterm_int ---> (float * int).
:- type lexp_int == list(lterm_int).
:- type lincons_int ---> lincons(float,lexp_int,float).


:- type locktype ---> neither ; down_only ; up_only ; both.
:- type lockinfo ---> lockinfo(lb,float,ub).

% :- type dualsol == map(lincons,float).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%
% Constants for SCIP variable types
%
%-----------------------------------------------------------------------------%

:- func scip_vartype(vartype) = int.
scip_vartype(binary) = 0.
scip_vartype(integer) = 1.
scip_vartype(implint) = 2.
scip_vartype(continuous) = 3.

%-----------------------------------------------------------------------------%
%
% Predicates for generating MIP variables
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", makevars(out,out,out,out,out,out,out), "MR_initial_variables").

makevars(AtomStore,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
	solutions(prob.initial_variable,AllAtoms),
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


%-----------------------------------------------------------------------------%
%
% Predicates for calculating variable locks
%
%-----------------------------------------------------------------------------%

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
	do_while(Call,filter,neither,Locks1),
	Call2 = (
		 pred(Out::out) is nondet :- prob.clause(clause_build([],[]),clause_build(NegLits,PosLits)),
		 (
		   (list.member(Atom,NegLits),Out=lockinfo(finite(1.0),-1.0,posinf))
		 ;
		   (list.member(Atom,PosLits),Out=lockinfo(finite(1.0),1.0,posinf))
		 )
	       ),
	do_while(Call2,filter,Locks1,Locks),
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

%-----------------------------------------------------------------------------%
%
% Predicate for pricing
%
%-----------------------------------------------------------------------------%

% :- pragma foreign_export("C", price(in,in,in,in,out,out,out,out,out,out), "MR_delayed_variables").

% price(AtomStore,ConsStore,Indices,Values,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
% 	map.init(DualSol0),
% 	makedualsol(Indices,Values,ConsStore,DualSol0,DualSol),
% 	prob.delayed_variable(Atom),
% 	reduced_cost(Atom,DualSol) < 0.

%-----------------------------------------------------------------------------%
%
% Predicates for generating initial constraints
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", makelincons(in,out,out,out,out,out,out,out,out,out), "MR_initial_constraints").

makelincons(AtomStore,ConsStore,Idents,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs) :-
	solutions(prob.initial_constraint,AllLinCons),
	bimap.init(CS0),
	makeconstore(AllLinCons,Idents,0,CS0,ConsStore),
	list.map7(lincons2scip(AtomStore),AllLinCons,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs).


:- pred makeconstore(list(lincons)::in,list(int)::out,int::in,cons_store::in,cons_store::out) is det.

makeconstore([],[],_,!CS).
makeconstore([H|T],[I|IT],I,!CS) :-
	bimap.det_insert(I,H,!CS),
	makeconstore(T,IT,I+1,!CS).

:- pred lincons2scip(atom_store::in,lincons::in,string::out,float::out,int::out,list(float)::out,list(int)::out,float::out,int::out) is det.

lincons2scip(AtomStore,LinCons,name(LinCons),LbF,FinLb,Coeffs,Vars,UbF,FinUb) :-
	LinCons = lincons(Lb,LinExpr,Ub),
	GetIndicesCoeffs = (pred(F*Atom::in,F::out,I::out) is det :- I = bimap.reverse_lookup(AtomStore,Atom)), 
	list.map2(GetIndicesCoeffs,LinExpr,Coeffs,Vars),
	(Lb=finite(LbFX) -> LbF=LbFX, FinLb=1; LbF=0.0, FinLb=0),
	(Ub=finite(UbFX) -> UbF=UbFX, FinUb=1; UbF=0.0, FinUb=0).

	
:- func name(T) = string.

name(X) = Name :-
	State0 = string.builder.init,
	stream.string_writer.write(string.builder.handle,X,State0,State),
	Name = string.builder.to_string(State).



%-----------------------------------------------------------------------------%
%
% Predicates for checking solutions
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", consfail(in,in,in), "MR_consFail").

consfail(AtomStore,Indices,Values) :-
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	consfail(Sol,_Cons).


%-----------------------------------------------------------------------------%
%
% Predicates for generating cuts
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", cuts(in,in,in,out,out,out,out,out,out,out), "MR_cuts").

cuts(AtomStore,Indices,Values,Names,LbFs,FinLbs,Coeffss,Varss,UbFs,FinUbs) :-
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	(
				% first try explicitly defined cutting plane algorithm
	  prob.cuts(Sol,Cuts0) ->
	  Cuts = Cuts0;
				% otherwise collect any clausal cuts
	  solutions(clausal_cut(Sol),Cuts0),
				% add in a single general cut (if any)
	  (
	    consfail(Sol,Cons) ->
	    Cuts = [Cons|Cuts0];
	    Cuts = Cuts0
	  )
	),
	list.map7(lincons2scip(AtomStore),Cuts,Names,LbFs,FinLbs,Coeffss,Varss,UbFs,FinUbs).

:- pred makesol(list(int)::in,list(float)::in,atom_store::in,sol::in,sol::out) is det.

makesol([],_Vals,_AtomStore,!Sol).
makesol([_H|_T],[],_AtomStore,!Sol).
makesol([H|T],[VH|VT],AtomStore,!Sol) :-
	bimap.lookup(AtomStore,H,Atom),
	map.det_insert(Atom,VH,!Sol),
	makesol(T,VT,AtomStore,!Sol).

:- pred consfail(sol::in,lincons::out) is nondet.

consfail(Sol,Cons) :-
	prob.delayed_constraint(Cons),
	Cons = lincons(Lb,LExp,Ub),
	activity(LExp,Sol,0.0,ConsVal),
	((Ub=finite(Ubf),ConsVal > Ubf) ; (Lb=finite(Lbf),ConsVal < Lbf)).

consfail(Sol,Cons) :-
	clausal_cut(Sol,Cons).

% evaluate the value of linear expression in constraint for given solution Sol
% only non-zero values recorded in Sol

:- pred activity(lexp::in,sol::in,float::in,float::out) is det.

activity([],_Sol,!ConsVal).
activity([Coeff * Atom|T],Sol,!ConsVal) :-
	activity(T,Sol,!.ConsVal+Coeff*solval(Sol,Atom),!:ConsVal).

solval(Sol,Atom) = Val :-
	(
	  map.search(Sol,Atom,Val0) ->
	  Val = Val0;
	  Val = 0.0
	).

%-----------------------------------------------------------------------------%
%
% Predicates for clauses, eg MLNs
%
%-----------------------------------------------------------------------------%

:- pred clausal_cut(sol::in,lincons::out) is nondet.

clausal_cut(Sol,Cut) :-
	StateIn = clause_cut(Sol,0.0,[],[]),
	prob.clause(StateIn,StateOut),
	StateOut = clause_cut(_Sol,_Val,NegLits,PosLits),
	clause2lincons(NegLits,PosLits,Cut).



poslit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn),
       clause_cut(Sol,ValOut,NegIn,[Atom|PosIn])) :-
	ValOut = ValIn+solval(Sol,Atom),
	ValOut < 1.0.

poslit(Atom,
       clause_build(NegIn,PosIn),
       clause_build(NegIn,[Atom|PosIn])).


neglit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn),
       clause_cut(Sol,ValOut,[Atom|NegIn],PosIn)) :-
	ValOut = ValIn+1.0-solval(Sol,Atom),
	ValOut < 1.0.


neglit(Atom,
       clause_build(NegIn,PosIn),
       clause_build([Atom|NegIn],PosIn)).


% syntactic sugar

if_this(Atom,!State) :- neglit(Atom,!State).
and_this(Atom,!State) :- neglit(Atom,!State).
then_this(Atom,!State) :- poslit(Atom,!State).
or_this(Atom,!State) :- poslit(Atom,!State).

:- pred clause2lincons(list(atom)::in,list(atom)::in,lincons::out) is det.

clause2lincons(NegLits,PosLits,lincons(finite(Lb),Terms,posinf)) :-
	c2l(NegLits,PosLits,Terms,Lb).

:- pred c2l(list(atom)::in,list(atom)::in,lexp::out,float::out) is det.
c2l([],[],[],1.0).
c2l([],[H|T],[1.0 * H | Rest],Lb) :-
   c2l([],T,Rest,Lb).
c2l([H|T],PosLits,[-1.0 * H | Rest],Lb-1.0) :-
	c2l(T,PosLits,Rest,Lb).

%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO).

% initialiser(!IO) :-
%     io.write_string("mfoilp: the initialiser has now been invoked.\n",
%         !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.
finaliser(!IO).
% finaliser(!IO) :-
%     io.write_string("mfoilp: the finaliser has now been invoked.\n",
%         !IO).

%-----------------------------------------------------------------------------%
:- end_module mfoilp.
%-----------------------------------------------------------------------------%
