%-----------------------------------------------------------------------------%

:- module mfoilp.
:- interface.

%-----------------------------------------------------------------------------%


:- import_module list.
:- import_module float.
:- import_module prob.

:- type vartype ---> binary ; integer ; implint ; continuous.

% for problem instance to define (initial) constraints

% linear term is either a float*atom or a call that returns float*atom terms
:- type lterm ---> (float * atom).
:- type lexp == list(lterm).
:- type sumterm == pred(lterm).
:- type sumexp == list(sumterm).
:- type lb ---> finite(float) ; neginf.
:- type ub ---> finite(float) ; posinf.
% not ready yet :- type lincons ---> lincons(lb,lexp,sumexp,ub).
:- type lincons ---> lincons(lb,lexp,ub).

:- type sol.

% for problem instance to define clauses
:- type clause_info.

:- pred insol(atom::out,clause_info::in,clause_info::out) is nondet.
:- pred poslit(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred neglit(atom::in,clause_info::in,clause_info::out) is semidet.
%:- pred if_this(atom::out,clause_info::in,clause_info::out) is nondet.
%:- pred and_this(atom::out,clause_info::in,clause_info::out) is nondet.
%:- pred then_this(atom::in,clause_info::in,clause_info::out) is semidet.
%:- pred or_this(atom::in,clause_info::in,clause_info::out) is semidet.

%:- pred gen_neglit(atom::in,gen_clause_info::in,gen_clause_info::out) is semidet.
%:- pred setrhs(float::in,gen_clause_info::in,gen_clause_info::out) is det.


%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.
:- import_module map.
:- import_module bimap.
:- import_module bool.
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.

:- type atom_store == bimap(int,prob.atom).
% 2nd argument is the index of the next variable to be created
:- type as_next --> as(atom_store,int).
:- type cons_store == bimap(int,lincons).
:- type lterm_int ---> (float * int).
:- type lexp_int == list(lterm_int).
:- type lincons_int ---> lincons(float,lexp_int,float).

:- type lockinfo ---> lockinfo(lb,float,ub).
:- type locktype ---> neither ; down_only ; up_only ; both.

:- type dualsol == map(lincons,float).

:- type sol == map(atom,float).
:- type clause_info ---> clause_cut(
				    sol,          % solution for which a cut is sought (does not change)
				    float,        % activity of the ground clause constructed so far
				    list(atom),   % negative literals in clause so far
				    list(atom)    % positive literals in clause so far
				   ).



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

:- pred makevars(as_next::out,
		 list(int)::out,
		 list(string)::out,
		 list(float)::out,
		 list(float)::out,
		 list(int)::out,
		 list(float)::out) is det.


makevars(AtomStore,Idents,Names,Lbs,Ubs,VarTypes,Objs) :-
	solutions(prob.initial_variable,AllAtoms),
	bimap.init(AS0),
	store_atoms(AllAtoms,Idents,Names,Lbs,Ubs,VarTypes,Objs,0,Next,AS0,AS),
	AtomStore = as_next(AS,Next).

:- pred store_atoms(list(prob.atom)::in,
		    list(int)::out,
		    list(string)::out,
		    list(float)::out,
		    list(float)::out,
		    list(int)::out,
		    list(float)::out,
		    int::in,int::out,atom_store::in,atom_store::out) is det.

store_atoms([],[],[],[],[],[],[],!I,!AS).
store_atoms([H|T],[I|IT],[name(H)|NT],[prob.lb(H)|LT],[prob.ub(H)|UT],
	    [scip_vartype(vartype(H))|VT],[prob.objective(H)|OT],I,J,!AS) :-
	bimap.det_insert(I,H,!AS),
	store_atoms(T,IT,NT,LT,UT,VT,OT,I+1,J,!AS).


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

:- pred locks(as_next::in,int::in,int::out,int::out) is cc_multi.

locks(as(AtomStore,_),Index,Up,Down) :-
	bimap.lookup(AtomStore,Index,Atom),
	Locks1 = neither,
	% add locks directly declared
	(
	  is_poslit(Atom) ->
	  filter(lockinfo(finite(1.0),1.0,posinf),_,Locks1,Locks2);
	  Locks2 = Locks1
	),
	(
	  is_neglit(Atom) ->
	  filter(lockinfo(finite(1.0),-1.0,posinf),_,Locks2,Locks3);
	  Locks3 = Locks2
	),
	locknum(Locks3,Up,Down).

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
% Predicates for generating initial constraints
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", makelincons(in,out,out,out,out,out,out,out,out,out), "MR_initial_constraints").

% generates SCIP-understandable versions of initial constraints

:- pred makelincons(as_next::in,                % atom store
		    cons_store::out,
		    list(int)::out,
		    list(string)::out,
		    list(float)::out,
		    list(int)::out,
		    list(list(float))::out,
		    list(list(int))::out,
		    list(float)::out,
		    list(int)::out) is det.


makelincons(AtomStoreNext,ConsStore,Idents,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs) :-
	solutions(prob.initial_constraint,AllLinCons),
	bimap.init(CS0),
	makeconstore(AllLinCons,AllLinConsOut,0,Idents,CS0,ConsStore),
	list.map7(lincons2scip(AtomStoreNext),AllLinConsOut,Names,Lbs,FinLbs,Coeffss,Varss,Ubs,FinUbs).


% updates a constraint store and records new constraints and their indices
% should really be called update_constore or something like that.

:- pred makeconstore(
		     list(lincons)::in,    % list of constraints to store
		     list(lincons)::out,   % list of new constraints (in input but not already in constraint store )
		     int::in,              % index to use to store the next constraint
		     list(int)::out,       % list of indices of new constaints
		     cons_store::in,       % initial constraint store
		     cons_store::out) is det.  % updated constraint store

makeconstore([],[],_,[],!CS).
makeconstore([H|T],Out,I,IOut,!CS) :-
	(
	  bimap.insert(I,H,!CS) ->
	  Out = [H|T2],
	  IOut = [I|IT],
	  makeconstore(T,T2,I+1,IT,!CS);
	  makeconstore(T,Out,I,IOut,!CS)
	).

% converts a lincons term into something SCIP can understand using the atom_store
% if the lincons term contains an atom not in the atom_store then it is added to atom_store
% and a list of added atom indices, if any, is returned.

:- pred lincons2scip(as_next::in,as_next::out,lincons::in,string::out,float::out,int::out,list(float)::out,list(int)::out,float::out,int::out) is det.

lincons2scip(ASN0,ASN,LinCons,name(LinCons),LbF,FinLb,Coeffs,Vars,UbF,FinUb) :-
	LinCons = lincons(Lb,LinExpr,Ub),
	% unfortunately no "filter_map2" in the Mercury list library
	my_filter_map2(LinExpr,ASN0,ASN,Coeffs,Vars),
	(Lb=finite(LbFX) -> LbF=LbFX, FinLb=1; LbF=0.0, FinLb=0),  % in right disjunct, LbF is a dummy value
	(Ub=finite(UbFX) -> UbF=UbFX, FinUb=1; UbF=0.0, FinUb=0).  % in right disjunct, UbF is a dummy value

% yanks our variable indices for the atoms in a constraint
% creates new variable indices if necessary

:- pred my_filter_map2(lexp::in,as_next::in,as_next::out,list(float)::out,list(int)::out) is det.

my_filter_map2([],!ASN,[],[]).
my_filter_map2([F * Atom|T],ASNIn,ASNOut,Coeffs,Vars) :-
	Coeffs = [F|CoeffsT],
	(
	  bimap.reverse_search(AtomStore,I,Atom) ->
	  Vars = [I|VarsT],
	  ASNMid = ASNIn;
	  % create a new variable in atom store (SCIP will add later)
	  ASNIn = as(ASIn,Next),
	  Vars = [Next|VarsT],
	  bimap.det_insert(Next,Atom,ASIn,ASOut),
	  ASNMid = as(ASOut,Next+1),
	),
	my_filter_map2(T,ASNMid,ASNOut,CoeffsT,VarsT).
	  

	
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

:- pred consfail(as_next::in,list(int)::in,list(float)::in) is semidet.

consfail(as(AtomStore,_Next),Indices,Values) :-
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	consfail(Sol,_Cons).


%-----------------------------------------------------------------------------%
%
% Predicates for generating cuts
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", cuts(in,in,in,in,in,out,out,out,out,out,out,out,out,out), "MR_cuts").

:- pred cuts(as_next::in,             % mapping
	     list(int)::in,		 % solution
	     list(float)::in,		 % solution
	     cons_store::in,
	     int::in,
	     cons_store::out,
	     list(int)::out,
	     list(string)::out,		 % cuts
	     list(float)::out,		 % cuts
	     list(int)::out,		 % cuts
	     list(list(float))::out,	 % cuts
	     list(list(int))::out,	 % cuts
	     list(float)::out,		 % cuts
	     list(int)::out) is cc_multi. % cuts


cuts(ASN,Indices,Values,RowStoreIn,NRowsIn,RowStoreOut,NewRowIdents,Names,LbFs,FinLbs,Coeffss,Varss,UbFs,FinUbs) :-
	ASN = as(AtomStore,_),
	map.init(Sol0),
	makesol(Indices,Values,AtomStore,Sol0,Sol),
	solutions(clausal_cut(Sol),Cuts),
	makeconstore(Cuts,CutsOut,NRowsIn,NewRowIdents,RowStoreIn,RowStoreOut),  % update row store
	list.map7(lincons2scip(ASN),CutsOut,Names,LbFs,FinLbs,Coeffss,Varss,UbFs,FinUbs).

:- pred makesol(list(int)::in,list(float)::in,atom_store::in,sol::in,sol::out) is det.

makesol([],_Vals,_AtomStore,!Sol).
makesol([_H|_T],[],_AtomStore,!Sol).
makesol([H|T],[VH|VT],AtomStore,!Sol) :-
	bimap.lookup(AtomStore,H,Atom),
	map.det_insert(Atom,VH,!Sol),
	makesol(T,VT,AtomStore,!Sol).



:- pred consfail(sol::in,lincons::out) is nondet.

consfail(Sol,Cons) :-
	prob.delayed_constraint(Sol,Cons),
	Cons = lincons(Lb,LExp,Ub),
	activity(LExp,Sol,0.0,ConsVal),
	((Ub=finite(Ubf),ConsVal > Ubf) ; (Lb=finite(Lbf),ConsVal < Lbf)).

% useful for yanking out atoms with non-zero values in a solution

:- pred insol(sol::in,list(atom)::out) is det.
insol(Sol,map.keys(Sol)).

consfail(Sol,Cons) :-
	clausal_cut(Sol,Cons).

% evaluate the value of linear expression in constraint for given solution Sol
% only non-zero values recorded in Sol

:- pred activity(lexp::in,sol::in,float::in,float::out) is det.

activity([],_Sol,!ConsVal).
activity([Coeff * Atom|T],Sol,!ConsVal) :-
	activity(T,Sol,!.ConsVal+Coeff*solval(Sol,Atom),!:ConsVal).

:- func solval(sol,atom) = float.
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
	StateIn = clause_cut(Sol,0.0,[],[],1.0),
	prob.clause(StateIn,StateOut),
	StateOut = clause_cut(_Sol,_Val,NegLits,PosLits,RHS),
	clause2lincons(NegLits,PosLits,Cut,RHS).

poslit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
       clause_cut(Sol,ValOut,NegIn,[Atom|PosIn],RHS)) :-
	ValOut = ValIn+solval(Sol,Atom),
	ValOut < 1.0.

% coeff_poslit(Atom,Coeff,
%        clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
%        clause_cut(Sol,ValOut,NegIn,[Atom|PosIn]),RHS) :-
% 	ValOut = ValIn+Coeff*solval(Sol,Atom),
% 	ValOut < 1.0.


%setrhs(RHS,
%       clause_cut(Sol,ValIn,NegIn,PosIn,_),
%       clause_cut(Sol,ValIn-RHS+1.0,NegIn,PosIn,RHS)
%      ).


insol(Atom,In,In) :-
	In  = clause_cut(Sol,_,_,_,_),
	map.member(Sol,Atom,_).

% normal negative literals are a special case
% a neglit can only succeed (to generate a ground atom
% for a cutting plane) if it is in the solution.
neglit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
       clause_cut(Sol,ValOut,[Atom|NegIn],PosIn,RHS)) :-
	% if this next goal fails then the value for Atom is 0,
	% and so ValOut would exceed 1,
	% this is why we call map.search rather than solval: to fail earlier
	map.search(Sol,Atom,Val),
	ValOut = ValIn+1.0-Val,
	ValOut < 1.0.

% coeff_neglit(Atom,Coeff,
%        clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
%        clause_cut(Sol,ValOut,[Atom|NegIn],PosIn,RHS)) :-
% 	map.member(Sol,Atom,Val),
% 	ValOut = ValIn+Coeff*(1.0-Val),
% 	ValOut < 1.0.

%gen_neglit(Atom,
%       clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
%       clause_cut(Sol,ValOut,[Atom|NegIn],PosIn,RHS)) :-
%	ValOut = ValIn+1.0-solval(Sol,Atom),
%	ValOut < 1.0.

% coeff_gen_neglit(Atom,Coeff,
%        clause_cut(Sol,ValIn,NegIn,PosIn,RHS),
%        clause_cut(Sol,ValOut,[Atom|NegIn],PosIn,RHS)) :-
% 	ValOut = ValIn+Coeff*(1.0-solval(Sol,Atom)),
% 	ValOut < 1.0.






% the following three predicate have not been updated to have the "RHS" argument

% setpack_cut(Sol,Cut) :-
% 	StateIn = clause_cut(Sol,0.0,[],[]),
% 	prob.setpack(StateIn,StateOut),
% 	StateOut = clause_cut(_Sol,_Val,NegLits,PosLits),
% 	clause2lincons(NegLits,PosLits,Cut).
	

% posunlit(Atom,
%        clause_cut(Sol,ValIn,NegIn,PosIn),
%        clause_cut(Sol,ValOut,NegIn,[Atom|PosIn])) :-
% 	(
% 	  ValIn > 1.0
% 	;
% 	  map.member(Sol,Atom,Val),
% 	  ValOut = ValIn+solval(Sol,Atom),
	  

% negunlit(Atom,
%        clause_cut(Sol,ValIn,NegIn,PosIn),
%        clause_cut(Sol,ValOut,[Atom|NegIn],PosIn)) :-
% 	map.member(Sol,Atom,Val),
% 	ValOut = ValIn+1.0-Val,
% 	ValOut < 1.0.

% syntactic sugar

%if_this(Atom,!State) :- neglit(Atom,!State).
%and_this(Atom,!State) :- neglit(Atom,!State).
%then_this(Atom,!State) :- poslit(Atom,!State).
%or_this(Atom,!State) :- poslit(Atom,!State).

:- pred clause2lincons(list(atom)::in,list(atom)::in,lincons::out,float::in) is det.

clause2lincons(NegLits,PosLits,lincons(finite(Lb),Terms,posinf),InLb) :-
	c2l(NegLits,PosLits,Terms,Lb,InLb).

:- pred c2l(list(atom)::in,list(atom)::in,lexp::out,float::out,float::in) is det.
c2l([],[],[],Lb,Lb).
c2l([],[H|T],[1.0 * H | Rest],Lb, InLb) :-
   c2l([],T,Rest,Lb,InLb).
c2l([H|T],PosLits,[-1.0 * H | Rest],Lb-1.0,InLb) :-
	c2l(T,PosLits,Rest,Lb,InLb).

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
