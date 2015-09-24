%-----------------------------------------------------------------------------%
:- module mfoilp.
:- interface.

%-----------------------------------------------------------------------------%


:- import_module prob.

% for problem instance to define clauses

:- type clause_info.
:- type clause_lits.

:- pred insol(atom::out,clause_info::in,clause_info::out) is nondet.
:- pred poslit(atom::in,clause_info::in,clause_info::out) is semidet.
:- pred neglit(atom::in,clause_info::in,clause_info::out) is semidet.

:- pred initial_poslit(atom::in,clause_lits::in,clause_lits::out) is det.
:- pred initial_neglit(atom::in,clause_lits::in,clause_lits::out) is det.

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
:- import_module list.
:- import_module float.


:- type atom_store == bimap(int,prob.atom).
% 2nd argument is the index of the next variable to be created
:- type as_next ---> as(atom_store,int).

:- type sol == map(atom,float).
:- type clause_info ---> clause_cut(
				    sol,          % solution for which a cut is sought (does not change)
				    float,        % activity of the ground clause constructed so far
				    list(atom),   % negative literals in clause so far
				    list(atom)    % positive literals in clause so far
				   ).

:- type clause_lits ---> lits(list(atom),list(atom)).
:- type named_clause_lits ---> named(string,clause_lits).


%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%
% Predicates for generating MIP variables
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", foclausenames(out), "MR_delayed_clauses").

:- pred foclausenames(list(string)::out) is det.

foclausenames(Names) :-
	solutions(prob.clause,Names).


%-----------------------------------------------------------------------------%
%
% Predicates for calculating variable locks
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", varsinfolinear(in,in,in,out,out,out,out), "MR_varsinfolinear").

:- pred varsinfolinear(string::in,int::in,as_next::in,list(int)::out,int::out,list(int)::out,list(int)::out) is det.

varsinfolinear(Name,N,as(AS,_),Vars,M,Down,Up) :-
	varsinfolinear(Name,0,N,AS,Vars,Down,Up,0,M).

:- pred varsinfolinear(string::in,int::in,int::in,atom_store::in,list(int)::out,list(int)::out,list(int)::out,int::in,int::out) is det.

varsinfolinear(Name,I,N,AS,Vars,Down,Up,MIn,MOut) :-
	(
	  I < N ->
	  bimap.lookup(AS,I,Atom),
	  (
	    prob.poslit(Name,Atom) ->
	    Vars = [I|T],
	    Down = [1|DownT],
	    (
	      prob.neglit(Name,Atom) ->
	      Up = [1|UpT];
	      Up = [0|UpT]
	    ),
	    varsinfolinear(Name,I+1,N,AS,T,DownT,UpT,MIn+1,MOut);
	    (
	      prob.neglit(Name,Atom) ->
	      Vars = [I|T],
	      Down = [0|DownT],
	      Up = [1|UpT],
	      varsinfolinear(Name,I+1,N,AS,T,DownT,UpT,MIn+1,MOut);
	      varsinfolinear(Name,I+1,N,AS,Vars,Down,Up,MIn,MOut)
	    ));
	  Vars = [],
	  Down = [],
	  Up = [],
	  MOut = MIn
	).


%-----------------------------------------------------------------------------%
%
% Predicates for generating initial constraints and variables
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", makeclauses(out,out,out,out,out,out), "MR_initial_constraints").

% generates SCIP-understandable versions of initial constraints
% and also records which variables (i.e. ground atoms ) are used in them. 

:- pred makeclauses(as_next::out,                % atom store with all variables in the clauses
		    list(float)::out,            % objective value for each variable in atom store
		                                 % list is ordered, so first two entries are objective values
		                                 % of variables 0 and 1.
		    list(string)::out,           % names for each variable
		    list(string)::out,           % a name for each constraint
		    list(list(int))::out,        % list of neg lit indices for each clause
		    list(list(int))::out         % list of pos lit indices for each clause
		   ) is det.

% generate clauses (as terms)
% convert to indices, (creating indices where necessary)
% get hold of objective values
% finally create names

makeclauses(AtomStoreNext,VarObjs,VarNames,ConsNames,NegLitss,PosLitss) :-
	Call = (pred(named(Name,Lits)::out) is nondet :- prob.initial_clause(Name,lits([],[]),Lits)),
	solutions(Call,AllNamedInitialClauses),
	bimap.init(AS0),
	list.map2_foldl(clause2indices,AllNamedInitialClauses,NegLitss,PosLitss,as(AS0,0),AtomStoreNext),
	AtomStoreNext = as(AS,Next),
	allobjs(0,Next,AS,VarObjs,VarNames),
	name_all(AllNamedInitialClauses,ConsNames,map.init,_).

:- pred name_all(list(named_clause_lits)::in,list(string)::out,map(string,int)::in,map(string,int)::out) is det.

name_all([],[],!Map).
name_all([named(Name,_)|T],[NameNum|NT],In,Out) :-
	(
	  map.search(In,Name,I) ->
	  NameNum = Name ++ "_" ++ int_to_string(I),
	  map.det_update(Name,I+1,In,Mid);
	  NameNum = Name ++ "_1",
	  map.det_insert(Name,2,In,Mid)
	),
	name_all(T,NT,Mid,Out).

:- pred allobjs(int::in,int::in,atom_store::in,list(float)::out,list(string)::out) is det.

allobjs(I,Next,AS,VarObjs,VarNames) :-
	(
	  I < Next ->
	  Atom = bimap.lookup(AS,I),
	  VarObjs = [prob.objective(Atom)|T],
	  VarNames = [name(Atom)|VT],
	  allobjs(I+1,Next,AS,T,VT);
	  VarObjs = [],
	  VarNames = []
	).

% convert a clause (as a pair of lists of ground terms) into corresponding integers
% updating atomstore as we go. Process negative literals before positive literals.

:- pred clause2indices(named_clause_lits::in,list(int)::out,list(int)::out,as_next::in,as_next::out) is det.

clause2indices(named(_,lits(NegLits,PosLits)),NegLitIndices,PosLitIndices,!ASN) :-
	lits2indices(NegLits,NegLitIndices,!ASN),
	lits2indices(PosLits,PosLitIndices,!ASN).

% take a lists of ground atoms
% and return corresponding lists on indices using AtomStore
% adding variables to AtomStore if they are not already there

:- pred lits2indices(list(atom)::in,list(int)::out,as_next::in,as_next::out) is det.

lits2indices([],[],ASNIn,ASNIn).
lits2indices([Lit|Lits],[LitIndex|LitIndices],ASNIn,ASNOut) :-
	ASNIn = as(ASIn,Next),
	(
	  bimap.reverse_search(ASIn,LitIndex0,Lit) ->
	  LitIndex = LitIndex0,
	  lits2indices(Lits,LitIndices,ASNIn,ASNOut);
	  bimap.det_insert(Next,Lit,ASIn,ASMid),
	  LitIndex = Next,
	  ASNMid = as(ASMid,Next+1),
	  lits2indices(Lits,LitIndices,ASNMid,ASNOut)
	).

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




%-----------------------------------------------------------------------------%
%
% Predicates for generating cuts
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", existscut(in,in,in,in), "MR_existscut").

:- pred existscut(string::in,as_next::in,list(int)::in,list(float)::in) is semidet.

existscut(Name,ASN,Indices,Values) :-
	ASN = as(AtomStore,_),
	makesol(Indices,Values,AtomStore,map.init,Sol),
	clausal_cut(Name,Sol).

:- pred makesol(list(int)::in,list(float)::in,atom_store::in,sol::in,sol::out) is det.

makesol([],_Vals,_AtomStore,!Sol).
%next clause should never be called since should always be called with lists of the same length
makesol([_H|_T],[],_AtomStore,!Sol).
makesol([H|T],[VH|VT],AtomStore,!Sol) :-
	bimap.lookup(AtomStore,H,Atom),
	map.det_insert(Atom,VH,!Sol),
	makesol(T,VT,AtomStore,!Sol).

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

% in this version we just check for the existence of a cut

:- pred clausal_cut(string::in,sol::in) is semidet.

clausal_cut(Name,Sol) :-
	StateIn = clause_cut(Sol,0.0,[],[]),
	prob.clause(Name,StateIn,_StateOut).

% :- pred clausal_cut(string::in,sol::in,lincons::out) is nondet.

% clausal_cut(Name,Sol,Cut) :-
% 	StateIn = clause_cut(Sol,0.0,[],[]),
% 	prob.clause(Name,StateIn,StateOut),
% 	StateOut = clause_cut(_Sol,_Val,NegLits,PosLits),
% 	% actually just need to send back two sets of indices
% 	% this is enough for SCIP
% 	clause2lincons(NegLits,PosLits,Cut).

initial_poslit(Atom,lits(NegIn,PosIn),lits(NegIn,[Atom|PosIn])).
initial_neglit(Atom,lits(NegIn,PosIn),lits([Atom|NegIn],PosIn)).

poslit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn),
       clause_cut(Sol,ValOut,NegIn,[Atom|PosIn])) :-
	ValOut = ValIn+solval(Sol,Atom),
	ValOut < 1.0.

% use this to generate atoms for negative literals
insol(Atom,In,In) :-
	In  = clause_cut(Sol,_,_,_),
	map.member(Sol,Atom,_).

neglit(Atom,
       clause_cut(Sol,ValIn,NegIn,PosIn),
       clause_cut(Sol,ValOut,[Atom|NegIn],PosIn)) :-
	% next goal fails if Atom not in the solution,
	% which is what we want
	map.search(Sol,Atom,Val),
	ValOut = ValIn+1.0-Val,
	ValOut < 1.0.



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
