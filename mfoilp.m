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
:- import_module solutions.
:- import_module io.
:- import_module string.builder.
:- import_module stream.string_writer.
:- import_module list.
:- import_module float.
:- import_module array.
:- import_module require.


% :- type atom_store == bimap(int,prob.atom).
% 2nd argument is the index of the next variable to be created
:- type as_next ---> as(
			array(atom),             % maps ints to atoms
			map(atom,int),           % and vice-versa
			int                      % number of atoms stored = next index
		       ).

:- type sol == map(atom,float).
:- type clause_info ---> clause_cut(
				    sol,          % solution for which a cut is sought (does not change)
				    float,        % activity of the ground clause constructed so far
				    list(atom),   % negative literals in clause so far
				    list(atom)    % positive literals in clause so far
				   ).

:- type clause_lits ---> lits(list(atom),list(atom)).
:- type named_clause_lits ---> named(string,clause_lits).

:- mutable(atomstore, as_next, as(array.make_empty_array,map.init,0), ground, [untrailed]).


%-----------------------------------------------------------------------------%
%
% Predicates for generating initial constraints and variables
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", makeclauses(out,out,out,out,out), "MR_initial_constraints").

% generates SCIP-understandable versions of initial constraints
% and also records which variables (i.e. ground atoms ) are used in them. 

:- pred makeclauses(list(float)::out,            % objective value for each variable in atom store
		                                 % list is ordered, so e.g. first two entries are objective values
		                                 % of variables 0 and 1.
		    list(string)::out,           % a name for each variable
		    list(string)::out,           % a name for each constraint
		    list(list(int))::out,        % list of neg lit indices for each clause
		    list(list(int))::out         % list of pos lit indices for each clause
		   ) is det.

% generate clauses (as terms)
% convert to indices, (creating indices where necessary)
% get hold of objective values
% finally create names
:- pragma promise_pure(makeclauses/5).

makeclauses(VarObjs,VarNames,ConsNames,NegLitss,PosLitss) :-
    Call = (pred(named(Name,Lits)::out) is nondet :- prob.initial_clause(Name,lits([],[]),Lits)),
    solutions(Call,AllNamedInitialClauses),
    list.map2_foldl3(clause2indices,AllNamedInitialClauses,NegLitss,PosLitss,map.init,Map,[],NewLits,0,Next),
	 ( NewLits = [Lit|_] -> array.init(Next,Lit,InitArray); make_empty_array(InitArray) ),
	 (max(InitArray) > -2 -> true; error("failoxo")),
    setall(NewLits,Next-1,InitArray,Array),
    AtomStoreNext = as(Array,Map,Next),
    allobjs(0,Next,Array,VarObjs,VarNames),
    name_all(AllNamedInitialClauses,ConsNames,map.init,_),
    impure set_atomstore(AtomStoreNext).

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

:- pred allobjs(int::in,int::in,array(atom)::in,list(float)::out,list(string)::out) is det.

allobjs(I,Next,Array,VarObjs,VarNames) :-
	(
	    I < Next ->
		(max(Array) > -2 -> true; error("failb")),
	  array.lookup(Array,I,Atom),
	  (
	    prob.objective(Atom,Obj0) ->
	    Obj = Obj0;
	    Obj = 0.0
	  ),
	  VarObjs = [Obj|T],
	  VarNames = [name(Atom)|VT],
	  allobjs(I+1,Next,Array,T,VT);
	  VarObjs = [],
	  VarNames = []
	).

% convert a clause (as a pair of lists of ground terms) into corresponding integers
% updating atomstore as we go. Process negative literals before positive literals.

:- pred clause2indices(named_clause_lits::in,list(int)::out,list(int)::out,
		       map(atom,int)::in,map(atom,int)::out,
		       list(atom)::in,list(atom)::out,
		       int::in,int::out
		      ) is det.

clause2indices(named(_,lits(NegLits,PosLits)),NegLitIndices,PosLitIndices,!Map,!NewLits,!N) :-
	lits2indices(NegLits,NegLitIndices,!Map,!NewLits,!N),
	lits2indices(PosLits,PosLitIndices,!Map,!NewLits,!N).

% take a lists of ground atoms
% and return corresponding lists on indices using AtomStore
% adding variables to AtomStore if they are not already there

:- pred lits2indices(list(atom)::in,list(int)::out,
		       map(atom,int)::in,map(atom,int)::out,
		       list(atom)::in,list(atom)::out,
		       int::in,int::out
		    ) is det.

lits2indices([],[],!Map,!NewLits,!N).
lits2indices([Lit|Lits],[LitIndex|LitIndices],MapIn,MapOut,NewLitsIn,NewLitsOut,M,N) :-
	(
	  map.search(MapIn,Lit,LitIndex0) ->
	  LitIndex = LitIndex0,
	  lits2indices(Lits,LitIndices,MapIn,MapOut,NewLitsIn,NewLitsOut,M,N);
	  map.det_insert(Lit,M,MapIn,NewMap),
	  LitIndex = M,
	  lits2indices(Lits,LitIndices,NewMap,MapOut,[Lit|NewLitsIn],NewLitsOut,M+1,N)
	).

:- func name(T) = string.

name(X) = Name :-
	State0 = string.builder.init,
	stream.string_writer.write(string.builder.handle,X,State0,State),
	Name = string.builder.to_string(State).


%-----------------------------------------------------------------------------%
%
% Predicate for generating the names of first-order clauses
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", foclausenames(out), "MR_delayed_clauses").

:- pred foclausenames(list(string)::out) is det.

foclausenames(Names) :-
	solutions(prob.clause,Names).

%-----------------------------------------------------------------------------%
%
% Predicate for computing variable locks due to first-order clauses
%
%-----------------------------------------------------------------------------%


:- pragma foreign_export("C", locks(in,in,out,out), "MR_locks").
:- pragma promise_pure(locks/4).

:- pred locks(string::in,int::in,int::out,int::out) is det.

locks(Name,I,Down,Up) :-
    semipure get_atomstore(as(Array,_Map,_N)),
    (
	prob.equality(Name) ->
		 Down=1,Up=1;
	             (max(Array) > -2 -> true; error("failc")),
	array.lookup(Array,I,Atom),
	(
	  % a positive literal is down-locked
	  prob.poslit(Name,Atom) ->
	  Down = 1;
	  Down = 0
	),
	(
	  % a negative literal is up-locked
	  prob.neglit(Name,Atom) ->
	  Up = 1;
	  Up = 0
	)
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for generating cuts
%
%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", existscut(in,in,in), "MR_existscut").
:- pragma promise_pure(existscut/3).
:- pred existscut(string::in,list(int)::in,list(float)::in) is semidet.

existscut(Name,Indices,Values) :-
    semipure get_atomstore(as(Array,_Map,_N)),
    (max(Array) > -2 -> true; error("failz")),
	makesol(Indices,Values,Array,map.init,Sol),
	clausal_cut(Name,Sol).

% find cuts and return the cuts (as a list of indices of neg and pos literals)
% together with the objectives and names of any new variables
% currently don't pass back the name of the cut

:- pragma foreign_export("C", findcuts(in,out,in,in,out,out,out,out), "MR_findcuts").
:- pragma promise_pure(findcuts/8).
:- pred findcuts(
	    string::in,             % name of the first-order clause for which cuts are sought
	    int::out,               % = 1 if these are 'equality clauses', otherwise 0
	    list(int)::in,          % indices of variables with non-zero values in the (LP) solution
	    list(float)::in,        % values of variables with non-zero values in the (LP) solution
	    list(list(int))::out,   % list of neg lit indices for each cut
	    list(list(int))::out,   % list of pos lit indices for each cut
	    list(float)::out,       % list of objective values for new variables
	    list(string)::out       % list of names for new variables
	) is det.

findcuts(Name,Equality,Indices,Values,NegLitss,PosLitss,VarObjs,VarNames) :-
    semipure get_atomstore(as(ArrayIn,MapIn,M)),
    (prob.equality(Name) -> Equality=1 ; Equality=0),
    (size(ArrayIn) = M -> true; error("fail")),
            (max(ArrayIn) > -2 -> true; error("failxx")),
    makesol(Indices,Values,ArrayIn,map.init,Sol),
    solutions(clausal_cut(Name,Sol),NamedCuts),
    list.map2_foldl3(clause2indices,NamedCuts,NegLitss,PosLitss,MapIn,MapOut,[],NewLits,M,N),
    (NewLits = [Lit|_] -> array.resize(N,Lit,ArrayIn,ArrayMid); ArrayMid=ArrayIn),
    setall(NewLits,N-1,ArrayMid,ArrayOut),
    impure set_atomstore(as(ArrayOut,MapOut,N)),
    allobjs(M,N,ArrayOut,VarObjs,VarNames).

:- pred setall(list(atom)::in,int::in,array(atom)::in,array(atom)::out) is det.

setall([],_,!Array).
setall([Lit|Lits],I,!Array) :-
    unsafe_set(I,Lit,!Array),
    setall(Lits,I-1,!Array).


:- pred makesol(list(int)::in,list(float)::in,array(atom)::in,sol::in,sol::out) is det.

makesol([],_Vals,_Array,!Sol).
%next clause should never be called since should always be called with lists of the same length
makesol([_H|_T],[],_Array,!Sol).
makesol([H|T],[VH|VT],Array,!Sol) :-
        (max(Array) > -2 -> true; error("faila")),
	array.lookup(Array,H,Atom),
	map.det_insert(Atom,VH,!Sol),
	makesol(T,VT,Array,!Sol).

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
    (
	prob.clause(Name,clause_cut(Sol,0.0,[],[]),_StateOut) ->
	true;
	prob.equality(Name),
	fail %replace with search for cut due to being 'too true'
    ).

% in this version we output the cut as a pair of lists of lits

:- pred clausal_cut(string::in,sol::in,named_clause_lits::out) is nondet.

clausal_cut(Name,Sol,named("cut",lits(NegLits,PosLits))) :-
 	prob.clause(Name,clause_cut(Sol,0.0,[],[]),clause_cut(_Sol,_Val,NegLits,PosLits)).

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
