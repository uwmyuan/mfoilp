:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred clause(string::out) is multi.
:- pred equality(string::in) is failure.
:- pred penalty_atom(string::in) is failure.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.

:- type obs ---> a ; b.
:- type state ---> s1 ; s2.

:- type atom ---> at_state(int,state) ; trans(int,state,state) ; emit(int,state,obs).

objective(emit(_,s1,a),2.0).
objective(emit(_,s1,b),12.0).
objective(emit(_,s2,a),2.0).
objective(emit(_,s2,b),1.0).

objective(trans(_,s1,s1),2.0).
objective(trans(_,s1,s2),12.0).
objective(trans(_,s2,s1),2.0).
objective(trans(_,s2,s2),1.0).


:- pred state(state::out) is multi.
state(s1).
state(s2).

:- pred obs(int,obs).
:- mode obs(out,out) is multi.
:- mode obs(in,out) is semidet.

obs(1,a).
obs(2,a).
obs(3,a).
obs(4,a).
obs(5,a).
obs(6,a).
obs(7,b).
obs(8,b).
obs(9,a).
obs(10,b).
obs(11,b).
obs(12,a).

clause("emit").
clause("emit") -->
    insol(at_state(I,State)),
    neglit(at_state(I,State)),
    {obs(I,Obs)},
    poslit(emit(I,State,Obs)).
neglit("emit",at_state(_,_)).
poslit("emit",emit(_,_,_)).

clause("dyn").
clause("dyn") -->
    {obs(I,_Obs)},
    poslit(at_state(I,s1)),
    poslit(at_state(I,s2)).
poslit("dyn",at_state(_,_)).

clause("trans").
clause("trans") -->
    insol(at_state(I,State)),
    neglit(at_state(I,State)),
    {state(State2)},
    neglit(trans(I,State,State2)),
    poslit(at_state(I+1,State2)).
neglit("trans",at_state(_,_)).
neglit("trans",trans(_,_,_)).
poslit("trans",at_state(_,_)).

initial_clause("start") -->
	initial_poslit(at_state(0,s1)).

    

% extras
equality(_) :- fail.

% only used if generating and constraints
penalty_atom(_) :- fail.
