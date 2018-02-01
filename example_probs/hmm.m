% implements Viterbi calculation for the hmm.psm example
% which comes with PRISM
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
:- import_module math.

:- type obs ---> a ; b.
:- type state ---> init ; s0 ; s1.

:- type atom ---> at_state(int,state) ; trans(int,state,state) ; emit(int,state,obs).

objective(Atom,-ln(Prob)) :- probs(Atom,Prob). 

:- pred probs(atom::in,float::out) is semidet.

probs(emit(_,s0,a),0.5).
probs(emit(_,s0,b),0.5).
probs(emit(_,s1,a),0.6).
probs(emit(_,s1,b),0.4).

probs(trans(_,init,s0),0.9).
probs(trans(_,init,s1),0.1).
probs(trans(_,s0,s0),0.2).
probs(trans(_,s0,s1),0.8).
probs(trans(_,s1,s0),0.8).
probs(trans(_,s1,s1),0.2).


:- pred obs(int::in,obs::out) is semidet.

obs(1,a).
obs(2,a).
obs(3,a).
obs(4,a).
obs(5,a).
obs(6,b).
obs(7,b).
obs(8,b).
obs(9,b).
obs(10,b).


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
    insol(at_state(I,State)),
    neglit(at_state(I,State)),
    {obs(I+1,_Obs)},
    poslit(trans(I,State,s0)),
    poslit(trans(I,State,s1)).
neglit("dyn",at_state(_,_)).
poslit("dyn",trans(_,_,_)).

clause("trans").
clause("trans") -->
    insol(at_state(I,State)),
    neglit(at_state(I,State)),
    insol(trans(I,State,State2)),
    neglit(trans(I,State,State2)),
    poslit(at_state(I+1,State2)).
neglit("trans",at_state(_,_)).
neglit("trans",trans(_,_,_)).
poslit("trans",at_state(_,_)).

initial_clause("start") -->
	initial_poslit(at_state(0,init)).

    

% extras
equality(_) :- fail.

% only used if generating and constraints
penalty_atom(_) :- fail.
