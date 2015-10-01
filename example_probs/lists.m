:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred clause(string::out) is multi.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module list.

%----------------------------------------------------------------------%

% define atom type

:- type atom ---> f(int,list(int)) ; cb(int).

% provide non-zero objective values for each atom-variable

objective(cb(X),1.0/float(X)).
objective(f(_,[_|_]),0.01).

initial_clause("data1") -->
	initial_poslit(f(1,[1])).

clause("fo1").
clause("fo1") -->
	insol(f(J,List)),
	neglit(f(J,List)),
	{list.append(List,[J+1],NewList)},
	poslit(f(J+1,NewList)),
	poslit(cb(J)).
neglit("fo1",f(_,_)).
poslit("fo1",f(_,_)).
poslit("fo1",cb(_)).
