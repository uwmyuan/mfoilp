:- module prob.
:- interface.

%----------------------------------------------------------------------%



:- import_module mercury_lib.

:- type atom.

:- pred atom(atom::out) is multi.
:- pred initial_constraint(lincons::out) is nondet.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module int.
:- import_module solutions.

% define atom type

:- type node == int.
:- type edge ---> e(node,node).
:- type atom == edge.




:- pred node(node::out) is multi.
node(X) :- node_gen(1,X).

:- pred node_gen(node::in,node::out) is multi.
node_gen(X,X).
node_gen(X,Y) :- X < 6, node_gen(X+1,Y).


:- pred edge(edge::out) is multi.
edge(e(1,2)).
edge(e(1,3)).
edge(e(1,4)).
edge(e(2,5)).
edge(e(2,6)).
edge(e(5,6)).

% define atom-variable generator
atom(X) :- edge(X).

objective(_Atom) = 1.0.
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.

% constraints

:- pred source(node::out) is det.
source(1).


:- pred target(node::out) is det.
target(6).

initial_constraint(lincons(finite(1.0),LinExpr,finite(1.0))) :-
	source(X),
	Out = (pred(1.0 * E::out) is nondet :- edge(E), E = e(X,_)),
	solutions(Out,LinExpr).

initial_constraint(lincons(finite(1.0),LinExpr,finite(1.0))) :-
	target(X),
	In = (pred(1.0 * E::out) is nondet :- edge(E), E = e(_,X)),
	solutions(In,LinExpr).

initial_constraint(lincons(finite(0.0),LinExpr,finite(0.0))) :-
	node(X),
	not source(X),
	not target(X),
	In =  (pred(1.0 * E::out) is nondet :- edge(E), E=e(_,X)),
	solutions(In,InEdges),
	Out = (pred(-1.0 * E::out) is nondet :- edge(E), E = e(X,_)),
	solutions(Out,OutEdges),
	append(InEdges,OutEdges,LinExpr).


