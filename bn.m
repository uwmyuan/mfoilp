:- module prob.
:- interface.

%----------------------------------------------------------------------%



:- import_module mercury_lib.

:- type atom.

:- pred atom(atom::out) is multi.
:- pred initial_constraint(lincons::out) is nondet.
:- pred delayed_constraint(lincons::out) is nondet.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module int.
:- import_module float.
:- import_module solutions.

% define atom type

:- type node == int.
:- type fv ---> fv(node,list(node)).
:- type atom == fv.


:- pred fv(fv::out) is multi.
fv(fv(Child,Parents)) :- score(Child,Parents,_).
%fv(fv(0,[])).

:- pred node(node::out) is multi.
node(Child) :- fv(fv(Child,_)).

% define atom-variable generator
atom(X) :- fv(X).

% fake local scores
objective(fv(Child,Parents)) = Score :-
	(
	  score(Child,Parents,Score0) ->
	  Score = -Score0;
	  Score = 0.0
	).
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.

% constraints

%convexity
initial_constraint(lincons(finite(1.0),LinExpr,finite(1.0))) :-
	node(Child),
	Out = (pred(1.0 * fv(Child1,Parents)::out) is nondet :- Child = Child1, score(Child,Parents,_)),
	solutions(Out,LinExpr).

%cluster constraints
delayed_constraint(lincons(finite(1.0),LinExpr,posinf)) :-
	cluster(Cluster),
	length(Cluster) > 1,
	Out = (
		pred(1.0 * fv(Child,Parents)::out) is nondet :- member(Child,Cluster),
		score(Child,Parents,_),
		not (member(X,Parents),member(X,Cluster))
	      ),
	solutions(Out,LinExpr).

:- pred cluster(list(node)::out) is multi.

cluster(Cluster) :-
	solutions(node,Nodes),
	mysublist(Nodes,Cluster).

:- pred mysublist(list(node)::in,list(node)::out) is multi.

mysublist([],[]).
mysublist([H|T],[H|T2]) :- mysublist(T,T2).
mysublist([_H|T],T2) :- mysublist(T,T2).

% local scores

:- pred score(node,list(node),float).

:- mode score(out,out,out) is multi.
%:- mode score(out,out,out) is nondet.
:- mode score(in,in,out) is semidet.
:- mode score(in,out,out) is nondet.

% score(Child,[P1,P2,P3],Score) :-
% 	score_table(Child,P1,P2,P3,Score),
% 	not P1 = -1,
% 	not P2 = -1,
% 	not P3 = -1.

% score(Child,[P1,P2],Score) :-
% 	score_table(Child,P1,P2,-1,Score),
% 	not P1 = -1,
% 	not P2 = -1.

% score(Child,[P1],Score) :-
% 	score_table(Child,P1,-1,-1,Score),
% 	not P1 = -1.

% score(Child,[],Score) :-
% 	score_table(Child,-1,-1,-1,Score).
	  



% :- pred score_table(int,int,int,int,float).
% :- mode score_table(out,out,out,out,out) is multi.
% :- mode score_table(in,in,in,in,out) is semidet.
% :- mode score_table(in,out,out,out,out) is nondet.

% :- pragma fact_table(score_table/5, "scores.m").

score(0,[2,5],-64.006043).
score(0,[1,2],-64.006043).
score(0,[2],-65.918644).
score(0,[5,7],-67.478468).
score(0,[1,7],-67.478468).
score(0,[5],-67.580857).
score(0,[1],-67.580857).
score(0,[7],-68.789131).
score(0,[6],-70.979812).
score(0,[],-71.123881).
score(1,[4,5],-2.247729).
score(1,[5],-3.009765).
score(1,[0,6],-8.070367).
score(1,[4,6],-9.375344).
score(1,[6],-9.380638).
score(1,[0,4],-21.675646).
score(1,[0],-21.690315).
score(1,[],-25.233339).
score(2,[7],-52.010149).
score(2,[0],-66.320026).
score(2,[],-71.525263).
score(3,[4],-12.214946).
score(3,[],-12.354096).
score(4,[1,5],-2.247729).
score(4,[3],-2.737050).
score(4,[],-2.876200).
score(5,[1,4],-2.247729).
score(5,[1],-3.009765).
score(5,[0,6],-8.070367).
score(5,[4,6],-9.375344).
score(5,[6],-9.380638).
score(5,[0,4],-21.675646).
score(5,[0],-21.690315).
score(5,[],-25.233339).
score(6,[5],-16.935375).
score(6,[1],-16.935375).
score(6,[0],-32.644008).
score(6,[],-32.788076).
score(7,[2],-52.250461).
score(7,[0],-69.430826).
score(7,[],-71.765576).

