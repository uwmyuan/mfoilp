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

:- type direction ---> l ; r ; u ;  d ; s.

% define atom type

:- type atom ---> move(int,direction) ; position(int,int,int).

% each move has a cost

objective(move(_,_),1.0).

% good and bad places

%objective(position(_,X,Y),Out) :-
%	(
%	  X = 2, Y = 3 ->
%	  Out = -2.0;
%	  X = Y, Out = 2.0
%	).

%objective(position(_,2,3),-2.0).
objective(position(_,X,X),2.0).

:- pred makemove(direction,int,int,int,int).
:- mode makemove(in,in,in,out,out) is det.
:- mode makemove(in,out,out,in,in) is det.

makemove(l,X,Y,X-1,Y).
makemove(r,X,Y,X+1,Y).
makemove(u,X,Y,X,Y+1).
makemove(d,X,Y,X,Y-1).
makemove(s,X,Y,X,Y).

% at most one move at any time point

clause("onedirection").
clause("onedirection") -->
	insol(move(I,D1)),
	insol(move(I,D2)),
	{not D1 = D2},
	neglit(move(I,D1)),
	neglit(move(I,D2)).
neglit("onedirection",move(_,_)).

clause("oneposition").
clause("oneposition") -->
	insol(position(I,X1,Y1)),
	insol(position(I,X2,Y2)),
	{not (X1 = X2, Y1=Y2)},
	neglit(position(I,X1,Y1)),
	neglit(position(I,X2,Y2)).
neglit("oneposition",position(_,_,_)).

clause("oneway").
clause("oneway") -->
	insol(position(I,X,Y)),
	{not I = 0},
	neglit(position(I,X,Y)),
	poslit(move(I-1,l)),
	poslit(move(I-1,r)),
	poslit(move(I-1,u)),
	poslit(move(I-1,s)),
	poslit(move(I-1,d)).
neglit("oneway",position(_,_,_)).
poslit("oneway",move(_,_)).

clause("moveinv").
clause("moveinv") -->
	insol(position(I,NewX,NewY)),
	{not I = 0},
	neglit(position(I,NewX,NewY)),
	insol(move(I-1,D)),
	neglit(move(I-1,D)),
	{makemove(D,X,Y,NewX,NewY)},
	poslit(position(I-1,X,Y)).
neglit("moveinv",position(I,_,_)) :- not I = 0.
neglit("moveinv",move(_,_)).
poslit("moveinv",position(_,_,_)).


clause("move").
clause("move") -->
	insol(position(I,X,Y)),
	insol(move(I,D)),
	neglit(position(I,X,Y)),
	neglit(move(I,D)),
	{makemove(D,X,Y,NewX,NewY)},
	poslit(position(I+1,NewX,NewY)).
neglit("move",position(_,_,_)).
poslit("move",position(_,_,_)).
neglit("move",move(_,_)).

initial_clause("start") -->
	initial_poslit(position(0,0,0)).

initial_clause("end") -->
	initial_poslit(position(6,4,2)).

