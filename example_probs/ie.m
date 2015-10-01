:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is nondet.

:- pred clause(string::out) is multi.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------%

:- type field ---> author ; title ; venue.
:- type bib == int.
:- type pos == int.

% define atom type

:- type atom ---> infield(bib,field,pos)
	; samebib(bib,bib)
	; cb(string,field,bib,pos).

objective(cb(Token,Field,_Bib,_Pos),Cost) :- weight(Token,Field,Cost).

:- pred weight(string::in,field::in,float::out) is semidet.

weight(Token,author,Cost) :- ft_weight(Token,"author",Cost).
weight(Token,title,Cost) :- ft_weight(Token,"title",Cost).
weight(Token,venue,Cost) :- ft_weight(Token,"venue",Cost).

:- pred ft_weight(string::in,string::in,float::out) is semidet.
:- pragma fact_table(ft_weight/3,"weights").


% Rockit 'observed predicates' are just predicates in the Mercury code
% and their atoms are not translated to SCIP variables

% predicates expressing data are more efficiently processed if in a Mercury
% fact table

:- pred token(string::out,pos::out,bib::out) is multi.
:- pragma fact_table(token/3,"tokens").

:- pred similarvenue(bib::out,bib::out) is multi.
:- pragma fact_table(similarvenue/2,"similarvenues").

:- pred similartitle(bib::out,pos::out,pos::out,bib::out,pos::out,pos::out) is multi.
:- pragma fact_table(similartitle/6,"similartitles").

:- pred jntinfcandidate(bib::out,pos::out,bib::out) is multi.
:- pragma fact_table(jntinfcandidate/3,"jntinfcandidates").


:- pred year_prefix(string::out) is multi.
year_prefix("18").
year_prefix("19").
year_prefix("20").

:- pred isdate(string::in) is semidet.
isdate("january").
isdate("february").
isdate("march").
isdate("april").
isdate("may").
isdate("june").
isdate("july").
isdate("august").
isdate("september").
isdate("october").
isdate("november").
isdate("december").
% this is better than having to list every year!
isdate(Year) :-
	string.length(Year,4),
	year_prefix(Prefix),
	string.prefix(Year,Prefix).


:- pred isdigit(string::in) is semidet.
% this is better than having to list every number!
isdigit(Number) :- string.is_all_digits(Number).

:- pred isalphachar(string::in) is semidet.
isalphachar(Char) :-
	string.length(Char,1),
	string.is_all_alpha(Char).

:- pred next(int::in,int::out) is det.
next(I,I+1).

:- pred lessthan(int::in,int::in) is semidet.
lessthan(I,J) :- I < J.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred field(field::out) is multi.
field(author).
field(venue).
field(title).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_clause(_,_,_) :- fail.

clause("exclusion1").
clause("exclusion1") -->
		     insol(infield(Bib,author,Pos)),
		     neglit(infield(Bib,author,Pos)),		     
		     neglit(infield(Bib,title,Pos)).
neglit("exclusion1",infield(_Bib,author,_Pos)).
neglit("exclusion1",infield(_Bib,title,_Pos)).


clause("exclusion2").
clause("exclusion2") -->
		     insol(infield(Bib,author,Pos)),
		     neglit(infield(Bib,author,Pos)),		     
		     neglit(infield(Bib,venue,Pos)).
neglit("exclusion2",infield(_Bib,author,_Pos)).
neglit("exclusion2",infield(_Bib,venue,_Pos)).


clause("exclusion3").
clause("exclusion3") -->
		     insol(infield(Bib,title,Pos)),
		     neglit(infield(Bib,title,Pos)),		     
		     neglit(infield(Bib,venue,Pos)).
neglit("exclusion3",infield(_Bib,title,_Pos)).
neglit("exclusion3",infield(_Bib,venue,_Pos)).

% each token in a given position in a give bib
% should be either a date, digit or a field (or there's a cost!)
clause("main").
clause("main") -->
		   {
		    token(Token,Pos,Bib),
		    not isdate(Token),
		    not isdigit(Token),
		    field(Field)
		   },
		   poslit(infield(Bib,Field,Pos)),
                   poslit(cb(Token,Field,Bib,Pos)).		 
poslit("main",infield(_Bib,_Field,_Pos)).
poslit("main",cb(_Token,_Field,_Bib,_Pos)).		

