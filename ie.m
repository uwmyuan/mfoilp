
:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred clause(string::out) is nondet.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- func objective(atom) = float.

%----------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------%

% define atom type


:- type atom ---> obs_isalphachar(string) ;
obs_lastinitial(string,string) ;
obs_next(string,string) ;
obs_similarvenue(string,string) ;
obs_jntinfcandidate(string,string,string) ;
obs_token(string,string,string) ;
obs_similartitle(string,string,string,string,string,string) ;
obs_center(string,string) ;
obs_hascomma(string,string) ;
obs_isdigit(string) ;
obs_haspunc(string,string) ;
obs_firstin(string,string) ;
obs_followby(string,string,string) ;
obs_isdate(string) ;
obs_firstnonauthortitletkn(string,string) ;
obs_lessthan(string,string).


:- pred obs_isalphachar(string::out) is multi.
:- pragma fact_table(obs_isalphachar/1, "obs_isalphachar").
initial_clause("obs_isalphachar") --> {obs_isalphachar(X0)}, initial_poslit(obs_isalphachar(X0)).
:- pred obs_lastinitial(string::out,string::out) is multi.
:- pragma fact_table(obs_lastinitial/2, "obs_lastinitial").
initial_clause("obs_lastinitial") --> {obs_lastinitial(X0,X1)}, initial_poslit(obs_lastinitial(X0,X1)).
:- pred obs_next(string::out,string::out) is multi.
:- pragma fact_table(obs_next/2, "obs_next").
initial_clause("obs_next") --> {obs_next(X0,X1)}, initial_poslit(obs_next(X0,X1)).
:- pred obs_similarvenue(string::out,string::out) is multi.
:- pragma fact_table(obs_similarvenue/2, "obs_similarvenue").
initial_clause("obs_similarvenue") --> {obs_similarvenue(X0,X1)}, initial_poslit(obs_similarvenue(X0,X1)).
:- pred obs_jntinfcandidate(string::out,string::out,string::out) is multi.
:- pragma fact_table(obs_jntinfcandidate/3, "obs_jntinfcandidate").
initial_clause("obs_jntinfcandidate") --> {obs_jntinfcandidate(X0,X1,X2)}, initial_poslit(obs_jntinfcandidate(X0,X1,X2)).
:- pred obs_token(string::out,string::out,string::out) is multi.
:- pragma fact_table(obs_token/3, "obs_token").
initial_clause("obs_token") --> {obs_token(X0,X1,X2)}, initial_poslit(obs_token(X0,X1,X2)).
:- pred obs_similartitle(string::out,string::out,string::out,string::out,string::out,string::out) is multi.
:- pragma fact_table(obs_similartitle/6, "obs_similartitle").
initial_clause("obs_similartitle") --> {obs_similartitle(X0,X1,X2,X3,X4,X5)}, initial_poslit(obs_similartitle(X0,X1,X2,X3,X4,X5)).
:- pred obs_center(string::out,string::out) is multi.
:- pragma fact_table(obs_center/2, "obs_center").
initial_clause("obs_center") --> {obs_center(X0,X1)}, initial_poslit(obs_center(X0,X1)).
:- pred obs_hascomma(string::out,string::out) is multi.
:- pragma fact_table(obs_hascomma/2, "obs_hascomma").
initial_clause("obs_hascomma") --> {obs_hascomma(X0,X1)}, initial_poslit(obs_hascomma(X0,X1)).
:- pred obs_isdigit(string::out) is multi.
:- pragma fact_table(obs_isdigit/1, "obs_isdigit").
initial_clause("obs_isdigit") --> {obs_isdigit(X0)}, initial_poslit(obs_isdigit(X0)).
:- pred obs_haspunc(string::out,string::out) is multi.
:- pragma fact_table(obs_haspunc/2, "obs_haspunc").
initial_clause("obs_haspunc") --> {obs_haspunc(X0,X1)}, initial_poslit(obs_haspunc(X0,X1)).
:- pred obs_firstin(string::out,string::out) is multi.
:- pragma fact_table(obs_firstin/2, "obs_firstin").
initial_clause("obs_firstin") --> {obs_firstin(X0,X1)}, initial_poslit(obs_firstin(X0,X1)).
:- pred obs_followby(string::out,string::out,string::out) is multi.
:- pragma fact_table(obs_followby/3, "obs_followby").
initial_clause("obs_followby") --> {obs_followby(X0,X1,X2)}, initial_poslit(obs_followby(X0,X1,X2)).
:- pred obs_isdate(string::out) is multi.
:- pragma fact_table(obs_isdate/1, "obs_isdate").
initial_clause("obs_isdate") --> {obs_isdate(X0)}, initial_poslit(obs_isdate(X0)).
:- pred obs_firstnonauthortitletkn(string::out,string::out) is multi.
:- pragma fact_table(obs_firstnonauthortitletkn/2, "obs_firstnonauthortitletkn").
initial_clause("obs_firstnonauthortitletkn") --> {obs_firstnonauthortitletkn(X0,X1)}, initial_poslit(obs_firstnonauthortitletkn(X0,X1)).
:- pred obs_lessthan(string::out,string::out) is multi.
:- pragma fact_table(obs_lessthan/2, "obs_lessthan").
initial_clause("obs_lessthan") --> {obs_lessthan(X0,X1)}, initial_poslit(obs_lessthan(X0,X1)).
objective(_Atom) = 0.0.
clause(_,_,_) :- fail.
clause(_) :- fail.
neglit(_,_) :- fail.
poslit(_,_) :- fail.
