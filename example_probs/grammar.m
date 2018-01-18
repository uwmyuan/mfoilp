:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is nondet.

:- pred clause(string::out) is multi.
:- pred equality(string::in) is failure.
:- pred penalty_atom(string::in) is failure.

:- pred neglit(string::in,atom::in) is failure.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is det.

%----------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type number ---> singular ; plural ; none.
:- type nil_type ---> nil.
:- type that_type ---> that.

:- type words == list(string).
       
:- type sentence_parse ---> s(noun_phrase_parse,verb_phrase_parse).

:- type noun_phrase_parse ---> np(det_parse,noun_parse,rel_clause_parse)
   ; np(name_parse).

:- type verb_phrase_parse ---> vp(trans_verb_parse,noun_phrase_parse)
   ; vp(intrans_verb_parse).

:- type rel_clause_parse ---> rel(that_type,verb_phrase_parse) ;
   rel(nil_type).

:- type det_parse ---> det(string) ; det2(nil_type).

:- type noun_parse ---> n(string).

:- type name_parse ---> name(string).

:- type trans_verb_parse ---> tv(string).

:- type intrans_verb_parse ---> iv(string).

:- type atom ---> bad_parse(sentence_parse).

initial_clause(_,_,_) :- fail.

equality(_) :- fail.
penalty_atom(_) :- fail.
neglit(_,_) :- fail.

objective(_Parse,1.0).   

%clause("test").
initial_clause("test") -->
    {
	sentence(Parse,["all","men","like","men"],[])
    },
    initial_poslit(bad_parse(Parse)).
%poslit("test",dummy(_)).

clause("test2").
clause("test2") -->
    {sentence(Parse,["all","men","like","mary"],[])},
    poslit(bad_parse(Parse)).
poslit("test2",bad_parse(_)).




:- pred sentence(sentence_parse::out,words::in,words::out) is nondet.
sentence(s(NP,VP)) --> 
    noun_phrase(N, NP), verb_phrase(N,VP).

:- pred noun_phrase(number::out,noun_phrase_parse::out,words::in,words::out) is nondet.
noun_phrase(N,np(Det,Noun,Rel)) --> 
	determiner(N, Det), noun(N,Noun), rel_clause(N1,Rel), {N1=none ; N1=N}.
noun_phrase(singular,np(Name)) --> 
    name(Name).

:- pred verb_phrase(number::out,verb_phrase_parse::out,words::in,words::out) is nondet.
verb_phrase(N,vp(TV,NP)) --> 
	trans_verb(N,TV), noun_phrase(_,NP).
verb_phrase(N,vp(IV)) --> 
    intrans_verb(N,IV).

:- pred rel_clause(number::out, rel_clause_parse::out, words::in, words::out) is multi.
rel_clause(N,rel(that,VP)) --> 
	["that"],verb_phrase(N,VP).
rel_clause(none,rel(nil)) --> [].

:- pred determiner(number::out,det_parse::out,words::in, words::out) is multi.
determiner(N,det(W)) --> [W],{is_determiner(W,N)}.
determiner(plural,det2(nil)) --> [].

:- pred noun(number::out,noun_parse::out,words::in,words::out) is semidet.
noun(N,n(Root)) --> [W],{is_noun(W,N,Root)}.

:- pred name(name_parse::out, words::in, words::out) is semidet.
name(name(W)) --> [W],{is_name(W)}.

:- pred trans_verb(number::out,trans_verb_parse::out,words::in,words::out) is semidet.
trans_verb(N,tv(Root)) --> [W],{is_trans(W,N,Root)}.

:- pred intrans_verb(number::out,intrans_verb_parse::out,words::in,words::out) is semidet.
intrans_verb(N,iv(Root)) --> [W],{is_intrans(W,N,Root)}.

:- pred is_determiner(string::in,number::out) is semidet.
is_determiner("every",singular).
is_determiner("all",plural).

:- pred is_noun(string::in,number::out,string::out) is semidet.
is_noun("boats",plural,"boat").
is_noun("man",singular,"man").
is_noun("men",plural,"man").

:- pred is_name(string::in) is semidet.
is_name("mary").
is_name("boats").

:- pred is_trans(string::in,number::out,string::out) is semidet.
is_trans("likes",singular,"like").
is_trans("like",plural,"like").

:- pred is_intrans(string::in,number::out,string::out) is semidet.
is_intrans("live",plural,"live").
