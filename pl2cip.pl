:- dynamic problem_var/3.
:- dynamic problem_var/6.
:- dynamic nvars/1.
:- dynamic nvars/2.
:- dynamic ncons/1.
:- dynamic cons_num/2.

:- dynamic objective/2.
:- dynamic lb/2.
:- dynamic ub/2.
:- dynamic vartype/2.


    

write_conss :-
    eq_clause(ResLit,Type,Lits),
    write_propcons2(Type,ResLit,Lits),
    fail.
write_conss :-
    linear(LHS,Coeffs,Vars,RHS),
    write_linear(LHS,Coeffs,Vars,RHS),
    fail.
write_conss :-
    eq_quad(X,XX),
    write_eq_quad(X,XX),
    fail.
write_conss.

write_cip(_OutFile) :-
    %open(tmp,write,S),
    %set_stream(S,alias(user_output)),
    tell(tmp),
    format('CONSTRAINTS~n'),
    write_conss,
    format('END~n'),
    %close(S),
    told,
    fail.
write_cip(OutFile) :-
    %open(OutFile,write,S),
    %set_stream(S,alias(user_output)),
    tell(tmp2),
    format('STATISTICS~n'),
    format('  Problem name     : ~w~n',[OutFile]),
    nvars(N),    nvars(binary,NB),    nvars(integer,NI),
    format('  Variables        : ~w (~w binary, ~w integer, 0 implicit integer, 0 continuous)~n',[N,NB,NI]),
    ncons(M),
    format('  Constraints      : 0 initial, ~w maximal~n',[M]),
    format('OBJECTIVE~n'),
    format('  Sense            : minimize~n'),
    format('VARIABLES~n'),
    write_vars,
    %close(S).
    told,
    format(atom(Cmd),'cat tmp2 tmp > ~w.cip', [OutFile]),
    shell(Cmd),
    shell('rm tmp'),
    shell('rm tmp2').

write_vars :-
    problem_var(_Term,IPVar,Obj),
    format('  [binary] <~w>: obj=~w, original bounds=[0,1]~n',[IPVar,Obj]),
    fail.
write_vars :-
    problem_var(_Term,IPVar,Obj,Type,Lb,Gb),
    format('  [~w] <~w>: obj=~w, original bounds=[~w,~w]~n',[Type,IPVar,Obj,Lb,Gb]),
    fail.
write_vars.


write_linterms([],[]).
write_linterms([0.0|CT],[_Term|TT]) :-
    !,
    write_linterms(CT,TT).
write_linterms([Coeff|CT],[Term|TT]) :-
    (Coeff > 0 -> format('+'); true),
    lit2ipvar(Term,IPVar),
    (problem_var(Term,IPVar,_,_,_,_)
     -> Type = 'I';
     Type = 'B'),
    format('~w<~w>[~w] ',[Coeff,IPVar,Type]),
    write_linterms(CT,TT).

% equality linear constraint
write_linear(LHS,Coeffs,Vars,LHS) :-
    constraint_num(linear,Num),
    format('  [linear] <linear_cons_~w>: ',[Num]),
    write_linterms(Coeffs,Vars),
    format(' == ~w;~n',[LHS]),
    inc_ncons.
    
% constraint that integer variable XX is the square of X
write_eq_quad(X,XX) :-
    constraint_num(quad,Num),
    lit2ipvar(X,XIPVar),
    lit2ipvar(XX,XXIPVar),
    % assume both are integer types
    format('  [quadratic] <quad_cons_~w>: -<~w>[I] +<~w>[I]^2 == 0;~n',[Num,XXIPVar,XIPVar]),
    inc_ncons.
    
% for writing constraints where lits are all the same type of argument
% for, e.g. logicor constraints
write_propcons(Type,Lits) :-
    constraint_num(Type,Num),
    format('  [~w] <~w_cons_~w>: ~w(',[Type,Type,Num,Type]),
    write_lits(Lits),
    format(');~n'),
    inc_ncons,!.

% for writing constraints where one lit is the 'result' of the others
% for, e.g. and constraints
write_propcons2(Type,ResLit,Lits) :-
    constraint_num(Type,Num),
    format('  [~w] <~w_cons_~w>: ',[Type,Type,Num]),
    (neglit(ResLit) -> format('~~'); true),
    lit2ipvar(ResLit,IPVar),
    format('<~w> == ~w(',[IPVar,Type]),
    write_lits(Lits),
    format(');~n'),
    inc_ncons,!.

inc_ncons :-
    retract(ncons(N)),
    !,
    NN is N+1,
    assert(ncons(NN)).
inc_ncons :-
    assert(ncons(1)).

inc_nvars :-
    retract(nvars(N)),
    !,
    NN is N+1,
    assert(nvars(NN)).
inc_nvars :-
    assert(nvars(1)).

inc_nvars(Type) :-
    retract(nvars(Type,N)),
    !,
    NN is N+1,
    assert(nvars(Type,NN)).
inc_nvars(Type) :-
    assert(nvars(Type,1)).


write_lits([Lit]) :-
    !,
    write_lit(Lit).
write_lits([Lit|Lits]) :-
    write_lit(Lit),
    format(','),
    write_lits(Lits).

write_lit(Lit) :-
    format('<'),
    (neglit(Lit) -> format('~~'); true),
    lit2ipvar(Lit,IPVar),
    format('~w>',[IPVar]).

neglit(lit(n,_)).

%Given a term of the form lit(_,Term),
%generate a string representation for the corresponding IP variable
%and store this mapping using asssert
lit2ipvar(lit(_,Term),IPVar) :-
    problem_var(Term,IPVar,_),
    !.
lit2ipvar(lit(_,Term),IPVar) :-
    !,
    term_to_atom(Term,Atom),
    atomic_concat('x#',Atom,IPVar),
    (objective(Term,Val) -> Obj = Val; Obj = 0.0),
    assert(problem_var(Term,IPVar,Obj)),
    inc_nvars(binary), inc_nvars.
% for terms which do not represent binary variables
lit2ipvar(Term,IPVar) :-
    problem_var(Term,IPVar,_Obj,_Type,_Lb,_Gb),
    !.
lit2ipvar(Term,IPVar) :-
    term_to_atom(Term,Atom),
    atomic_concat('x#',Atom,IPVar),
    (objective(Term,Val) -> Obj = Val; Obj = 0.0),
    (lb(Term,Val) -> Lb = Val; Lb = 0.0),
    (ub(Term,Val) -> Ub = Val; Ub = '+inf'),
    (vartype(Term,Val) -> Type = Val; Type = integer),
    assert(problem_var(Term,IPVar,Obj,Type,Lb,Ub)),
    inc_nvars(integer), inc_nvars.

constraint_num(Type,Num) :-
    retract(cons_num(Type,N)),
    !,
    Num is N+1,
    assert(cons_num(Type,Num)).
constraint_num(Type,1) :-
    assert(cons_num(Type,1)).

