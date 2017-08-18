:- dynamic problem_var/3.
:- dynamic nvars/1.
:- dynamic ncons/1.
:- dynamic cons_num/2.

write_conss :-
    eq_clause(ResLit,Type,Lits),
    write_propcons2(Type,ResLit,Lits),
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
    nvars(N),
    format('  Variables        : ~w (~w binary, 0 integer, 0 implicit integer, 0 continuous)~n',[N,N]),
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
write_vars.

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

lit2ipvar(lit(_,Term),IPVar) :-
    problem_var(Term,IPVar,_),
    !.
lit2ipvar(lit(_,Term),IPVar) :-
    term_to_atom(Term,Atom),
    atomic_concat('x#',Atom,IPVar),
    (objective(Term,Val) -> Obj = Val; Obj = 0.0),
    assert(problem_var(Term,IPVar,Obj)),
    inc_nvars.

constraint_num(Type,Num) :-
    retract(cons_num(Type,N)),
    !,
    Num is N+1,
    assert(cons_num(Type,Num)).
constraint_num(Type,1) :-
    assert(cons_num(Type,1)).
