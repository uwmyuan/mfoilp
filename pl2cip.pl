% for writing constraints where lits are all the same type of argument
% for, e.g. logicor constraints
write_propcons(Type,Lits) :-
    constraint_num(Type,Num),
    format('[~w] <~w_cons_~w>: ~w(',[Type,Type,Num,Type]),
    write_lits(Lits),
    format(')').

% for writing constraints where one lit is the 'result' of the others
% for, e.g. and constraints
write_propcons2(Type,ResLit,Lits) :-
    constraint_num(Type,Num),
    format('[~w] <~w_cons_~w>: ',[Type,Type,Num]),
    (neglit(ResLit) -> format('~~'); true),
    lit2ipvar(ResLit,IPVar),
    format('<~w> == ~w(',[IPVar,Type]),
    write_lits(Lits),
    format(')').


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
    term_to_atom(Term,Atom),
    atomic_concat('x#',Atom,IPVar).


constraint_num(Type,Num) :-
    retract(cons_num(Type,N)),
    !,
    Num is N+1,
    assert(cons_num(Type,Num)).
constraint_num(Type,1) :-
    assert(cons_num(Type,1)).
