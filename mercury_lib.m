%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module mercury_lib.
:- interface.

:- import_module io.
:- import_module int.
:- import_module list.
%-----------------------------------------------------------------------------%



%:- type atom == int.
:- type intlist.

:- pragma foreign_type("C", intlist, "int*").

:- type atom ---> bob ; jane.

    % Write "Hello World" to the current Mercury text output stream.
    %
:- pred write_hello(int::in, int::out, io::di, io::uo) is det.

    % Write the current value of the mutable `global' to the current
    % Mercury text output stream.
    %
:- pred write_global_value(io::di, io::uo) is det.
:- impure pred makevars(io::di, io::uo) is det.

:- pred get_atom(atom::out) is det.
:- pred write_atom(atom::in, io::di, io::uo) is det.

:- pred foo(intlist::in,int::in,int::out) is det.

:- pred all_foos(list(atom)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module string.
:- import_module array.


%-----------------------------------------------------------------------------%

:- mutable(global, int, 561, ground, [untrailed,
    foreign_name("C", "GLOBAL"), attach_to_io_state]).


:- mutable(atomarray, array(int), array.make_empty_array, ground, [untrailed]).

:- mutable(atomlist, list(atom), [], ground, [untrailed, attach_to_io_state]). 

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", all_foos(out), "foo_list").

all_foos([bob,jane,bob]).

/* this has to call some foreign_proc to get hold of the intlist */
:- pragma foreign_export("C", foo(in,in,out),
			 "foo").
foo(X,Y,Z) :- loku(X,Y,Z).

:- pred loku(intlist::in,int::in,int::out) is det.

:- pragma foreign_proc("C",
    loku(List::in, Index::in, Val::out),
    [will_not_call_mercury, promise_pure],
"
Val = List[Index];		    
"
		   ). 
:- pragma foreign_export("C", makevars(di,uo),
    "makevars").

makevars(!IO) :-
	semipure get_atomlist(L),
	list.append(L,[bob,jane],L1),
	impure set_atomlist(L1),
	io.write(L1,!IO).
	

:- pragma foreign_export("C", write_hello(in, out, di, uo),
    "write_hello").

write_hello(X,Y,!IO) :-
    io.write_int(X, !IO),
    Y = X+1,
    io.write_string(" Hello World\n", !IO).

:- pragma foreign_export("C", write_global_value(di, uo),
    "write_global_value").

write_global_value(!IO) :-
    get_global(Value, !IO),
    io.format("The new value of global is %d.\n", [i(Value)], !IO).

 :- pragma foreign_export("C", get_atom(out),
     "get_atom").

get_atom(bob).

:- pragma foreign_export("C", write_atom(in, di, uo),
    "write_atom").

write_atom(X,!IO) :-
	io.write(X,!IO),	io.write(X,!IO),
	io.nl(!IO).



%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO) :-
    io.write_string("mercury_lib: the initialiser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.

finaliser(!IO) :-
    io.write_string("mercury_lib: the finaliser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
