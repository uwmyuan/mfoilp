:- module prob.
:- interface.

%----------------------------------------------------------------------%



:- import_module mercury_lib.

:- type atom.

:- pred atom(atom::out) is multi.
:- pred initial_constraint(lincons::out) is nondet.
:- pred delayed_constraint(atom,lincons).
:- mode delayed_constraint(in,out) is nondet.
:- mode delayed_constraint(out,out) is nondet.

:- func objective(atom) = float.
:- func lb(atom) = float.
:- func ub(atom) = float.
:- func vartype(atom) = vartype.

%----------------------------------------------------------------------%

:- implementation.

:- import_module list.

% define atom type

:- type protein ---> p1;p2.
:- type location ---> l1;l2.
:- type pc_id ---> pc1;pc2.
:- type enzyme_id ---> e1;e2.
:- type complex_id ---> c1;c2.
:- type phenotype_id ---> ph1;ph2.
:- type func_id ---> f1;f2.

:- type atom --->
	location(protein,location_id)
	; interaction(protein,protein)
	; protein_class(protein,pc_id)
	; enzyme(protein,enzyme_id)
	; complex(protin,complex_id)
	; phenotype(protein,phenotype_id)
	; function(protin,func_id)
	; c1_broken(protein,location_id,func_id)
	; c2_broken(protein,function_id,func_id)

% define atom-variable generator
	
:- pred protein(protein::out) is multi.
protein(p1).
protein(p2).

:- pred location_id(location_id::out) is multi.
location_id(l1).
location_id(l2).

:- pred pc_id(pc_id::out) is multi.
pc_id(pc1).
pc_id(pc2).

:- pred enzyme_id(enzyme_id::out) is multi.
enzyme_id(e1).
enzyme_id(e2).

:- pred complex_id(complex_id::out) is multi.
complex_id(c1).
complex_id(c2).

:- pred phenotype_id(phenotype_id::out) is multi.
phenotype_id(ph1).
phenotype_id(ph2).

:- pred func_id(func_id::out) is multi.
func_id(f1).
func_id(f2).

atom(location(Protein,Location_id)) :- protein(Protein), location_id(Location_id).
atom(interaction(Protein1,Protein2)) :- protein(Protein1), protein(Protein2).
	; interaction(protein,protein)
	; protein_class(protein,pc_id)
	; enzyme(protein,enzyme_id)
	; complex(protin,complex_id)
	; phenotype(protein,phenotype_id)
	; function(protin,func_id)
	; c1_broken(protein,location_id,func_id)
	; c2_broken(protein,function_id,func_id)




% provide properties for each atom-variable

objective(_Atom) = 50.5.
lb(_Atom) = 0.0.
ub(_Atom) = 1.0.
vartype(_Atom) = binary.

% constraints

initial_constraint(lincons(neginf,[1.0 * friends(X,Y), 1.0 * friends(X,Z)],finite(1.0)))  :-
	atom(friends(X,Y)), atom(friends(X,Z)), not Y=Z. 

% for delayed constraints an atom involved in the constraint can be
% given as input to speed up finding constraints involving particular atoms

delayed_constraint(Atom,lincons(neginf,[1.0 * friends(X,Y), 1.0 * friends(X,Z)],finite(1.0)))  :-
	(Atom=friends(X,Y) ; Atom = friends(X,Z)),
	atom(friends(X,Y)), atom(friends(X,Z)), not Y=Z. 




