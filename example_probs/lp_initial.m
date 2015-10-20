:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is failure.
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is multi.

:- pred clause(string::out) is nondet.

:- pred neglit(string::in,atom::in) is failure.
:- pred poslit(string::in,atom::in) is failure.

:- pred objective(atom::in,float::out) is semidet.

:- pred once_only(atom::in) is semidet.
%----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.

%----------------------------------------------------------------------%

% define atom type

:- type person == int.
:- type position ---> faculty ; facultyadjunct ; facultyaffiliate.
:- type project == int.
:- type course == int.
:- type title == int.
:- type phase ---> prequals ; postgenerals ; postquals.
:- type quarter ---> winter0304 ; autumn0304 ; spring0203 ; winter0203 ; autumn0203 ; spring0102 ; winter0102 ; autumn0001 ; winter0001 ; spring0001 ; autumn0102 ; spring0304.
:- type level ---> level400 ; level500 ; level100.


:- type atom ---> advisedBy(person,person)
	; c1(person,person)
	; c2(course,person,quarter,person)
	; c3(person,person)
	; c4(person,person)
	; c5(person,person,title)
	; c6(person,person)
	; c7(person,person)
	; c8(person)
	; c9(person,person,person)
	; c10(person,person)
	; c11(course,person,quarter,person)
	; c12(course,person,quarter,person)
	; c13(course,person,quarter,person)
	; c14(course,person,quarter,person)
	; c15(course,person,quarter,person)
	; c16(course,person,quarter,person)
	; c17(title,person,person)
	; c18(person,person,title)
	; c19(title,person,person)
	; c20(person,person)
	; c21(person,person)
	; c22(person,person)
	; c23(person,person)
	; c24(person,person,person).

objective(c1(_,_),1.94203).
objective(c2(_,_,_,_),0.0732856).
objective(c3(_,_),2.38127).
objective(c4(_,_),0.118837).
objective(c5(_,_,_),0.0302834).
objective(c6(_,_),2.38127).
objective(c7(_,_),1.27773).
objective(c8(_),0.671981).
objective(c9(_,_,_),2.01213).
objective(c10(_,_),0.326654).
objective(c11(_,_,_,_),0.000635066).
objective(c12(_,_,_,_),0.112133).
objective(c13(_,_,_,_),0.0518195).
objective(c14(_,_,_,_),0.000634612).
objective(c15(_,_,_,_),0.145903).
objective(c16(_,_,_,_),0.095052).
objective(c17(_,_,_),0.749123).
objective(c18(_,_,_),0.0302834).
objective(c19(_,_,_),0.337329).
objective(c20(_,_),0.515549).
objective(c21(_,_),0.954782).
objective(c22(_,_),2.89681).
%objective(c23(_,_),0.709057).
objective(c23(_,_),2.0*0.709057).
%objective(c24(_,_,_),0.384788).
objective(c24(_,_,_),2.0*0.384788).

clause(_,_,_) :- fail.
clause(_) :- fail.
neglit(_,_) :- fail.
poslit(_,_) :- fail.

% all ground atoms indicating broken clauses
% occur in only one ground clause
% they are easily identified by having an objective defined for them.
once_only(Atom) :- objective(Atom,_).

%-1.94203  student(a1) v !advisedBy(a2,a1)
% so each grounding where a1 is a student generates a penalty of -1.94203 irrespective of model
% so just worry about cases where a1 is not a student
initial_clause("1") -->
	{person(A1), not student(A1), person(A2)},
	initial_poslit(advisedBy(A2,A1)),
	initial_poslit(c1(A1,A2)).

%-0.0732856  !taughtBy(a1,a2,a3) v !courseLevel(a1,"Level500") v advisedBy(a4,a2) v tempAdvisedBy(a4,a2) v !ta(a1,a4,a3)
% each grounding of a1,a2,a3,a4 where !taughtBy(a1,a2,a3) v !courseLevel(a1,"Level500") v tempAdvisedBy(a4,a2) v !ta(a1,a4,a3) generates a penalty irrespective of model so just worry about other cases.
initial_clause("2") -->
	{person(A2), person(A4),
	 not tempAdvisedBy(A4,A2), taughtBy(A1,A2,A3),
	 courseLevel(A1,level500), ta(A1,A4,A3)
	},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c2(A1,A2,A3,A4)).


%-2.38127  professor(a1) v !advisedBy(a1,a2)
initial_clause("3") -->
	{person(A1), not professor(A1), person(A2)},
	initial_poslit(advisedBy(A1,A2)),
	initial_poslit(c3(A1,A2)).

%0.118837  !advisedBy(a1,a2) v !yearsInProgram(a1,"Year1")

initial_clause("4") -->
	{person(A1), person(A2), yearsInProgram(A1,1)},
	initial_neglit(advisedBy(A1,A2)),
	initial_poslit(c4(A1,A2)).


%-0.0302834  student(a1) v !student(a2) v !publication(a3,a1) v !publication(a3,a2) v advisedBy(a2,a1) v tempAdvisedBy(a2,a1)
initial_clause("5") -->
	{student(A2), person(A1), not student(A1), publication(A3,A2),
	 publication(A3,A1), not tempAdvisedBy(A2,A1)},
	initial_neglit(advisedBy(A2,A1)),	
	initial_poslit(c5(A1,A2,A3)).



%-2.38127  !student(a1) v advisedBy(a1,a2) v tempAdvisedBy(a1,a2)
initial_clause("6") -->
	{student(A1), person(A2), not tempAdvisedBy(A1,A2)},
	initial_neglit(advisedBy(A2,A1)),	
	initial_poslit(c6(A1,A2)).

%1.27773  !professor(a1) v !hasPosition(a1,"Faculty") v advisedBy(a2,a1) v tempAdvisedBy(a2,a1)
initial_clause("7") -->
	{professor(A1), hasPosition(A1,faculty),
	 person(A2), not tempAdvisedBy(A2,A1)},
	initial_poslit(advisedBy(A2,A1)),
	initial_poslit(c7(A1,A2)).

%0.671981  !advisedBy(a1,a1)
initial_clause("8") -->
	{person(A1)},
	initial_neglit(advisedBy(A1,A1)),
	initial_poslit(c8(A1)).


%2.01213  !advisedBy(a1,a2) v !tempAdvisedBy(a1,a3)
initial_clause("9") -->
	{person(A1), person(A2),tempAdvisedBy(A1,A3)},
	initial_neglit(advisedBy(A1,A2)),
	initial_poslit(c9(A1,A2,A3)).


%0.326654  !advisedBy(a1,a2) v !inPhase(a1,"PreQuals")
initial_clause("10") -->
	{person(A1), person(A2), inPhase(A1,prequals)},
	initial_neglit(advisedBy(A1,A2)),
	initial_poslit(c10(A1,A2)).


%-0.000635066  !taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v advisedBy(a4,a2) v !inPhase(a4,"PostQuals") v !ta(a1,a4,a3)
initial_clause("11") -->
	{person(A4), person(A2), inPhase(A4,postquals), ta(A1,A4,A3),
	 not courseLevel(A1,level100),
	 taughtBy(A1,A2,A3)},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c11(A1,A2,A3,A4)).


% the following could be folded into initial_clause 11 !!
%0.112133  !taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v !advisedBy(a4,a2) v !inPhase(a4,"PostQuals") v ta(a1,a4,a3)
initial_clause("12") -->
	{person(A4), person(A2), inPhase(A4,postquals),
	 taughtBy(A1,A2,A3), 
	 not courseLevel(A1,level100),
	 not ta(A1,A4,A3)
	},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c12(A1,A2,A3,A4)).


%0.0518195  taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v !advisedBy(a4,a2) v !inPhase(a4,"PostQuals") v !ta(a1,a4,a3)
initial_clause("13") -->
	{person(A4), person(A2), inPhase(A4,postquals), ta(A1,A4,A3),
	 not courseLevel(A1,level100),
	 not taughtBy(A1,A2,A3)},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c13(A1,A2,A3,A4)).

%-0.000634612  !taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v advisedBy(a4,a2) v !inPhase(a4,"PostGenerals") v !ta(a1,a4,a3)

initial_clause("14") -->
	{person(A2), person(A4), inPhase(A4,postgenerals), ta(A1,A4,A3),
	 not courseLevel(A1,level100),
	 taughtBy(A1,A2,A3)},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c14(A1,A2,A3,A4)).

%0.145903  !taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v !advisedBy(a4,a2) v !inPhase(a4,"PostGenerals") v ta(a1,a4,a3)
initial_clause("15") -->
	{
	 person(A4), person(A2),
	 inPhase(A4,postgenerals),
	 taughtBy(A1,A2,A3),
	 not ta(A1,A4,A3),
	 not courseLevel(A1,level100)
	},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c15(A1,A2,A3,A4)).


%0.095052  taughtBy(a1,a2,a3) v courseLevel(a1,"Level100") v !advisedBy(a4,a2) v !inPhase(a4,"PostGenerals") v !ta(a1,a4,a3)

initial_clause("16") -->
	{
	 person(A4), person(A2),
	 inPhase(A4,postgenerals),
	 ta(A1,A4,A3),
	 not courseLevel(A1,level100),
	 not taughtBy(A1,A2,A3)
	},
	initial_neglit(advisedBy(A4,A2)),
	initial_poslit(c16(A1,A2,A3,A4)).

%0.749123  !publication(a1,a2) v !publication(a1,a3) v advisedBy(a2,a3) v advisedBy(a3,a2) v samePerson(a2,a3)

initial_clause("17") -->
	{
	 publication(A1,A2),
	 publication(A1,A3),
	 not A2 = A3
	},
	initial_poslit(advisedBy(A2,A3)),
	initial_poslit(advisedBy(A3,A2)),
	initial_poslit(c17(A1,A2,A3)).


%-0.0302834  !professor(a1) v !student(a2) v !publication(a3,a1) v !publication(a3,a2) v advisedBy(a2,a1) v samePerson(a1,a2)
initial_clause("18") -->
	{
	 professor(A1),
	 student(A2),
	 not A1 = A2,
	 publication(A3,A1),
	 publication(A3,A2)
	},
	initial_neglit(advisedBy(A2,A1)),
	initial_poslit(c18(A1,A2,A3)).


%-0.337329  publication(a1,a2) v !publication(a1,a3) v !advisedBy(a3,a2)
initial_clause("19") -->
	{
	 publication(A1,A3),
	 person(A2),
	 not publication(A1,A2)
	},
	initial_poslit(advisedBy(A3,A2)),
	initial_poslit(c19(A1,A2,A3)).


%0.515549  student(a1) v !advisedBy(a1,a2)
initial_clause("20") -->
	{
	 person(A2), person(A1),
	 not student(A1)
	},
	initial_neglit(advisedBy(A1,A2)),
	initial_poslit(c20(A1,A2)).

%0.954782  professor(a1) v !advisedBy(a2,a1)
initial_clause("21") -->
	{
	 person(A2), person(A1),
	 not professor(A1)
	},
	initial_neglit(advisedBy(A2,A1)),
	initial_poslit(c21(A1,A2)).

%-2.89681  advisedBy(a1,a2)
initial_clause("22") -->
	{person(A1), person(A2)},
	initial_neglit(advisedBy(A1,A2)),
	initial_poslit(c22(A1,A2)).

%0.709057  !advisedBy(a1,a2) v !advisedBy(a2,a1)
% cheat by doubling penalty and reducing number of instances
initial_clause("23") -->
	{person(A1), person(A2), A1 @< A2},
	initial_neglit(advisedBy(A1,A2)),
	initial_neglit(advisedBy(A2,A1)),
	initial_poslit(c23(A1,A2)).

%0.384788  !advisedBy(a1,a2) v !advisedBy(a1,a3) v samePerson(a2,a3)
% cheat by doubling penalty and reducing number of instances
initial_clause("24") -->
	{ person(A1), person(A2), person(A3), A2 @< A3 },
	initial_neglit(advisedBy(A1,A2)),
	initial_neglit(advisedBy(A1,A3)),
	initial_poslit(c24(A1,A2,A3)).


% Rockit 'observed predicates' are just predicates in the Mercury code
% and their atoms are not translated to SCIP variables

% predicates expressing data are more efficiently processed if in a Mercury
% fact table

% next predicate is an addition to generate people.
	
:- pred person(person::out) is multi.

person(X) :- professor(X).
person(X) :- student(X).
	
:- pred hasPosition(person,position).
:- mode hasPosition(out,out) is multi.

hasPosition(292, facultyaffiliate).
hasPosition(293, facultyaffiliate).
hasPosition(240, faculty).
hasPosition(211, faculty).
hasPosition(150, faculty).
hasPosition(415, faculty).
hasPosition(79, faculty).
hasPosition(349, facultyadjunct).
hasPosition(7, facultyadjunct).
hasPosition(319, faculty).
hasPosition(185, facultyadjunct).
hasPosition(171, faculty).
hasPosition(168, faculty).
hasPosition(407, faculty).

:- pred sameProject(project,project).
:- mode sameProject(in,in) is semidet.
sameProject(X,X).

:- pred sameCourse(course,course).
:- mode sameCourse(in,in) is semidet.
sameCourse(X,X).

:- pred samePerson(person,person).
:- mode samePerson(in,in) is semidet.
samePerson(X,X).


:- pred professor(person).
:- mode professor(in) is semidet.
:- mode professor(out) is multi.

professor(319).
professor(292).
professor(293).
professor(240).
professor(211).
professor(150).
professor(415).
professor(79).
professor(349).
professor(7).
professor(185).
professor(171).
professor(168).
professor(407).

:- pred student(person).
:- mode student(in) is semidet.
:- mode student(out) is multi.

student(284).
student(311).
student(14).
student(275).
student(259).
student(139).
student(176).
student(400).
student(318).
student(161).
student(347).
student(408).
student(265).
student(70).
student(381).
student(382).
student(333).
student(94).
student(272).
student(37).
student(353).
student(432).
student(377).
student(239).
student(13).
student(286).
student(412).
student(418).
student(320).
student(42).
student(20).
student(352).
student(276).
student(45).
student(233).
student(148).
student(193).
student(314).
student(21).
student(262).
student(257).
student(73).
student(380).
student(384).
student(406).
student(266).
student(312).
student(208).
student(63).
student(83).
student(271).
student(392).
student(420).
student(86).

:- pred publication(title,person).
:- mode publication(in,in) is semidet.
:- mode publication(in,out) is nondet.
:- mode publication(out,in) is nondet.
:- mode publication(out,out) is multi.

publication(25 , 284).
publication(284 , 14).
publication(110 , 14).
publication(118 , 14).
publication(71 , 14).
publication(316 , 14).
publication(118 , 318).
publication(217 , 161).
publication(55 , 161).
publication(331 , 161).
publication(250 , 161).
publication(268 , 161).
publication(271 , 161).
publication(171 , 161).
publication(120 , 347).
publication(86 , 347).
publication(338 , 347).
publication(224 , 347).
publication(260 , 347).
publication(112 , 347).
publication(97 , 347).
publication(50 , 292).
publication(103 , 292).
publication(166 , 292).
publication(72 , 292).
publication(47 , 292).
publication(41 , 292).
publication(40 , 293).
publication(13 , 240).
publication(140 , 240).
publication(217 , 240).
publication(92 , 240).
publication(167 , 240).
publication(331 , 240).
publication(26 , 240).
publication(275 , 240).
publication(333 , 240).
publication(270 , 240).
publication(208 , 240).
publication(103 , 240).
publication(268 , 240).
publication(340 , 240).
publication(192 , 240).
publication(54 , 240).
publication(177 , 240).
publication(33 , 240).
publication(10 , 240).
publication(84 , 240).
publication(161 , 240).
publication(248 , 240).
publication(102 , 240).
publication(274 , 240).
publication(47 , 240).
publication(0 , 240).
publication(82 , 240).
publication(337 , 240).
publication(344 , 240).
publication(254 , 240).
publication(119 , 240).
publication(114 , 211).
publication(259 , 211).
publication(59 , 211).
publication(160 , 211).
publication(88 , 211).
publication(24 , 211).
publication(323 , 211).
publication(190 , 211).
publication(11 , 211).
publication(199 , 211).
publication(240 , 211).
publication(335 , 211).
publication(241 , 211).
publication(212 , 211).
publication(228 , 211).
publication(345 , 211).
publication(89 , 211).
publication(165 , 211).
publication(113 , 211).
publication(233 , 211).
publication(132 , 211).
publication(310 , 211).
publication(218 , 211).
publication(71 , 211).
publication(341 , 211).
publication(207 , 211).
publication(229 , 211).
publication(292 , 211).
publication(49 , 211).
publication(238 , 211).
publication(255 , 211).
publication(329 , 211).
publication(79 , 211).
publication(325 , 211).
publication(44 , 211).
publication(25 , 211).
publication(118 , 150).
publication(140 , 415).
publication(12 , 415).
publication(182 , 415).
publication(122 , 415).
publication(208 , 415).
publication(103 , 415).
publication(347 , 415).
publication(266 , 415).
publication(340 , 415).
publication(269 , 415).
publication(5 , 415).
publication(70 , 415).
publication(179 , 415).
publication(29 , 415).
publication(72 , 415).
publication(47 , 415).
publication(0 , 415).
publication(38 , 415).
publication(290 , 415).
publication(63 , 415).
publication(82 , 415).
publication(283 , 415).
publication(337 , 415).
publication(94 , 415).
publication(147 , 415).
publication(329 , 415).
publication(297 , 415).
publication(79 , 415).
publication(312 , 415).
publication(107 , 415).
publication(273 , 415).
publication(172 , 415).
publication(295 , 415).
publication(41 , 415).
publication(325 , 415).
publication(44 , 415).
publication(87 , 415).
publication(222 , 415).
publication(236 , 415).
publication(258 , 415).
publication(301 , 415).
publication(318 , 79).
publication(115 , 79).
publication(231 , 79).
publication(226 , 79).
publication(195 , 79).
publication(162 , 185).
publication(178 , 171).
publication(225 , 171).
publication(269 , 171).
publication(150 , 171).
publication(70 , 171).
publication(63 , 171).
publication(94 , 171).
publication(147 , 171).
publication(170 , 171).
publication(125 , 171).
publication(90 , 171).
publication(114 , 407).
publication(12 , 407).
publication(259 , 407).
publication(217 , 407).
publication(92 , 407).
publication(182 , 407).
publication(59 , 407).
publication(160 , 407).
publication(55 , 407).
publication(88 , 407).
publication(167 , 407).
publication(24 , 407).
publication(323 , 407).
publication(331 , 407).
publication(190 , 407).
publication(120 , 407).
publication(250 , 407).
publication(11 , 407).
publication(284 , 407).
publication(199 , 407).
publication(240 , 407).
publication(335 , 407).
publication(270 , 407).
publication(241 , 407).
publication(212 , 407).
publication(110 , 407).
publication(268 , 407).
publication(228 , 407).
publication(347 , 407).
publication(266 , 407).
publication(192 , 407).
publication(345 , 407).
publication(5 , 407).
publication(271 , 407).
publication(89 , 407).
publication(165 , 407).
publication(113 , 407).
publication(233 , 407).
publication(179 , 407).
publication(132 , 407).
publication(177 , 407).
publication(310 , 407).
publication(171 , 407).
publication(33 , 407).
publication(218 , 407).
publication(71 , 407).
publication(341 , 407).
publication(207 , 407).
publication(229 , 407).
publication(292 , 407).
publication(316 , 407).
publication(49 , 407).
publication(38 , 407).
publication(238 , 407).
publication(283 , 407).
publication(255 , 407).
publication(224 , 407).
publication(260 , 407).
publication(297 , 407).
publication(312 , 407).
publication(273 , 407).
publication(25 , 407).
publication(258 , 407).
publication(118 , 408).
publication(118 , 353).
publication(40 , 239).
publication(13 , 13).
publication(26 , 13).
publication(275 , 13).
publication(333 , 13).
publication(54 , 13).
publication(10 , 13).
publication(84 , 13).
publication(161 , 13).
publication(248 , 13).
publication(344 , 13).
publication(50 , 352).
publication(208 , 352).
publication(103 , 352).
publication(166 , 352).
publication(314 , 352).
publication(47 , 352).
publication(86 , 352).
publication(82 , 352).
publication(79 , 352).
publication(261 , 352).
publication(87 , 352).
publication(329 , 45).
publication(79 , 45).
publication(325 , 45).
publication(44 , 45).
publication(150 , 148).
publication(125 , 148).
publication(90 , 148).
publication(162 , 193).
publication(170 , 314).
publication(107 , 314).
publication(172 , 314).
publication(295 , 314).
publication(222 , 314).
publication(301 , 314).
publication(25 , 21).
publication(122 , 262).
publication(314 , 262).
publication(29 , 262).
publication(72 , 262).
publication(290 , 262).
publication(86 , 262).
publication(261 , 262).
publication(41 , 262).
publication(102 , 257).
publication(274 , 257).
publication(254 , 257).
publication(119 , 257).
publication(269 , 73).
publication(63 , 73).
publication(318 , 380).
publication(115 , 380).
publication(231 , 380).
publication(226 , 380).
publication(195 , 380).
publication(314 , 406).
publication(86 , 406).
publication(261 , 406).
publication(118 , 208).
publication(182 , 63).
publication(178 , 63).
publication(225 , 63).
publication(5 , 63).
publication(314 , 63).
publication(86 , 63).
publication(147 , 63).
publication(261 , 63).
publication(97 , 63).
publication(222 , 63).
publication(236 , 63).
publication(301 , 63).
publication(325 , 83).

:- pred yearsInProgram(person,int).
:- mode yearsInProgram(out,in) is nondet.
:- mode yearsInProgram(in,in) is semidet.

yearsInProgram(408, 2).
yearsInProgram(265, 9).
yearsInProgram(70, 1).
yearsInProgram(381, 10).
yearsInProgram(139, 3).
yearsInProgram(382, 3).
yearsInProgram(333, 2).
yearsInProgram(94, 1).
yearsInProgram(176, 2).
yearsInProgram(272, 2).
yearsInProgram(37, 1).
yearsInProgram(353, 4).
yearsInProgram(432, 5).
yearsInProgram(377, 1).
yearsInProgram(239, 4).
yearsInProgram(13, 7).
yearsInProgram(286, 3).
yearsInProgram(412, 3).
yearsInProgram(418, 3).
yearsInProgram(14, 10).
yearsInProgram(320, 3).
yearsInProgram(42, 1).
yearsInProgram(20, 1).
yearsInProgram(352, 5).
yearsInProgram(276, 3).
yearsInProgram(45, 5).
yearsInProgram(233, 1).
yearsInProgram(148, 5).
yearsInProgram(193, 1).
yearsInProgram(314, 4).
yearsInProgram(275, 5).
yearsInProgram(21, 5).
yearsInProgram(262, 7).
yearsInProgram(257, 7).
yearsInProgram(73, 4).
yearsInProgram(380, 6).
yearsInProgram(384, 3).
yearsInProgram(406, 5).
yearsInProgram(266, 5).
yearsInProgram(312, 4).
yearsInProgram(208, 4).
yearsInProgram(311, 3).
yearsInProgram(63, 5).
yearsInProgram(318, 5).
yearsInProgram(83, 5).
yearsInProgram(161, 7).
yearsInProgram(284, 3).

:- pred inPhase(person,phase).
:- mode inPhase(in,in) is semidet.

inPhase(408, prequals).
inPhase(265, postgenerals).
inPhase(70, prequals).
inPhase(381, postgenerals).
inPhase(139, postquals).
inPhase(382, postquals).
inPhase(333, prequals).
inPhase(94, prequals).
inPhase(176, postquals).
inPhase(272, postquals).
inPhase(37, prequals).
inPhase(353, postquals).
inPhase(432, postquals).
inPhase(377, prequals).
inPhase(239, postquals).
inPhase(13, postgenerals).
inPhase(286, postquals).
inPhase(412, postquals).
inPhase(418, postquals).
inPhase(14, postgenerals).
inPhase(320, postquals).
inPhase(42, prequals).
inPhase(20, prequals).
inPhase(352, postgenerals).
inPhase(276, prequals).
inPhase(45, postgenerals).
inPhase(233, prequals).
inPhase(148, postquals).
inPhase(193, prequals).
inPhase(314, postgenerals).
inPhase(275, postgenerals).
inPhase(21, postgenerals).
inPhase(262, postgenerals).
inPhase(257, postgenerals).
inPhase(73, postquals).
inPhase(380, postgenerals).
inPhase(384, postquals).
inPhase(406, postgenerals).
inPhase(266, postquals).
inPhase(312, prequals).
inPhase(208, postquals).
inPhase(311, postquals).
inPhase(63, postgenerals).
inPhase(318, prequals).
inPhase(83, postquals).
inPhase(161, postgenerals).
inPhase(284, postquals).

:- pred ta(course,person,quarter).
:- mode ta(in,in,in) is semidet.
:- mode ta(in,out,in) is nondet.
:- mode ta(out,in,out) is nondet.

ta(52, 70, winter0304).
ta(44, 193, winter0304).
ta(128, 271, winter0304).
ta(128, 392, winter0304).
ta(44, 377, autumn0304).
ta(24, 70, autumn0304).
ta(156, 257, autumn0304).
ta(132, 94, autumn0304).
ta(24, 21, spring0203).
ta(44, 420, winter0203).
ta(44, 382, winter0203).
ta(141, 14, winter0203).
ta(12, 21, winter0203).
ta(44, 286, autumn0203).
ta(52, 318, spring0102).
ta(44, 382, spring0102).
ta(44, 86, spring0102).
ta(50, 314, spring0102).
ta(39, 73, spring0102).
ta(82, 381, winter0102).

:- pred taughtBy(course,person,quarter).
:- mode taughtBy(in,in,in) is semidet.
:- mode taughtBy(out,out,out) is multi.

taughtBy(44, 171, autumn0001).
taughtBy(24, 240, autumn0001).
taughtBy(12, 211, autumn0001).
taughtBy(123, 150, autumn0001).
taughtBy(44, 293, winter0001).
taughtBy(143, 211, winter0001).
taughtBy(50, 171, winter0001).
taughtBy(170, 79, winter0001).
taughtBy(15, 292, winter0001).
taughtBy(32, 319, winter0001).
taughtBy(158, 240, winter0001).
taughtBy(24, 150, spring0001).
taughtBy(52, 168, spring0001).
taughtBy(16, 240, spring0001).
taughtBy(173, 171, spring0001).
taughtBy(64, 79, spring0001).
taughtBy(44, 171, autumn0102).
taughtBy(24, 211, autumn0102).
taughtBy(156, 240, autumn0102).
taughtBy(12, 79, autumn0102).
taughtBy(143, 407, winter0102).
taughtBy(170, 211, winter0102).
taughtBy(44, 415, spring0102).
taughtBy(24, 240, spring0102).
taughtBy(52, 168, spring0102).
taughtBy(50, 171, spring0102).
taughtBy(39, 415, spring0102).
taughtBy(123, 150, spring0102).
taughtBy(76, 319, spring0102).
taughtBy(44, 171, autumn0203).
taughtBy(24, 240, autumn0203).
taughtBy(44, 415, winter0203).
taughtBy(52, 168, winter0203).
taughtBy(141, 150, winter0203).
taughtBy(12, 211, winter0203).
taughtBy(16, 79, winter0203).
taughtBy(24, 211, spring0203).
taughtBy(170, 407, spring0203).
taughtBy(15, 292, spring0203).
taughtBy(168, 240, spring0203).
taughtBy(64, 79, spring0203).
taughtBy(44, 171, autumn0304).
taughtBy(24, 79, autumn0304).
taughtBy(156, 240, autumn0304).
taughtBy(12, 407, autumn0304).
taughtBy(76, 319, autumn0304).
taughtBy(44, 415, winter0304).
taughtBy(57, 150, winter0304).
taughtBy(52, 168, winter0304).
taughtBy(170, 79, winter0304).
taughtBy(24, 407, spring0304).
taughtBy(50, 171, spring0304).
taughtBy(158, 240, spring0304).
taughtBy(7, 415, spring0304).

:- pred tempAdvisedBy(person,person).
:- mode tempAdvisedBy(in,in) is semidet.
:- mode tempAdvisedBy(in,out) is nondet.

tempAdvisedBy(408, 150).
tempAdvisedBy(382, 415).
tempAdvisedBy(333, 211).
tempAdvisedBy(94, 79).
tempAdvisedBy(377, 292).
tempAdvisedBy(412, 168).
tempAdvisedBy(42, 150).
tempAdvisedBy(20, 240).
tempAdvisedBy(233, 319).
tempAdvisedBy(193, 415).
tempAdvisedBy(284, 211).

:- pred projectMember(person,person).
:- mode projectMember(in,in) is semidet.

projectMember(62, 319).


:- pred courseLevel(course,level).
:- mode courseLevel(in,in) is semidet.

courseLevel(52, level400).
courseLevel(44, level400).
courseLevel(24, level400).
courseLevel(128, level400).
courseLevel(57, level400).
courseLevel(82, level400).
courseLevel(143, level400).
courseLevel(50, level500).
courseLevel(156, level500).
courseLevel(141, level500).
courseLevel(12, level500).
courseLevel(170, level500).
courseLevel(65, level500).
courseLevel(123, level500).
courseLevel(173, level500).
courseLevel(86, level500).
courseLevel(131, level500).
courseLevel(85, level500).
courseLevel(64, level500).
courseLevel(168, level500).
courseLevel(158, level500).
courseLevel(132, level500).
courseLevel(76, level500).
courseLevel(16, level500).
courseLevel(15, level500).
courseLevel(39, level500).
courseLevel(32, level500).
courseLevel(7, level500).
courseLevel(134, level500).
courseLevel(135, level500).
