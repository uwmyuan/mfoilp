% example input START

%-0.0732856  !courseLevel(a3,Level_500) v !taughtBy(a3,a2,a4) v !ta(a3,a1,a4) v tempAdvisedBy(a1,a2) v advisedBy(a1,a2)
eq_clause(lit(p,cb(1,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(1,A1,A2,[_A3,_A4]).

%1.94203  student(a2) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(2,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    person(A1),
    person(A2),
    \+ student(A2).

%2.38127  professor(a1) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(3,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    person(A1),
    \+ professor(A1),
    person(A2).

%0.118837  !yearsInProgram(a1,Year_1) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(4,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    yearsInProgram(A1,"Year_1"),
    person(A2).

%-0.0302834  !student(a1) v !publication(a3,a2) v !publication(a3,a1) v student(a2) v tempAdvisedBy(a1,a2) v advisedBy(a1,a2)
eq_clause(lit(p,cb(5,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(5,A1,A2,[_A3]).

%-2.38127  !student(a1) v tempAdvisedBy(a1,a2) v advisedBy(a1,a2)
eq_clause(lit(p,cb(6,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    student(A1),
    person(A2),
    \+ tempAdvisedBy(A1,A2).

%1.27773  !professor(a1) v !hasPosition(a1,Faculty) v tempAdvisedBy(a2,a1) v advisedBy(a2,a1)
eq_clause(lit(p,cb(7,A1,A2)),and,[lit(n,advisedBy(A1,A2))]) :-
  professor(A1),
  hasPosition(A1,"Faculty"),
  person(A2),
  \+ tempAdvisedBy(A2,A1).

%0.671981  !advisedBy(a1,a1)
eq_clause(lit(p,cb(8,A1)),and,[lit(p,advisedBy(A1,A1))]) :-
    person(A1).

%0.709057  !advisedBy(a1,a2) v !advisedBy(a2,a1)
eq_clause(lit(p,cb(9,A1,A2)),and,[lit(p,advisedBy(A1,A2)),lit(p,advisedBy(A2,A1))]) :-
    person(A1),
    person(A2),
    A1 @< A2.

eq_clause(lit(p,cb(9,A1)),and,[lit(p,advisedBy(A1,A1))]) :-
    person(A1).
    
%0.384788  samePerson(a2,a3) v !advisedBy(a1,a3) v !advisedBy(a1,a2)
%Following constraint replaced by a 'counting' constraint, see below.
%eq_clause(lit(p,cb(10,A1,A2,A3)),and,[lit(p,advisedBy(A1,A3)),lit(p,advisedBy(A1,A2))]) :-
%    person(A2),
%    person(A3),
%    A2 @< A3,
%    %\+ samePerson(A2,A3),
%    person(A1).

%2.01213  !tempAdvisedBy(a1,a3) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(11,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(11,A1,A2,[_A3]),
    person(A2).

%0.326654  !inPhase(a1,Pre_Quals) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(12,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    inPhase(A1,"Pre_Quals"),
    person(A2).

%-0.000635066  !inPhase(a1,Post_Quals) v !ta(a3,a1,a4) v !taughtBy(a3,a2,a4) v courseLevel(a3,Level_100) v advisedBy(a1,a2)
eq_clause(lit(p,cb(13,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
  guard(13,A1,A2,[_A3,_A4]).

%0.112133  !inPhase(a1,Post_Quals) v !taughtBy(a3,a2,a4) v courseLevel(a3,Level_100) v ta(a3,a1,a4) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(14,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(14,A1,A2,[_A3,_A4]).

%0.0518195  !inPhase(a1,Post_Quals) v !ta(a3,a1,a4) v courseLevel(a3,Level_100) v taughtBy(a3,a2,a4) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(15,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(15,A1,A2,[_A3,_A4]).

%-0.000634612  !inPhase(a1,Post_Generals) v !ta(a3,a1,a4) v !taughtBy(a3,a2,a4) v courseLevel(a3,Level_100) v advisedBy(a1,a2)
eq_clause(lit(p,cb(16,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(16,A1,A2,[_A3,_A4]).

%0.145903  !inPhase(a1,Post_Generals) v !taughtBy(a3,a2,a4) v courseLevel(a3,Level_100) v ta(a3,a1,a4) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(17,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(17,A1,A2,[_A3,_A4]).

%0.095052  !inPhase(a1,Post_Generals) v !ta(a3,a1,a4) v courseLevel(a3,Level_100) v taughtBy(a3,a2,a4) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(18,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(18,A1,A2,[_A3,_A4]).

%0.749123  !publication(a3,a1) v !publication(a3,a2) v samePerson(a1,a2) v advisedBy(a1,a2) v advisedBy(a2,a1)
% should add symmetry breaking
eq_clause(lit(p,cb(19,A1,A2)),and,[lit(n,advisedBy(A1,A2)),lit(n,advisedBy(A2,A1))]) :-
    guard(19,A1,A2,[_A3]).

%-0.0302834  !student(a1) v !professor(a2) v !publication(a3,a1) v !publication(a3,a2) v samePerson(a2,a1) v advisedBy(a1,a2)
eq_clause(lit(p,cb(20,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    guard(20,A1,A2,[_A3]).

%-0.337329  !publication(a3,a1) v publication(a3,a2) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(21,A1,A2)),and,[lit(n,advisedBy(A1,A2))]) :-
    guard(21,A1,A2,[_A3]).

%0.515549  student(a1) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(22,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    person(A1),
    \+ student(A1),
    person(A2).

%0.954782  professor(a2) v !advisedBy(a1,a2)
eq_clause(lit(p,cb(23,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    person(A2),
    \+ professor(A2),
    person(A1).

%-2.89681  advisedBy(a1,a2)
eq_clause(lit(p,cb(24,A1,A2)),and,[lit(p,advisedBy(A1,A2))]) :-
    person(A2),
    person(A1).

%% %-2.46982  EXIST y !student(x) v advisedBy(x,y) v tempAdvisedBy(x,y)
%% eq_clause(lit(n,cb(25,X)),and,Lits) :-
%%     student(X),
%%     \+ tempAdvisedBy(X,_Y),
%%     findall(lit(n,advisedBy(X,Y)),person(Y),Lits).

%% %-1.23183  EXIST y !professor(x) v hasPosition(x,Faculty_visiting) v advisedBy(y,x)
%% eq_clause(lit(n,cb(26,X)),and,Lits) :-
%%     professor(X),
%%     \+ hasPosition(X,"Faculty_visiting"),
%%     findall(lit(n,advisedBy(Y,X)),person(Y),Lits).

linear(0.0,[-1.0|Ones],[dcount(A1)|Others],0.0) :-
    person(A1),
    findall(lit(p,advisedBy(A1,Person)),person(Person),Others),
    findall(1.0,person(Person),Ones).

eq_quad([1.0,-2.0],[dcount(A1),cb(10,A1)],[1.0],[dcount(A1)],0.0) :-
    person(A1).

% objective vals


meta_objective(cb(Id,A1,A2),OrigCost,Cost) :-
    findall(a,guard(Id,A1,A2,_),Sols),
    length(Sols,Count),
    Cost is Count * OrigCost.

objective(cb(1,A1,A2),Cost) :-
    meta_objective(cb(1,A1,A2),0.0732856,Cost).
objective(cb(2,_,_),1.94203).
objective(cb(3,_,_),2.38127).
objective(cb(4,_,_),0.118837).
objective(cb(5,A1,A2),Cost) :-
    meta_objective(cb(5,A1,A2),0.0302834,Cost).
objective(cb(6,_,_),2.38127).
objective(cb(7,_,_),1.27773).
objective(cb(8,_),0.671981).
objective(cb(9,_,_),Cost) :- Cost is 2*0.709057.
objective(cb(9,_),Cost) :- Cost is 2*0.709057.
objective(cb(10,_),0.384788).
objective(cb(11,A1,A2),Cost) :-
    meta_objective(cb(11,A1,A2),2.01213,Cost).
objective(cb(12,_,_),0.326654).
objective(cb(13,A1,A2),Cost) :-
    meta_objective(cb(13,A1,A2),0.000635066,Cost).
objective(cb(14,A1,A2),Cost) :-
    meta_objective(cb(14,A1,A2),0.112133,Cost).
objective(cb(15,A1,A2),Cost) :-
    meta_objective(cb(15,A1,A2),0.0518195,Cost).
objective(cb(16,A1,A2),Cost) :-
    meta_objective(cb(16,A1,A2),0.000634612,Cost).
objective(cb(17,A1,A2),Cost) :-
    meta_objective(cb(17,A1,A2),0.145903,Cost).
objective(cb(18,A1,A2),Cost) :-
    meta_objective(cb(18,A1,A2),0.095052,Cost).
objective(cb(19,A1,A2),Cost) :-
    meta_objective(cb(19,A1,A2),0.749123,Cost).
objective(cb(20,A1,A2),Cost) :-
    meta_objective(cb(20,A1,A2),0.0302834,Cost).
objective(cb(21,A1,A2),Cost) :-
    meta_objective(cb(21,A1,A2),0.337329,Cost).
objective(cb(22,_,_),0.515549).
objective(cb(23,_,_),0.954782).
objective(cb(24,_,_),2.89681).
objective(cb(25,_),2.46982).
objective(cb(26,_),1.23183).

guard(1,A1,A2,[A3,A4]) :-
  courseLevel(A3,"Level_500"),
  taughtBy(A3,A2,A4),
  ta(A3,A1,A4),
  \+ tempAdvisedBy(A1,A2).
guard(5,A1,A2,[A3]) :-
  student(A1),
  publication(A3,A2),
  publication(A3,A1),
  \+ student(A2),
  \+ tempAdvisedBy(A1,A2).
guard(11,A1,_,[A3]) :-
  tempAdvisedBy(A1,A3).
guard(13,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Quals"),
  ta(A3,A1,A4),
  taughtBy(A3,A2,A4),
  \+ courseLevel(A3,"Level_100").
guard(14,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Quals"),
  taughtBy(A3,A2,A4),
  \+ courseLevel(A3,"Level_100"),
  \+ ta(A3,A1,A4).
guard(15,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Quals"),
  ta(A3,A1,A4),
  \+ courseLevel(A3,"Level_100"),
  \+ taughtBy(A3,A2,A4).
guard(16,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Generals"),
  ta(A3,A1,A4),
  taughtBy(A3,A2,A4),
  \+ courseLevel(A3,"Level_100").
guard(17,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Generals"),
  taughtBy(A3,A2,A4),
  \+ courseLevel(A3,"Level_100"),
  \+ ta(A3,A1,A4).
guard(18,A1,A2,[A3,A4]) :-
  inPhase(A1,"Post_Generals"),
  ta(A3,A1,A4),
  \+ courseLevel(A3,"Level_100"),
  \+ taughtBy(A3,A2,A4).
guard(19,A1,A2,[A3]) :-
  publication(A3,A1),
  publication(A3,A2),
  \+ samePerson(A1,A2).
guard(20,A1,A2,[A3]) :-
  student(A1),
  professor(A2),
  publication(A3,A1),
  publication(A3,A2),
  \+ samePerson(A2,A1).
guard(21,A1,A2,[A3]) :-
  publication(A3,A1),
  person(A2),
  \+ publication(A3,A2).


% context vars

hasPosition("Person292","Faculty_affiliate").
hasPosition("Person293","Faculty_affiliate").
hasPosition("Person240","Faculty").
hasPosition("Person211","Faculty").
hasPosition("Person150","Faculty").
hasPosition("Person415","Faculty").
hasPosition("Person79","Faculty").
hasPosition("Person349","Faculty_adjunct").
hasPosition("Person7","Faculty_adjunct").
hasPosition("Person319","Faculty").
hasPosition("Person185","Faculty_adjunct").
hasPosition("Person171","Faculty").
hasPosition("Person168","Faculty").
hasPosition("Person407","Faculty").

yearsInProgram("Person408","Year_2").
yearsInProgram("Person265","Year_9").
yearsInProgram("Person70","Year_1").
yearsInProgram("Person381","Year_10").
yearsInProgram("Person139","Year_3").
yearsInProgram("Person382","Year_3").
yearsInProgram("Person333","Year_2").
yearsInProgram("Person94","Year_1").
yearsInProgram("Person176","Year_2").
yearsInProgram("Person272","Year_2").
yearsInProgram("Person37","Year_1").
yearsInProgram("Person353","Year_4").
yearsInProgram("Person432","Year_5").
yearsInProgram("Person377","Year_1").
yearsInProgram("Person239","Year_4").
yearsInProgram("Person13","Year_7").
yearsInProgram("Person286","Year_3").
yearsInProgram("Person412","Year_3").
yearsInProgram("Person418","Year_3").
yearsInProgram("Person14","Year_10").
yearsInProgram("Person320","Year_3").
yearsInProgram("Person42","Year_1").
yearsInProgram("Person20","Year_1").
yearsInProgram("Person352","Year_5").
yearsInProgram("Person276","Year_3").
yearsInProgram("Person45","Year_5").
yearsInProgram("Person233","Year_1").
yearsInProgram("Person148","Year_5").
yearsInProgram("Person193","Year_1").
yearsInProgram("Person314","Year_4").
yearsInProgram("Person275","Year_5").
yearsInProgram("Person21","Year_5").
yearsInProgram("Person262","Year_7").
yearsInProgram("Person257","Year_7").
yearsInProgram("Person73","Year_4").
yearsInProgram("Person380","Year_6").
yearsInProgram("Person384","Year_3").
yearsInProgram("Person406","Year_5").
yearsInProgram("Person266","Year_5").
yearsInProgram("Person312","Year_4").
yearsInProgram("Person208","Year_4").
yearsInProgram("Person311","Year_3").
yearsInProgram("Person63","Year_5").
yearsInProgram("Person318","Year_5").
yearsInProgram("Person83","Year_5").
yearsInProgram("Person161","Year_7").
yearsInProgram("Person284","Year_3").

student("Person284").
student("Person311").
student("Person14").
student("Person275").
student("Person259").
student("Person139").
student("Person176").
student("Person400").
student("Person318").
student("Person161").
student("Person347").
student("Person408").
student("Person265").
student("Person70").
student("Person381").
student("Person382").
student("Person333").
student("Person94").
student("Person272").
student("Person37").
student("Person353").
student("Person432").
student("Person377").
student("Person239").
student("Person13").
student("Person286").
student("Person412").
student("Person418").
student("Person320").
student("Person42").
student("Person20").
student("Person352").
student("Person276").
student("Person45").
student("Person233").
student("Person148").
student("Person193").
student("Person314").
student("Person21").
student("Person262").
student("Person257").
student("Person73").
student("Person380").
student("Person384").
student("Person406").
student("Person266").
student("Person312").
student("Person208").
student("Person63").
student("Person83").
student("Person271").
student("Person392").
student("Person420").
student("Person86").

samePerson(X,X).

taughtBy("Course44","Person171","Autumn_0001").
taughtBy("Course24","Person240","Autumn_0001").
taughtBy("Course12","Person211","Autumn_0001").
taughtBy("Course123","Person150","Autumn_0001").
taughtBy("Course44","Person293","Winter_0001").
taughtBy("Course143","Person211","Winter_0001").
taughtBy("Course50","Person171","Winter_0001").
taughtBy("Course170","Person79","Winter_0001").
taughtBy("Course15","Person292","Winter_0001").
taughtBy("Course32","Person319","Winter_0001").
taughtBy("Course158","Person240","Winter_0001").
taughtBy("Course24","Person150","Spring_0001").
taughtBy("Course52","Person168","Spring_0001").
taughtBy("Course16","Person240","Spring_0001").
taughtBy("Course173","Person171","Spring_0001").
taughtBy("Course64","Person79","Spring_0001").
taughtBy("Course44","Person171","Autumn_0102").
taughtBy("Course24","Person211","Autumn_0102").
taughtBy("Course156","Person240","Autumn_0102").
taughtBy("Course12","Person79","Autumn_0102").
taughtBy("Course143","Person407","Winter_0102").
taughtBy("Course170","Person211","Winter_0102").
taughtBy("Course44","Person415","Spring_0102").
taughtBy("Course24","Person240","Spring_0102").
taughtBy("Course52","Person168","Spring_0102").
taughtBy("Course50","Person171","Spring_0102").
taughtBy("Course39","Person415","Spring_0102").
taughtBy("Course123","Person150","Spring_0102").
taughtBy("Course76","Person319","Spring_0102").
taughtBy("Course44","Person171","Autumn_0203").
taughtBy("Course24","Person240","Autumn_0203").
taughtBy("Course44","Person415","Winter_0203").
taughtBy("Course52","Person168","Winter_0203").
taughtBy("Course141","Person150","Winter_0203").
taughtBy("Course12","Person211","Winter_0203").
taughtBy("Course16","Person79","Winter_0203").
taughtBy("Course24","Person211","Spring_0203").
taughtBy("Course170","Person407","Spring_0203").
taughtBy("Course15","Person292","Spring_0203").
taughtBy("Course168","Person240","Spring_0203").
taughtBy("Course64","Person79","Spring_0203").
taughtBy("Course44","Person171","Autumn_0304").
taughtBy("Course24","Person79","Autumn_0304").
taughtBy("Course156","Person240","Autumn_0304").
taughtBy("Course12","Person407","Autumn_0304").
taughtBy("Course76","Person319","Autumn_0304").
taughtBy("Course44","Person415","Winter_0304").
taughtBy("Course57","Person150","Winter_0304").
taughtBy("Course52","Person168","Winter_0304").
taughtBy("Course170","Person79","Winter_0304").
taughtBy("Course24","Person407","Spring_0304").
taughtBy("Course50","Person171","Spring_0304").
taughtBy("Course158","Person240","Spring_0304").
taughtBy("Course7","Person415","Spring_0304").
taughtBy("Course128","Person150","Winter_0304").
taughtBy("Course132","Person319","Autumn_0304").
taughtBy("Course134","Person240","Spring_0203").
taughtBy("Course82","Person407","Winter_0102").

tempAdvisedBy("Person408","Person150").
tempAdvisedBy("Person382","Person415").
tempAdvisedBy("Person333","Person211").
tempAdvisedBy("Person94","Person79").
tempAdvisedBy("Person377","Person292").
tempAdvisedBy("Person412","Person168").
tempAdvisedBy("Person42","Person150").
tempAdvisedBy("Person20","Person240").
tempAdvisedBy("Person233","Person319").
tempAdvisedBy("Person193","Person415").
tempAdvisedBy("Person284","Person211").

ta("Course52","Person70","Winter_0304").
ta("Course44","Person193","Winter_0304").
ta("Course128","Person271","Winter_0304").
ta("Course128","Person392","Winter_0304").
ta("Course44","Person377","Autumn_0304").
ta("Course24","Person70","Autumn_0304").
ta("Course156","Person257","Autumn_0304").
ta("Course132","Person94","Autumn_0304").
ta("Course24","Person21","Spring_0203").
ta("Course44","Person420","Winter_0203").
ta("Course44","Person382","Winter_0203").
ta("Course141","Person14","Winter_0203").
ta("Course12","Person21","Winter_0203").
ta("Course44","Person286","Autumn_0203").
ta("Course52","Person318","Spring_0102").
ta("Course44","Person382","Spring_0102").
ta("Course44","Person86","Spring_0102").
ta("Course50","Person314","Spring_0102").
ta("Course39","Person73","Spring_0102").
ta("Course82","Person381","Winter_0102").

inPhase("Person408","Pre_Quals").
inPhase("Person265","Post_Generals").
inPhase("Person70","Pre_Quals").
inPhase("Person381","Post_Generals").
inPhase("Person139","Post_Quals").
inPhase("Person382","Post_Quals").
inPhase("Person333","Pre_Quals").
inPhase("Person94","Pre_Quals").
inPhase("Person176","Post_Quals").
inPhase("Person272","Post_Quals").
inPhase("Person37","Pre_Quals").
inPhase("Person353","Post_Quals").
inPhase("Person432","Post_Quals").
inPhase("Person377","Pre_Quals").
inPhase("Person239","Post_Quals").
inPhase("Person13","Post_Generals").
inPhase("Person286","Post_Quals").
inPhase("Person412","Post_Quals").
inPhase("Person418","Post_Quals").
inPhase("Person14","Post_Generals").
inPhase("Person320","Post_Quals").
inPhase("Person42","Pre_Quals").
inPhase("Person20","Pre_Quals").
inPhase("Person352","Post_Generals").
inPhase("Person276","Pre_Quals").
inPhase("Person45","Post_Generals").
inPhase("Person233","Pre_Quals").
inPhase("Person148","Post_Quals").
inPhase("Person193","Pre_Quals").
inPhase("Person314","Post_Generals").
inPhase("Person275","Post_Generals").
inPhase("Person21","Post_Generals").
inPhase("Person262","Post_Generals").
inPhase("Person257","Post_Generals").
inPhase("Person73","Post_Quals").
inPhase("Person380","Post_Generals").
inPhase("Person384","Post_Quals").
inPhase("Person406","Post_Generals").
inPhase("Person266","Post_Quals").
inPhase("Person312","Pre_Quals").
inPhase("Person208","Post_Quals").
inPhase("Person311","Post_Quals").
inPhase("Person63","Post_Generals").
inPhase("Person318","Pre_Quals").
inPhase("Person83","Post_Quals").
inPhase("Person161","Post_Generals").
inPhase("Person284","Post_Quals").

publication("Title25","Person284").
publication("Title284","Person14").
publication("Title110","Person14").
publication("Title118","Person14").
publication("Title71","Person14").
publication("Title316","Person14").
publication("Title118","Person318").
publication("Title217","Person161").
publication("Title55","Person161").
publication("Title331","Person161").
publication("Title250","Person161").
publication("Title268","Person161").
publication("Title271","Person161").
publication("Title171","Person161").
publication("Title120","Person347").
publication("Title86","Person347").
publication("Title338","Person347").
publication("Title224","Person347").
publication("Title260","Person347").
publication("Title112","Person347").
publication("Title97","Person347").
publication("Title50","Person292").
publication("Title103","Person292").
publication("Title166","Person292").
publication("Title72","Person292").
publication("Title47","Person292").
publication("Title41","Person292").
publication("Title40","Person293").
publication("Title13","Person240").
publication("Title140","Person240").
publication("Title217","Person240").
publication("Title92","Person240").
publication("Title167","Person240").
publication("Title331","Person240").
publication("Title26","Person240").
publication("Title275","Person240").
publication("Title333","Person240").
publication("Title270","Person240").
publication("Title208","Person240").
publication("Title103","Person240").
publication("Title268","Person240").
publication("Title340","Person240").
publication("Title192","Person240").
publication("Title54","Person240").
publication("Title177","Person240").
publication("Title33","Person240").
publication("Title10","Person240").
publication("Title84","Person240").
publication("Title161","Person240").
publication("Title248","Person240").
publication("Title102","Person240").
publication("Title274","Person240").
publication("Title47","Person240").
publication("Title0","Person240").
publication("Title82","Person240").
publication("Title337","Person240").
publication("Title344","Person240").
publication("Title254","Person240").
publication("Title119","Person240").
publication("Title114","Person211").
publication("Title259","Person211").
publication("Title59","Person211").
publication("Title160","Person211").
publication("Title88","Person211").
publication("Title24","Person211").
publication("Title323","Person211").
publication("Title190","Person211").
publication("Title11","Person211").
publication("Title199","Person211").
publication("Title240","Person211").
publication("Title335","Person211").
publication("Title241","Person211").
publication("Title212","Person211").
publication("Title228","Person211").
publication("Title345","Person211").
publication("Title89","Person211").
publication("Title165","Person211").
publication("Title113","Person211").
publication("Title233","Person211").
publication("Title132","Person211").
publication("Title310","Person211").
publication("Title218","Person211").
publication("Title71","Person211").
publication("Title341","Person211").
publication("Title207","Person211").
publication("Title229","Person211").
publication("Title292","Person211").
publication("Title49","Person211").
publication("Title238","Person211").
publication("Title255","Person211").
publication("Title329","Person211").
publication("Title79","Person211").
publication("Title325","Person211").
publication("Title44","Person211").
publication("Title25","Person211").
publication("Title118","Person150").
publication("Title140","Person415").
publication("Title12","Person415").
publication("Title182","Person415").
publication("Title122","Person415").
publication("Title208","Person415").
publication("Title103","Person415").
publication("Title347","Person415").
publication("Title266","Person415").
publication("Title340","Person415").
publication("Title269","Person415").
publication("Title5","Person415").
publication("Title70","Person415").
publication("Title179","Person415").
publication("Title29","Person415").
publication("Title72","Person415").
publication("Title47","Person415").
publication("Title0","Person415").
publication("Title38","Person415").
publication("Title290","Person415").
publication("Title63","Person415").
publication("Title82","Person415").
publication("Title283","Person415").
publication("Title337","Person415").
publication("Title94","Person415").
publication("Title147","Person415").
publication("Title329","Person415").
publication("Title297","Person415").
publication("Title79","Person415").
publication("Title312","Person415").
publication("Title107","Person415").
publication("Title273","Person415").
publication("Title172","Person415").
publication("Title295","Person415").
publication("Title41","Person415").
publication("Title325","Person415").
publication("Title44","Person415").
publication("Title87","Person415").
publication("Title222","Person415").
publication("Title236","Person415").
publication("Title258","Person415").
publication("Title301","Person415").
publication("Title318","Person79").
publication("Title115","Person79").
publication("Title231","Person79").
publication("Title226","Person79").
publication("Title195","Person79").
publication("Title162","Person185").
publication("Title178","Person171").
publication("Title225","Person171").
publication("Title269","Person171").
publication("Title150","Person171").
publication("Title70","Person171").
publication("Title63","Person171").
publication("Title94","Person171").
publication("Title147","Person171").
publication("Title170","Person171").
publication("Title125","Person171").
publication("Title90","Person171").
publication("Title114","Person407").
publication("Title12","Person407").
publication("Title259","Person407").
publication("Title217","Person407").
publication("Title92","Person407").
publication("Title182","Person407").
publication("Title59","Person407").
publication("Title160","Person407").
publication("Title55","Person407").
publication("Title88","Person407").
publication("Title167","Person407").
publication("Title24","Person407").
publication("Title323","Person407").
publication("Title331","Person407").
publication("Title190","Person407").
publication("Title120","Person407").
publication("Title250","Person407").
publication("Title11","Person407").
publication("Title284","Person407").
publication("Title199","Person407").
publication("Title240","Person407").
publication("Title335","Person407").
publication("Title270","Person407").
publication("Title241","Person407").
publication("Title212","Person407").
publication("Title110","Person407").
publication("Title268","Person407").
publication("Title228","Person407").
publication("Title347","Person407").
publication("Title266","Person407").
publication("Title192","Person407").
publication("Title345","Person407").
publication("Title5","Person407").
publication("Title271","Person407").
publication("Title89","Person407").
publication("Title165","Person407").
publication("Title113","Person407").
publication("Title233","Person407").
publication("Title179","Person407").
publication("Title132","Person407").
publication("Title177","Person407").
publication("Title310","Person407").
publication("Title171","Person407").
publication("Title33","Person407").
publication("Title218","Person407").
publication("Title71","Person407").
publication("Title341","Person407").
publication("Title207","Person407").
publication("Title229","Person407").
publication("Title292","Person407").
publication("Title316","Person407").
publication("Title49","Person407").
publication("Title38","Person407").
publication("Title238","Person407").
publication("Title283","Person407").
publication("Title255","Person407").
publication("Title224","Person407").
publication("Title260","Person407").
publication("Title297","Person407").
publication("Title312","Person407").
publication("Title273","Person407").
publication("Title25","Person407").
publication("Title258","Person407").
publication("Title118","Person408").
publication("Title118","Person353").
publication("Title40","Person239").
publication("Title13","Person13").
publication("Title26","Person13").
publication("Title275","Person13").
publication("Title333","Person13").
publication("Title54","Person13").
publication("Title10","Person13").
publication("Title84","Person13").
publication("Title161","Person13").
publication("Title248","Person13").
publication("Title344","Person13").
publication("Title50","Person352").
publication("Title208","Person352").
publication("Title103","Person352").
publication("Title166","Person352").
publication("Title314","Person352").
publication("Title47","Person352").
publication("Title86","Person352").
publication("Title82","Person352").
publication("Title79","Person352").
publication("Title261","Person352").
publication("Title87","Person352").
publication("Title329","Person45").
publication("Title79","Person45").
publication("Title325","Person45").
publication("Title44","Person45").
publication("Title150","Person148").
publication("Title125","Person148").
publication("Title90","Person148").
publication("Title162","Person193").
publication("Title170","Person314").
publication("Title107","Person314").
publication("Title172","Person314").
publication("Title295","Person314").
publication("Title222","Person314").
publication("Title301","Person314").
publication("Title25","Person21").
publication("Title122","Person262").
publication("Title314","Person262").
publication("Title29","Person262").
publication("Title72","Person262").
publication("Title290","Person262").
publication("Title86","Person262").
publication("Title261","Person262").
publication("Title41","Person262").
publication("Title102","Person257").
publication("Title274","Person257").
publication("Title254","Person257").
publication("Title119","Person257").
publication("Title269","Person73").
publication("Title63","Person73").
publication("Title318","Person380").
publication("Title115","Person380").
publication("Title231","Person380").
publication("Title226","Person380").
publication("Title195","Person380").
publication("Title314","Person406").
publication("Title86","Person406").
publication("Title261","Person406").
publication("Title118","Person208").
publication("Title182","Person63").
publication("Title178","Person63").
publication("Title225","Person63").
publication("Title5","Person63").
publication("Title314","Person63").
publication("Title86","Person63").
publication("Title147","Person63").
publication("Title261","Person63").
publication("Title97","Person63").
publication("Title222","Person63").
publication("Title236","Person63").
publication("Title301","Person63").
publication("Title325","Person83").

courseLevel("Course52","Level_400").
courseLevel("Course44","Level_400").
courseLevel("Course24","Level_400").
courseLevel("Course128","Level_400").
courseLevel("Course57","Level_400").
courseLevel("Course82","Level_400").
courseLevel("Course143","Level_400").
courseLevel("Course50","Level_500").
courseLevel("Course156","Level_500").
courseLevel("Course141","Level_500").
courseLevel("Course12","Level_500").
courseLevel("Course170","Level_500").
courseLevel("Course65","Level_500").
courseLevel("Course123","Level_500").
courseLevel("Course173","Level_500").
courseLevel("Course86","Level_500").
courseLevel("Course131","Level_500").
courseLevel("Course85","Level_500").
courseLevel("Course64","Level_500").
courseLevel("Course168","Level_500").
courseLevel("Course158","Level_500").
courseLevel("Course132","Level_500").
courseLevel("Course76","Level_500").
courseLevel("Course16","Level_500").
courseLevel("Course15","Level_500").
courseLevel("Course39","Level_500").
courseLevel("Course32","Level_500").
courseLevel("Course7","Level_500").
courseLevel("Course134","Level_500").
courseLevel("Course135","Level_500").

professor("Person319").
professor("Person292").
professor("Person293").
professor("Person240").
professor("Person211").
professor("Person150").
professor("Person415").
professor("Person79").
professor("Person349").
professor("Person7").
professor("Person185").
professor("Person171").
professor("Person168").
professor("Person407").

person("Person13").
person("Person139").
person("Person14").
person("Person148").
person("Person150").
person("Person161").
person("Person168").
person("Person171").
person("Person176").
person("Person185").
person("Person193").
person("Person20").
person("Person208").
person("Person21").
person("Person211").
person("Person233").
person("Person239").
person("Person240").
person("Person257").
person("Person259").
person("Person262").
person("Person265").
person("Person266").
person("Person271").
person("Person272").
person("Person275").
person("Person276").
person("Person284").
person("Person286").
person("Person292").
person("Person293").
person("Person311").
person("Person312").
person("Person314").
person("Person318").
person("Person319").
person("Person320").
person("Person333").
person("Person347").
person("Person349").
person("Person352").
person("Person353").
person("Person37").
person("Person377").
person("Person380").
person("Person381").
person("Person382").
person("Person384").
person("Person392").
person("Person400").
person("Person406").
person("Person407").
person("Person408").
person("Person412").
person("Person415").
person("Person418").
person("Person42").
person("Person420").
person("Person432").
person("Person45").
person("Person63").
person("Person7").
person("Person70").
person("Person73").
person("Person79").
person("Person83").
person("Person86").
person("Person94").
person("Person13").
person("Person139").
person("Person14").
person("Person148").
person("Person150").
person("Person161").
person("Person168").
person("Person171").
person("Person176").
person("Person185").
person("Person193").
person("Person20").
person("Person208").
person("Person21").
person("Person211").
person("Person233").
person("Person239").
person("Person240").
person("Person257").
person("Person259").
person("Person262").
person("Person265").
person("Person266").
person("Person271").
person("Person272").
person("Person275").
person("Person276").
person("Person284").
person("Person286").
person("Person292").
person("Person293").
person("Person311").
person("Person312").
person("Person314").
person("Person318").
person("Person319").
person("Person320").
person("Person333").
person("Person347").
person("Person349").
person("Person352").
person("Person353").
person("Person37").
person("Person377").
person("Person380").
person("Person381").
person("Person382").
person("Person384").
person("Person392").
person("Person400").
person("Person406").
person("Person407").
person("Person408").
person("Person412").
person("Person415").
person("Person418").
person("Person42").
person("Person420").
person("Person432").
person("Person45").
person("Person63").
person("Person7").
person("Person70").
person("Person73").
person("Person79").
person("Person83").
person("Person86").
person("Person94").

