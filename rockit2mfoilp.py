#!/usr/bin/env python
'''Converts rockit input to suitable Mercury code
converting all types to strings
'''

import argparse
import re
import sys

predpattern = re.compile(r'\s*(?P<pred>[^(]+)\((?P<args>[^)]+)\)\s*$')
   
parser = argparse.ArgumentParser(description='converts rockit input to suitable Mercury code')

parser.add_argument('mln', help='prog.mln file')
parser.add_argument('evidence', help='query.db file')
args = parser.parse_args()

observed_preds = {}
numargs = {}

for line in open(args.evidence):
   predresult = predpattern.match(line)
   if predresult is None:
      sys.exit("Failed to parse this fact: {0}".format(line))
   newargs = ['"'+x+'"' for x in predresult.group("args").split(',')]
   thispred = "obs_"+predresult.group("pred").lower()
   try:
      fobj = observed_preds[thispred]
   except KeyError:
      fobj = open(thispred,"w")
      observed_preds[thispred] = fobj
      numargs[thispred] = len(newargs)
   print >>fobj, "{0}({1}).".format(thispred,','.join(newargs))
for fobj in observed_preds.values():
   fobj.close()

prob = open("prob.m","w")

preamble = '''
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

'''

print >>prob, preamble


typedec = []
for thispred in observed_preds:
   nargs = numargs[thispred]
   typedec.append("{0}({1})".format(thispred,','.join(["string"]*nargs)))
print >>prob, ':- type atom ---> {0}.\n\n'.format(' ;\n'.join(typedec))

for thispred in observed_preds:
   nargs = numargs[thispred]
   variables = ','.join(['X'+str(i) for i in range(nargs)])
   atom = "{0}({1})".format(thispred,variables)
   print >>prob, ":- pred {0}({1}) is multi.".format(thispred,','.join(["string::out"]*nargs))
   print >>prob, ':- pragma fact_table({0}/{1}, "{0}").'.format(thispred,nargs)
   print >>prob, 'initial_clause("{0}") --> {{{1}}}, initial_poslit({1}).'.format(thispred,atom)
   

print >>prob, "objective(_Atom) = 0.0."
print >>prob, "clause(_,_,_) :- fail."
print >>prob, "clause(_) :- fail."
print >>prob, "neglit(_,_) :- fail."
print >>prob, "poslit(_,_) :- fail."
