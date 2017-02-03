#!/usr/bin/env python3
'''
converts tuffy input to mfoilp input

use like this: python tuffy2mfoilp.py prog.mln evidence.db

In this version everything is a string.
'''
import sys
import re

fact_pattern = re.compile(r'^(\w+)\((.*)\)\s*$')
clause_pattern = re.compile(r'^([-\d+.]+)\s+(.*)')

mln = open(sys.argv[1])
evidence = open(sys.argv[2])

header = '''
:- module prob.
:- interface.

%----------------------------------------------------------------------%

:- import_module mfoilp.

:- type atom.

:- pred clause(string::in,clause_info::in,clause_info::out) is nondet.

:- pred clause(string::out) is multi.

:- pred neglit(string::in,atom::in) is semidet.
:- pred poslit(string::in,atom::in) is semidet.

:- pred objective(atom::in,float::out) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module string.
:- import_module int.
'''

def updatetypes(match):
    k = match.group(1)
    v = match.group(2).split(',')
    type_dkt[k] = v

def getpred(lit):
    match = fact_pattern.match(lit)
    return match.group(1)
    
def getargs(lit):
    match = fact_pattern.match(lit)
    return frozenset([x for x in match.group(2).split(',') if x[0].isupper()])

def nargs(lit):
    match = fact_pattern.match(lit)
    return len(match.group(2).split(','))

def is_cwa(lit):
    match = fact_pattern.match(lit)
    return match.group(1) in cwa
    
def to_lp(lit):
    match = fact_pattern.match(lit)
    if match is None:
        return 'ERROR'
    outargs = []
    for arg in match.group(2).split(','):
        if arg[0].islower():
            outargs.append(arg.capitalize())
        else:
            outargs.append('"{0}"'.format(arg))
    return "{0}({1})".format(match.group(1),','.join(outargs))
            
current_predicate = None
fobj = None
fact_table_decl = []
for line in evidence:
    match = fact_pattern.match(line)
    if match:
        this_predicate = match.group(1)
        args = ['"'+x+'"' for x in match.group(2).replace(' ','').split(',')]
        if this_predicate != current_predicate:
            if fobj is not None:
                fobj.close()
            current_predicate = this_predicate
            fobj = open(current_predicate,'w')
            decl_args = ','.join(['string::out']*len(args))
            fact_table_decl.append(':- pred {0}({1}) is multi.'.format(this_predicate,decl_args))
            fact_table_decl.append(':- pragma fact_table({0}/{1},"{0}").'.format(this_predicate,len(args)))
        print('{0}({1}).'.format(this_predicate,','.join(args)),file=fobj)
fobj.close()

cwa = set()
noncwa = set()
foclausenum = 1
type_dkt = {}
atom_types = set()
objectives = []
clauses = []
cl1 = []
pl = []
nl = []

for line in mln:
    line = line.rstrip()

    if line == '':
        continue
    
    if line[:2] == '//':
        continue

    if line[0] == '*':
        match = fact_pattern.match(line[1:])
        if match:
            cwa.add(match.group(1))
            updatetypes(match)
            continue

    match = fact_pattern.match(line)
    if match:
        noncwa.add(match.group(1))
        updatetypes(match)
        continue

    match = clause_pattern.match(line)
    if match:
        lits = match.group(2).split(' v ')
        neglits = []
        poslits = []
        for lit in lits:
            if lit[0] == '!':
                neglits.append(to_lp(lit[1:]))
            else:
                poslits.append(to_lp(lit))
        #print(line,poslits,neglits)
        weight = float(match.group(1))
        if weight > 0:
            mc_lits = ['clause("{0}")'.format(foclausenum)]
            grounded = set()
            cl1.append('clause("{0}").'.format(foclausenum))
            for neglit in neglits:
                if not is_cwa(neglit):
                    if not getargs(neglit).issubset(grounded):
                        mc_lits.append('insol({0})'.format(neglit))
                    mc_lits.append('neglit({0})'.format(neglit))
                    nl.append('neglit("{0}",{1}).'.format(foclausenum,neglit))
                    atom_types.add('{0}({1})'.format(getpred(neglit),','.join(['string']*nargs(neglit))))
                    grounded.update(getargs(neglit))
            for neglit in neglits:
                if is_cwa(neglit):
                    mc_lits.append('{{{0}}}'.format(neglit))
                    grounded.update(getargs(neglit))
            for poslit in poslits:
                if is_cwa(poslit):
                    match = fact_pattern.match(poslit)
                    predsym = match.group(1)
                    args = match.group(2).split(',')
                    for i, arg in enumerate(args):
                        if arg[0].isupper() and arg not in grounded:
                            #print(type_dkt)
                            #print(predsym)
                            #print(i)
                            mc_lits.append('{{{0}({1})}}'.format(type_dkt[predsym][i],arg))
                    mc_lits.append('{{not {0}}}'.format(poslit))
                    grounded.update(getargs(poslit))
            for poslit in poslits:
                if not is_cwa(poslit):
                    match = fact_pattern.match(poslit)
                    predsym = match.group(1)
                    args = match.group(2).split(',')
                    for i, arg in enumerate(args):
                        if arg[0].isupper() and arg not in grounded:
                            mc_lits.append('{0}({1})'.format(predsym,type_dkt[predsym][i]))
                    mc_lits.append('poslit({0})'.format(poslit))
                    pl.append('poslit("{0}",{1}).'.format(foclausenum,poslit))
                    atom_types.add('{0}({1})'.format(getpred(poslit),','.join(['string']*nargs(poslit))))
                    grounded.update(getargs(poslit))
            cblit = 'cb({0},{1})'.format(foclausenum,','.join(sorted(grounded)))
            atom_types.add('cb(int,{0})'.format(','.join(['string']*len(grounded))))
            mc_lits.append('poslit({0})'.format(cblit))
            pl.append('poslit("{0}",{1}).'.format(foclausenum,cblit))

            this_clause = '{0} -->\n'.format(mc_lits[0])
            for lit in mc_lits[1:-1]:
                  this_clause += '  {0},\n'.format(lit)
            this_clause += '  {0}.\n'.format(mc_lits[-1])
            clauses.append(this_clause)
            objectives.append('objective({0},{1}).'.format(cblit,weight))
            foclausenum += 1

#print(atom_types)

print(header)
print('% define atotm type')
print(':- type -->')
atom_types = sorted(atom_types)
for at in atom_types[:-1]:
    print('  {0}'.format(at))
print('  {0}.'.format(atom_types[-1]))
print()
print('% provide non-zero objective values for each atom-variable')
for obj in objectives:
    print(obj)
print()
print('% predicates defined in fact tables') 
for f in fact_table_decl:
    print(f)
print()
print('% clauses')
for c in clauses:
    print(c)
print()

print('% utility predicates to make e.g. variable locking easier')
for x in cl1:
    print(x)
print()

for x in pl:
    print(x)
print()

for x in nl:
    print(x)
print()
