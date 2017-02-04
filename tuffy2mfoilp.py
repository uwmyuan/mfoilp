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
:- pred initial_clause(string::out,clause_lits::in,clause_lits::out) is nondet.

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

def update_grounded(grounded,lit):
    match = fact_pattern.match(lit)
    predsym = match.group(1)
    for i, x in enumerate(match.group(2).split(',')):
        if x not in grounded and x[0].isupper():
            grounded[x] = type_dkt[predsym][i]

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


def process_clause(neglits,poslits,foclausenum,cblit):
        mc_lits = ['clause("{0}")'.format(foclausenum)]
        # 'grounded' maps each variable to its type
        grounded = {}
        cl1.append('clause("{0}").'.format(foclausenum))
        for neglit in neglits:
            if not is_cwa(neglit):
                if not getargs(neglit).issubset(grounded):
                    mc_lits.append('insol({0})'.format(neglit))
                mc_lits.append('neglit({0})'.format(neglit))
                nl.append('neglit("{0}",{1}).'.format(foclausenum,neglit))
                atom_types.add('{0}({1})'.format(getpred(neglit),','.join(['string']*nargs(neglit))))
                update_grounded(grounded,neglit)
        for neglit in neglits:
            if is_cwa(neglit):
                mc_lits.append('{{{0}}}'.format(neglit))
                inout = []
                match = fact_pattern.match(neglit)
                for a in match.group(2).split(','):
                    if a[0].isupper() and a not in grounded:
                        inout.append('out')
                    else:
                        inout.append('in')
                try:
                    modes[getpred(neglit)].add(tuple(inout))
                except KeyError:
                    modes[getpred(neglit)] = set([tuple(inout)])
                update_grounded(grounded,neglit)
        for poslit in poslits:
            if is_cwa(poslit):
                match = fact_pattern.match(poslit)
                predsym = match.group(1)
                args = match.group(2).split(',')
                for i, arg in enumerate(args):
                    if arg[0].isupper() and arg not in grounded:
                        mc_lits.append('{{{0}({1})}}'.format(type_dkt[predsym][i],arg))
                mc_lits.append('{{not {0}}}'.format(poslit))
                inout = ['in']*len(args)
                try:
                    modes[getpred(poslit)].add(tuple(inout))
                except KeyError:
                    modes[getpred(poslit)] = set([tuple(inout)])
                update_grounded(grounded,poslit)
        for poslit in poslits:
            if not is_cwa(poslit):
                match = fact_pattern.match(poslit)
                predsym = match.group(1)
                args = match.group(2).split(',')
                for i, arg in enumerate(args):
                    if arg[0].isupper() and arg not in grounded:
                        mc_lits.append('{{{0}({1})}}'.format(type_dkt[predsym][i],arg))
                mc_lits.append('poslit({0})'.format(poslit))
                pl.append('poslit("{0}",{1}).'.format(foclausenum,poslit))
                atom_types.add('{0}({1})'.format(getpred(poslit),','.join(['string']*nargs(poslit))))
                update_grounded(grounded,poslit)
        sorted_grounded = sorted(grounded)
        if cblit is None:
            cblitargs = sorted_grounded
            cblit = 'cb({0},{1})'.format(foclausenum,','.join(cblitargs))
            atom_types.add('cb(int,{0})'.format(','.join(['string']*len(grounded))))
            # this is wrong, need to get the correct types!!
            type_dkt['cb{0}'.format(foclausenum)] = [grounded[x] for x in sorted_grounded]
        else:
            match = fact_pattern.match(cblit)
            args = match.group(2).split(',')
            predsym = 'cb{0}'.format(args[0])
            args = args[1:]
            for i, arg in enumerate(args):
                if arg[0].isupper() and arg not in grounded:
                    mc_lits.append('{{{0}({1})}}'.format(type_dkt[predsym][i],arg))
        mc_lits.append('poslit({0})'.format(cblit))
        pl.append('poslit("{0}",{1}).'.format(foclausenum,cblit))
        this_clause = '{0} -->\n'.format(mc_lits[0])
        for lit in mc_lits[1:-1]:
              this_clause += '  {0},\n'.format(lit)
        this_clause += '  {0}.\n'.format(mc_lits[-1])
        return this_clause, cblit

current_predicate = None
#fact_table_decl = []
fobjs = {}
for line in evidence:
    match = fact_pattern.match(line)
    if match:
        this_predicate = match.group(1)
        args = ['"'+x+'"' for x in match.group(2).replace(' ','').split(',')]
        if this_predicate != current_predicate:
            try:
                fobj = fobjs[this_predicate]
            except KeyError:
                fobj = open(this_predicate,'a')
                fobjs[this_predicate] = fobj
                decl_args = ','.join(['string::out']*len(args))
                #fact_table_decl.append(':- pred {0}({1}) is multi.'.format(this_predicate,decl_args))
                #fact_table_decl.append(':- pragma fact_table({0}/{1},"{0}").'.format(this_predicate,len(args)))
            current_predicate = this_predicate
        print('{0}({1}).'.format(this_predicate,','.join(args)),file=fobj)
for fobj in fobjs.values():
    fobj.close()
evidence.close()

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
modes = {}
for line in mln:
    line = line.rstrip()

    if line == '':
        continue

    if 'EXIST' in line:
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
            clause, cblit = process_clause(neglits,poslits,foclausenum,None)
            clauses.append(clause)
            foclausenum += 1
            objectives.append('objective({0},{1}).'.format(cblit,weight))
        elif weight < 0:
            # use this to get the cblit
            clause, cblit = process_clause(neglits,poslits,foclausenum,None)
            cwa_neglits = []
            noncwa_neglits = []
            for neglit in neglits:
                if is_cwa(neglit):
                    cwa_neglits.append(neglit)
                else:
                    noncwa_neglits.append(neglit)
            cwa_poslits = []
            noncwa_poslits = []
            for poslit in poslits:
                if is_cwa(poslit):
                    cwa_poslits.append(poslit)
                else:
                    noncwa_poslits.append(poslit)
            for neglit in noncwa_neglits:
                clause, junk = process_clause(cwa_neglits+[neglit],cwa_poslits,foclausenum,cblit)
                clauses.append(clause)
                foclausenum += 1
            for poslit in noncwa_poslits:
                clause, junk = process_clause(cwa_neglits,cwa_poslits+[poslit],foclausenum,cblit)
                clauses.append(clause)
                foclausenum += 1
            objectives.append('objective({0},{1}).'.format(cblit,-weight))
            
# now collect constants of various types

evidence = open(sys.argv[2])
constants = {}
for line in evidence:
    match = fact_pattern.match(line)
    if not match:
        continue
    types = type_dkt[match.group(1)]
    for i, constant in enumerate(match.group(2).split(',')):
        try:
            constants[types[i]].add(constant)
        except KeyError:
            constants[types[i]] = set([constant])
evidence.close()
            
print(header)
print('% define atom type')
print(':- type atom --->')
atom_types = sorted(atom_types)
for at in atom_types[:-1]:
    print('  {0};'.format(at))
print('  {0}.'.format(atom_types[-1]))
print()
print('% provide non-zero objective values for each atom-variable')
for obj in objectives:
    print(obj)
print()
print('% predicates defined in fact tables') 
for pred, modes in modes.items():
    l = len(list(modes)[0])
    decl_args = ','.join(['string']*l)
    print(':- pred {0}({1}).'.format(pred,decl_args))
    for mode in modes:
        print(':- mode {0}({1}) is nondet.'.format(pred,','.join(mode)))
    #print(':- pragma fact_table({0}/{1},"{0}").'.format(pred,l))
    if pred.startswith('same'):
        print('{0}(X,X).'.format(pred))
    else:
        fobj = open(pred)
        for line in fobj:
            print(line,end="")
        fobj.close()
    print()
print()
print('% no initial clauses')
print('initial_clause(_,_,_) :- fail.')
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

for typ, konstants in constants.items():
    print(':- pred {0}(string).'.format(typ))
    print(':- mode {0}(out) is multi.'.format(typ))
    print(':- mode {0}(in) is semidet.'.format(typ))
    for k in sorted(konstants):
        print('{0}("{1}").'.format(typ,k.strip()))
    print()
