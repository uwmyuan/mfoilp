#!/usr/bin/env python3
import re

fact_pattern = re.compile(r'^(\w+)\((.*)\)\s*$')
clause_pattern = re.compile(r'^([-\d+.]*)\s*(.*)')


class Theory:

    def __init__(self,clauses=None,from_files=False,prog='prog.mln',evidence='evidence.db'):
        if clauses is None:
            self._clauses = []
        else:
            self._clauses = clauses

        if not from_files:
            # don't record these at present if not reading a file
            # could extract them from clauses
            self._psyms = None
            self._logictypes = None
            self._constants = None
            return

        def ignore(line):
            return (
                line == '' or
                'EXIST' in line or
                line[:2] == '//'
            )

        reading_predicate_definitions = False
        for line in open(prog):
            if line.startswith('//predicate declarations'):
                reading_predicate_definitions = True

                # records all types appearing in formulae in the theory
                # and maps name/arity to pysm object
                self._logictypes = {}
                
                # records predicate symbols appearing in the theory
                # and maps name/arity to pysm object
                self._psyms = {}

                # records constants appearing in the theory
                self._constants = {}
                
                continue
            if reading_predicate_definitions:
                if line == '\n':
                    reading_predicate_definitions = False
                    continue
                if line[0] == '*':
                    cwa = True
                    line = line[1:]
                else:
                    cwa = False
                match = fact_pattern.match(line)
                name = match.group(1)
                lts = []
                for ltstr in match.group(2).split(','):
                    try:
                        lts.append(self._logictypes[ltstr])
                    except KeyError:
                        # new type
                        nt = LogicType(ltstr)
                        self._logictypes[ltstr] = nt
                        lts.append(nt)
                self._psyms[name,len(lts)] = PSym(name,len(lts),lts,cwa)
                continue

            if ignore(line):
                continue
            
            try:
                self._clauses.append(Clause(line=line,preds=self._psyms,constants=self._constants))
            except ValueError:
                pass

        for line in open(evidence):
            try:
                self._clauses.append(Clause(line=line,preds=self._psyms,constants=self._constants))
            except ValueError:
                pass

    def __str__(self):
        theory  = '\n'.join([repr(clause) for clause in self._clauses])
        if self._psyms is not None:
            preds = '\n'.join([repr(psym) for psym in self._psyms.values()])
        else:
            preds = ''
        if self._logictypes is not None:
            types = '\n'.join([str(lt) for lt in self._logictypes.values()])
        else:
            types = ''
        if self._psyms is not None:
            constants = '\n'.join([repr(c) for c in self._constants.values()])
        else:
            constants = ''
        return '\n\n'.join((theory,preds,types,constants))

        
    def clauses(self):
        return self._clauses
    
    def equivalent(self,other):
        '''True if two theories have equivalent clauses
        '''
        if len(self._clauses) != len(other.clauses()):
            return False
        other_matched = set(other.clauses())
        for clause in self._clauses:
            for oclause in other_matched.copy():
                if clause.equivalent(oclause):
                    other_matched.remove(oclause)
                    break
            else:
                return False
        return True

    def weights_on_atoms(self):
        '''create an equivalent theory
        where only atoms can have weights
        '''
        newtheory = Theory()
        newtheory._pysms = self._psyms
        newtheory._logictypes = self._logictypes
        newtheory._constants = self._constants
        cbn = 1
        for clause in self._clauses:
            weight = clause.weight()
            if weight is None:
                newtheory._clauses.append(clause)
            else:
                vs = clause.get_vars()
                cp = PSym('cb{0}'.format(cbn),len(vs),[v.logictype() for v in vs])
                cbn += 1
                if weight > 0:
                    lit = Lit(cp,vs)
                    cbclause = Clause(lits=[lit],weight=weight)
                    extclause = Clause(lits=clause.lits()+[lit])
                    newtheory._clauses.append(cbclause)
                    newtheory._clauses.append(extclause)
        return newtheory
    
    def add_clause(self,clause):
        self._clauses.append(clause)
    
    def delete_zero_weighted(self):
        clauses = []
        for clause in self._clauses:
            if clause.weight() != 0.0:
                clauses.append(clause)
        self._clauses = clauses

    def constants(self):
        ks = {}
        for i, clause in enumerate(self._clauses):
            for j, lit in enumerate(clause.lits()):
                for k, arg in enumerate(lit.args()):
                    for cons in arg.constants():
                        try:
                            ks[cons].add((i,j,k))
                        except KeyError:
                            ks[cons] = set([(i,j,k)])
        return ks

    def after_swapping(self,k1,k2):
        '''returns theory that we get once constants k1 and k2 
        have been swapped
        '''
        swapped = Theory()
        for clause in self.clauses():
            swapped.add_clause(clause.after_swapping(k1,k2))
        return swapped

    def containing_two(self,ks,k1,k2):
        '''make theory with only clauses containing either k1 or k2
        using constant dictionary
        '''
        theory = Theory()
        for trio in ks[k1]:
            theory.add_clause(self._clauses[trio[0]])
        for trio in ks[k2]:
            theory.add_clause(self._clauses[trio[0]])
        return theory

    # def find_equiv(self):
    #     ks = self.constants()
    #     kskeys = ks.keys()
    #     for i, k1 in enumerate(kskeys):
    #         for k2 in kskeys[i+1:]:
    #             th = self.containing_two(ks,k1,k2)
    #             th2 = th.after_swapping(k1,k2)
    #             if th2.equivalent(th):
    #                 print th
    #                 print th2
    #                 print 'yes', k1, k2
    #                 print
    #                 print

class LogicType:
    
    def __init__(self,name):
        self._name = name

    def __eq__(self,other):
        return self._name == other._name

    def __repr__(self):
        return self._name
    
    def name(self):
        return self._name

    
class PFSym:
    '''Union of predicate symbols and function symbols
    '''

    def __init__(self,name,arity,logictypes=None):
        self._name = name
        self._arity = arity
        self._logictypes = [None]*arity
        if logictypes is not None:
            for i, typ in enumerate(logictypes):
                self._logictypes[i] = typ

    def __repr__(self):
        if isinstance(self,PSym):
            klass_name = 'PSym'
        else:
            klass_name = 'FSym'
        return '{0}({1},{2},logictypes=[{3}])'.format(
            klass_name,
            self._name,
            self._arity,
            ','.join((repr(x) for x in self._logictypes)))

    def __str__(self):
        '''NB: arity not given in this
        informal string representation
        '''
        return self._name
    
    def __eq__(self,other):
        '''have to agree on types to be considered
        the same
        '''
        if self.__class__ != other.__class__:
            return False
        if self._name != other._name:
            return False
        elif self._arity != other._arity:
            return False
        else:
            for i, typ in enumerate(self._logictypes):
                if typ != other._logictypes[i]:
                    return False
        return True
            
    def arity(self):
        return self._arity

    def logictypes(self,i):
        return self._logictypes[i]

class PSym(PFSym):
    '''Predicate symbols
    '''
    def __init__(self,name,arity,types=None,cwa=False):
        super().__init__(name,arity,types)
        self._cwa = cwa

class FSym(PFSym):
    '''Function symbols
    '''
    pass

class MercuryClause:

    def __init__(self,head,body=None):
        self._head = head
        self._body = body

    def __str__(self):
        ret = str(self._head)
        if self._body is not None:
            return '{0} :-\n{1}.'.format(str(self._head),',\n'.join(['\t'+str(x) for x in self._body]))
        else:
            return '{0}.'.format(str(self._head))

    def add_lit(self,lit):
        self._body.append(lit)


class Clause:

    _num = 1
    
    def __init__(self,lits=None,weight=None,line=None,preds=None,constants=None):
        '''construct from a set of lits or from a textual
        description
        '''
        self._weight = weight
        if lits is None:
            self._lits = []
        else:
            self._lits = lits

        if line is not None:

            if lits is not None:
                raise ValueError("Do not supply both lits and textual rep when making a clause")
            
            clausematch = clause_pattern.match(line)
            if clausematch is None:
                raise ValueError("'{0}' does not describe a clause".format(line))

            try:
                self._weight = float(clausematch.group(1))
            except ValueError:
                self._weight = None
                
            lits = clausematch.group(2).split(' v ')

            if len(lits) == 0:
                raise ValueError("'{0}' has no literals".format(line))

            clausevars = []
            for lit in lits:
                if len(lit) == 0:
                    raise ValueError("'{0} in {1}' does not describe a literal".format(lit,line))
                if lit[0] == '!':
                    negated = True
                    lit = lit[1:]
                else:
                    negated = False
                match = fact_pattern.match(lit)
                if match is None:
                    raise ValueError("'{0}' does not describe a literal".format(lit))
                outargs = []
                inargs = match.group(2).split(',')
                # use PSym object rather than just string
                # will generate Key Error if missing
                psym = preds[match.group(1),len(inargs)]

                for i, arg in enumerate(inargs):
                    if arg[0].islower():
                        newvar = Variable(arg.capitalize(),logictype=psym.logictypes(i))
                        for v in clausevars:
                            if v.eq(newvar):
                                newvar = v
                                break
                        else:
                            clausevars.append(newvar)
                        outargs.append(newvar)
                    else:
                        cons_str = arg.strip()
                        try:
                            cons = constants[cons_str]
                        except KeyError:
                            cons = NonVarTerm('"{0}"'.format(cons_str),logictype=psym.logictypes(i))
                            #update constants
                            constants[cons_str] = cons
                        outargs.append(cons)
                self._lits.append(Lit(psym,outargs,negated))

    def lits(self):
        return self._lits
            
    def weight(self):
        return self._weight
            
    def add_lit(self,lit):
        self._lits.append(lit)

    def after_swapping(self,k1,k2):
        '''returns clause that we get once constants k1 and k2 
        have been swapped
        '''
        swapped = Clause()
        for lit in self._lits:
            swapped.add_lit(lit.after_swapping(k1,k2))
        return swapped

    def equivalent(self,other):
        '''True if two clauses have the same set of lits,
        and same weight if any
        '''
        if self._weight != other.weight():
            return False
        if len(self._lits) != len(other.lits()):
            return False
        other_matched = set(other.lits())
        for lit in self._lits:
            for olit in other_matched.copy():
                if lit.equivalent(olit):
                    other_matched.remove(olit)
                    break
            else:
                return False
        return True

    def get_vars(self):
        vs = set()
        for lit in self._lits:
            vs.update(lit.get_vars())
        return vs

    def remove_cwas(self):
        lits = []
        for lit in self._lits:
            if lit.psym() not in PSym.cwa:
                lits.append(lit)
        self._lits = lits
    
    def make_weighted(self,weight):
        #self.remove_cwas()
        vs = self.get_vars()
        cblit = Lit('cb'+str(Clause._num),sorted(vs))
        Clause._num += 1
        clauses = []
        if weight > 0:
            self.add_lit(cblit)
            clauses.append(self)
        elif weight < 0:
            for lit in self._lits:
                clause = Clause([cblit])
                clause.add_lit(Lit(lit.psym(),lit.args(),not lit.is_negated()))
                clauses.append(clause)
        return clauses

    def __repr__(self):
        return 'Clause([{0}],{1})'.format(','.join([repr(lit) for lit in self._lits]),self._weight)
    
    def __str__(self):
        if self._weight is not None:
            ret = '{0} : '.format(self._weight)
        else:
            ret = ''
        return ret + ';'.join([str(x) for x in self._lits])

class GuardedClause(Clause):

    def add_guard(self,guard):
        self._guard = guard

    def get_guard(self):
        return self._guard
    
class Lit:

    def __init__(self,psym,args=None,negated=False):

        if args is None:
            self._args = []
        else:
            self._args=args
        
        self._psym = psym
        self._negated = negated

    def __repr__(self):
        return 'Lit({0},[{1}],{2})'.format(
            repr(self._psym),
            ','.join([repr(x) for x in self._args]),
            self._negated)

    def __str__(self):
        pred_name = str(self._psym)
        if self._args is None:
            ret = pred_name
        else:
            ret = '{0}({1})'.format(pred_name,','.join([str(x) for x in self._args]))        
        if self._negated:
            return 'not ' + ret
        else:
            return ret

        
    def after_swapping(self,k1,k2):
        '''returns lit that we get once constants k1 and k2 
        have been swapped
        '''
        swapped = Lit(self._psym,negated=self._negated)
        for arg in self._args:
            if arg == k1:
                swapped.add_arg(k2)
            elif arg == k2:
                swapped.add_arg(k1)
            else:
                swapped.add_arg(arg)
        return swapped

    def add_arg(self,arg):
        self._args.append(arg)
            
    def equivalent(self,other):
        '''True if two lits are the same
        '''
        if self._psym != other.psym():
            return False
        if self._negated != other.is_negated():
            return False
        otherargs = other.args()
        for i, arg in enumerate(self._args):
            if not arg.equivalent(otherargs[i]):
                return False
        return True
            
    def psym(self):
        return self._psym

    def args(self):
        return self._args
    
    def is_negated(self):
        return self._negated
            
    def get_vars(self):
        vs = set()
        for arg in self._args:
            vs.update(arg.get_vars())
        return vs
            
    def psym(self):
        return self._psym


class Term:

    def logictype(self):
        return self._logictype
    
class Variable(Term):

    def __init__(self,varname,logictype=None):
        self._varname = varname
        self._logictype = logictype
        
    def eq(self,other):
        return (
            self._varname == other._varname and
            self._logictype == other._logictype)
        
    def varname(self):
        '''return variable as a string
        '''
        return self._varname 
        
    def equivalent(self,other):
        '''other can be any term'''
        try:
            return self._varname == other.varname()
        except AttributeError:
            return False
        
    def get_vars(self):
        return set([self])
        
    def __repr__(self):
        '''produces str representation
        as well
        '''
        return 'Variable({0},{1})'.format(self._varname,repr(self._logictype))

    def constants(self):
        return set()
    
class NonVarTerm(Term):

    def __init__(self,fsym,args=None,logictype=None):
        self._fsym = fsym
        self._logictype = logictype
        if args is None:
            self._args = ()
        else:
            self._args = tuple(args)

    def __repr__(self):
        return 'NonVarTerm({0},{1},logictype={2})'.format(
            repr(self._fsym),
            [repr(x) for x in self._args],
            repr(self._logictype))
            
    def __str__(self):
        functor = str(self._fsym)
        if self.is_constant():
            return functor
        else:
            return '{0}({1})'.format(functor,','.join([str(x) for x in args]))

    def __eq__(self,other):
        if self.__class__ != other.__class__:
            return False
        if self._fsym != other._fsym:
            return False
        for i, arg in enumerate(self._args):
            if arg != other._args[i]:
                return False
        return True

        
    def args(self):
        return self._args 
            
        
    def constants(self):
        if self.is_constant():
            return set([self])
        else:
            ks = set()
            for arg in self._args:
                ks.update(arg.constants())
            return ks
            
    def is_constant(self):
        return self._args == ()

    def get_vars(self):
        vs = set()
        for arg in self._args:
            vs.update(arg.get_vars())
        return vs

if __name__ == '__main__':

    theory = Theory(from_files=True)
    #theory.delete_zero_weighted()
    print(theory)

    print('************')
    
    print(theory.weights_on_atoms())

    #print theory.constants()

    #theory.find_equiv()
