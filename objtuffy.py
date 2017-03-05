import re

fact_pattern = re.compile(r'^(\w+)\((.*)\)\s*$')
clause_pattern = re.compile(r'^([-\d+.]+)\s+(.*)')


class Theory:

    def __init__(self,clauses=None):
        if clauses is None:
            self._clauses = []
        else:
            self._clauses = clauses

    def __str__(self):
        out = ''
        for clause in self._clauses:
            out += '{0}\n'.format(clause)
        return out

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

    def find_equiv(self):
        ks = self.constants()
        kskeys = ks.keys()
        for i, k1 in enumerate(kskeys):
            for k2 in kskeys[i+1:]:
                th = self.containing_two(ks,k1,k2)
                th2 = th.after_swapping(k1,k2)
                if th2.equivalent(th):
                    print th
                    print th2
                    print 'yes', k1, k2
                    print
                    print

class Types:
    
    def __init__(self,name):
        self._name = name

    def name(self):
        return self._name

    def __eq__(self,other):
        return self._name == other._name
    
class PFSym:
    '''Union of predicate symbols and function symbols
    '''

    def __init__(self,name,arity,types=None):
        self._name = name
        self._arity = arity
        self._types = [None]*arity
        if types is not None:
            for i, typ in enumerate(types):
                self._types[i] = typ

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
            for i, typ in enumerate(self._types):
                if typ != other._types[i]:
                    return False
        return True
            
    def arity(self):
        return self._arity

    def type(self,i):
        return self._types[i]

class PSym(PFSym):
    '''Predicate symbols
    '''
    cwa = set()

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
    
    def __init__(self,lits=None,weight=None,line=None):
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

            self._weight = float(clausematch.group(1))
            lits = clausematch.group(2).split(' v ')
            for lit in lits:
                if lit[0] == '!':
                    negated = True
                    lit = lit[1:]
                else:
                    negated = False
                match = fact_pattern.match(lit)
                if match is None:
                    raise ValueError("'{0}' does not describe a literal".format(lit))
                outargs = []
                for arg in match.group(2).split(','):
                    if arg[0].islower():
                        outargs.append(Variable(arg.capitalize()))
                    else:
                        outargs.append(NonVarTerm('"{0}"'.format(arg.strip())))
                self._lits.append(Lit(match.group(1),outargs,negated))

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
            
    def __str__(self):
        if self._args is None:
            ret = self._psym
        else:
            ret = '{0}({1})'.format(self._psym,','.join([str(x) for x in self._args]))        
        if self._negated:
            return 'not ' + ret
        else:
            return ret

    def psym(self):
        return self._psym


class Term:
    pass
    
class Variable(Term):

    def __init__(self,varname):
        self._varname = varname

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
        return set([self._varname])
        
    def __str__(self):
        return self._varname

    def constants(self):
        return set()
    
class NonVarTerm(Term):

    def __init__(self,fsym=None,args=None):
        self._fsym = fsym
        if args is None:
            self._args = ()
        else:
            self._args = tuple(args)

    def args(self):
        return self._args 
            
    def equivalent(self,other):
        try:
            if self._fsym == other.fsym():
                if self.is_constant():
                    return True
            else:
                return False
            otherargs = other.args()
            if len(self._args) != len(otherargs):
                return False
        except AttributeError:
            return False
        for i, arg in enumerate(self._args):
            if not arg.equivalent(otherargs[i]):
                return False
        return True
        
        
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
    
    def __str__(self):
        if self.is_constant():
            return self._fsym
        else:
            return '{0}({1})'.format(self._fsym,','.join([str(x) for x in args]))




if __name__ == '__main__':


    def ignore(line):
        return (
            line == '' or
            'EXIST' in line or
            line[:2] == '//'
        )
    
    clauses = []
    for line in open('prog.mln'):

        if ignore(line):
            continue

        if line[0] == '*':
            match = fact_pattern.match(line[1:])
            if match:
                PSym.cwa.add(match.group(1))
                continue
        try:
            clauses.append(Clause(line=line))
        except ValueError:
            pass
            
    for line in open('evidence.db'):
        try:
            clauses.append(Clause(line=line))
        except ValueError:
            pass


    theory = Theory(clauses)
    theory.delete_zero_weighted()
    #print theory

    #print theory.constants()

    theory.find_equiv()
