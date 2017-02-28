import re

fact_pattern = re.compile(r'^(\w+)\((.*)\)\s*$')
clause_pattern = re.compile(r'^([-\d+.]+)\s+(.*)')


class MercuryProgram:

    def __init__(self):
        self._clauses = []


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
    
    def __init__(self,lits=None):
        if lits is None:
            self._lits = []
        else:
            self._lits = lits

    def add_lit(self,lit):
        self._lits.append(lit)

    def get_vars(self):
        vs = set()
        for lit in self._lits:
            vs.update(lit.get_vars())
        return vs
        
    def make_weighted(self,weight):
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
        return ';'.join([str(x) for x in self._lits])
            
class Lit:

    def __init__(self,psym,args=None,negated=False):
        self._psym = psym
        self._negated = negated
        if args is None:
            self._args = ()
        else:
            self._args=tuple(args)

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
            
class Variable:

    def __init__(self,varname):
        self._varname = varname

    def get_vars(self):
        return set([self._varname])
        
    def __str__(self):
        return self._varname
            
class NonVarTerm:

    def __init__(self,fsym=None,args=None):
        self._fsym = fsym
        if args is None:
            self._args = ()
        else:
            self._args = tuple(args)

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

def clause_from_line(line):
    '''return a clause and its weight from a tuffy-style line
    or none if that does not work
    '''
    clausematch = clause_pattern.match(line)
    if clausematch is None:
        return None

    lits = clausematch.group(2).split(' v ')
    clause = Clause()
    for lit in lits:
        if lit[0] == '!':
            negated = True
            lit = lit[1:]
        else:
            negated = False
        match = fact_pattern.match(lit)
        if match is None:
            raise IOError
        outargs = []
        for arg in match.group(2).split(','):
            if arg[0].islower():
                outargs.append(Variable(arg.capitalize()))
            else:
                outargs.append(NonVarTerm('"{0}"'.format(arg)))
        clause.add_lit(Lit(match.group(1),outargs,negated))
    return clause, float(clausematch.group(1))

def fact_from_line(line):
    '''return a ground fact from a tuffy-style line
    or none if that does not work
    everything is converted into a string
    '''
    match = fact_pattern.match(line)
    if match is None:
        return None
    args = [NonVarTerm('"'+x+'"') for x in match.group(2).replace(' ','').split(',')]
    psym = match.group(1)
    return Clause(Lit(psym,args))


if __name__ == '__main__':
    for line in open('prog.mln'):
        if 'EXIST' not in line and clause_pattern.match(line) is not None:
            clause, w = clause_from_line(line)
            wclauses = clause.make_weighted(w)
            for wc in wclauses:
                print wc

