import mathlink

class cache(object):
    '''the base type that is used to lookup the particular classdef'''
    class state:
        id = {}         # mathematica id -> ika type
        type = {}       # python type -> ika type

    @classmethod
    def byid(cls, id):
        '''search the current cache by a python-mathlink id'''
        return cls.state.id[id]

    @classmethod
    def bytype(cls, type):
        '''search the current cache for a hash of a python type'''
        return cls.state.type[type]

    @classmethod
    def cons(cls, value):
        '''responsible for converting a python type into an ika-derived type'''
        newcls = cls.bytype(value.__class__)
        return newcls(value)

    @classmethod
    def register(cls, type=(), id=()):
        def __register_closure(definition):
            if definition in cls.state.type:
                raise KeyError('type <%s> is already registered'%repr(definition))
            mathids = (id,) if not hasattr(id,'__iter__') else id
            classes = (type,) if not hasattr(type, '__iter__') else type

            # sanity checks
            regged_ids = set(cls.state.id.keys())
            regged_types = set(cls.state.type.keys())
            for i in regged_ids.intersection(set(mathids)):
                raise KeyError('type-id <%d> is already registered'%i)
            for t in regged_types.intersection(set(classes)):
                raise KeyError('type <%s> is already registered'%repr(t))
            if definition in cls.state.type:
                raise KeyError('type <%s> is already registered'%repr(definition))

            # writes
            cls.state.type[definition] = definition
            cls.state.id.update((id,definition) for id in mathids)
            cls.state.type.update((type,definition) for type in classes)
            return definition
        return __register_closure

class ika(object):
    '''base type for resolving the operations between an object and a non-ika object'''
    def __radd__(self, operand):
        return mf.Plus(operand, self)
    def __add__(self, operand):
        return mf.Plus(self, operand)
    def __rsub__(self, operand):
        return mf.Subtract(operand, self)
    def __sub__(self, operand):
        return mf.Subtract(self, operand)
    def __rmul__(self, operand):
        return mf.Times(operand, self)
    def __mul__(self, operand):
        return mf.Times(self, operand)
    def __rdiv__(self, operand):
        return mf.Quotient(operand, self)
    def __div__(self, operand):
        return mf.Quotient(self, operand)
    def __or__(self, operand):
        return mf.BitOr(self, operand)
    def __and__(self, operand):
        return mf.BitAnd(self, operand)
    def __xor__(self, operand):
        return mf.BitXor(self, operand)

    def send(self, link): raise NotImplementedError(self.__class__)
    @classmethod
    def recv(self, link): raise NotImplementedError(self.__class__)

    def __init__(self, value):
        if issubclass(value.__class__, ika):
            self.value = value.value
            return
        self.value = value

    def get(self):
        return self.value
    def set(self, value):
        result,self.value = self.value,value
        return result
    def name(self):
        return self.value
    def __repr__(self):
        return '<%s.%s %s>'%(self.__module__, self.__class__.__name__, self.repr())
    
@cache.register((),35)
class symbol(ika):
    def repr(self):
        return '%s'% self.value

    @classmethod
    def recv(cls, link):
        return cls(link.getsymbol())
    def send(self, link):
        return link.putsymbol(self.value)

@cache.register(str,34)
class string(ika):
    def repr(self):
        return '"%s"'% self.value
    @classmethod
    def recv(cls, link):
        return cls(link.getstring())
    def send(self, link):
        return link.putstring(self.value)

@cache.register((int,long),(42,43) )
class number(ika):
    def repr(self):
        return '%d'% self.value
        if type(self.value) is float:
            return '%f'% self.value
        return '%d'% self.value
    @classmethod
    def recv(cls, link):
        return cls(link.getnumber())
    def send(self, link):
        return link.putnumber(self.value)

@cache.register((),70)
class function(ika):
    sequence = ()
    def __init__(self, value):
        if isinstance(value,function):
            self.value = value.value
            self.sequence = value.sequence
            return
        super(function,self).__init__(value)

    def set(self, *sequence):
        sequence = tuple(cache.bytype(x.__class__)(x) for x in sequence)
        result,self.sequence = self.sequence,sequence
        return result
    def get(self):
        return self.sequence

    def __call__(self, *sequence):
        # convert sequence into ika-types
        sequence = tuple(cache.cons(x) for x in sequence)

        result = function(self.value)
        result.sequence = sequence
        return result

    def repr(self):
        seq = ','.join(x.repr() for x in self.sequence)
        return '%s [%s]'%(str(self.value),seq)

    @classmethod
    def recv(cls, link):
        name,count = link.getfunction()
        result = cls(name)
        result.sequence = tuple(cache.byid(link.gettype()).recv(link) for _ in xrange(count))
        return result
    def send(self, link):
        result = link.putfunction(self.value, len(self.sequence))
        [x.send(link) for x in self.sequence]
        return result

class instance(object):
    def __init__(self, **kwds):
        raise NotImplementedError("this should be upgraded for doing parallel computations")

        self.env = mathlink.env()

        # FIXME: i can probably use this same class to run/manage mathlink instances in parallel
        argv0 = kwds.pop('argv0', '<none>')
        if 'create' in kwds:
            name = kwds.pop('create')
            argv = [argv0, "-mathlink", "-linkcreate", "-linkname", name]
        elif 'connect' in kwds:
            name = kwds.pop('connect')
            argv = [argv0, "-mathlink", "-linkconnect", "-linkname", name]
        else:
            raise UserError("%s : please specify the 'create' or 'connect' keywords"%(self.__class__.__module__, self.__class__.__name__))
        self.link = self.env.openargv(argv)

    def send(self, expr, **kwds):
        if not isinstance(expr, ika):
            expr = cache.cons(expr)
        result = expr.send(self.link)
        _=self.flush()
        return result

    def recv(self, **kwds):
        if self.link.ready():
            t = self.link.gettype()
            cls = cache.byid(t)
            return cls.recv(self.link)
        raise Exception("%s.%s : link is not ready"%(self.__class__.__module__, self.__class__.__name__))

    def flush(self):
        return self.link.flush()
    def ready(self):
        return self.link.ready()
    def close(self):
        return self.link.close()

### standard operators
class MFGen(object):
    def __getattr__(self, name):
        return function(name)

mf = MFGen()

class SymGen(object):
    def __getattr__(self, name):
        return symbol(name)

sym = SymGen()

if __name__ == '__main__':
    import pyml,mathlink
    reload(pyml)

    a = pyml.instance(create="mylink")
#    a = pyml.instance(connect="57005@172.22.22.142")
    print a.ready()
    #a.flush()

    print a.send( pyml.mf.Set(pyml.sym.x, 15) )

    x = pyml.number(5) 
    y = a.send((x+5*20*(5+4)-1)^2)
    print y

    if False:
        a.put(fac(5))
        print a.read

        print a.link.ready()
        a.link.flush()
        t = a.link.gettype()
        cls = pyml.cache.byid(t)
        x = cls.recv(a.link)
        print x

        print a.link.featurestring()
        #print b.deviceinformation(2)

        b.putsymbol('x')
        b.flush()

    mathcode ='''
        a = LinkCreate["mylink"]
        status := {LinkConnectedQ[a],LinkReadyQ[a]}
        count = 0
        next := While[ LinkReadyQ[a], LinkRead[a]; count++ ]
    '''

    if False:
        import re, pyml, database,function

        SetDelayed = pyml.function("SetDelayed")
        Set = pyml.function("Set")
        List = pyml.function("List")
        Hold = pyml.function("Hold")
        RuleDelayed = pyml.function("RuleDelayed")
        Rule = pyml.function("Rule")

        call = pyml.function("call")
        symcall = pyml.function("symcall")
        
        rules = []
        max = len(database.functions())
        for i,ea in enumerate(database.functions()):
            result = []
            for l,r in function.chunks(ea):
                for x in database.iterate(l,r):
                    address,size,insn = x,idc.ItemSize(x),idc.GetDisasm(x)
                    insn = re.sub(" +", " ", insn)
                    insn = insn.replace("short", "")
                    insn = insn.replace("dword", "")
                    insn = insn.replace("offset", "")
                    insn = insn.replace("large", "")
                    insn = insn.replace("ptr", "")
                    insn = insn.replace("[", "")
                    insn = insn.replace("]", "")
                    if " " in insn:
                        insn = insn.replace(" ", "|", 1)
                        insn = insn.replace(",", "|")
                        insn = insn.replace(" ", "")
                    fn = insn.split("|")
                    name = fn.pop(0)
                    expr = pyml.function(name)(*(pyml.cache.cons(x) for x in fn))
                    #   print len(result),instruction(address,size,expr)
                    result.append(pyml.function("instruction")(address,size,expr))

            name = pyml.symbol(function.name(ea))
            rules.append(Rule(call(name), List(*result)))
            print '%x : %d of %d : %s'%(ea, i, max, call(name))

        x = List(*rules)
        z.send(SetDelayed(pyml.symbol('module'), x))
        print 'done'
