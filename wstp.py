import builtins, os, operator, math, functools, itertools, sys, threading
import weakref, ctypes, logging, libwstp

class utils(object):
    '''basic utils for dealing with results from ctypes.'''
    def __init__(self, object, target, transform, *release):
        self.__objects, self.__target, self.__transform = object if isinstance(object, list) else [object], target, transform if callable(transform) else lambda item: item
        iterable = iter(release)
        [self.__release, iterable] = next(iterable, None), iterable
        self.__release_args = [arg for arg in iterable]

    def __enter__(self):
        objects, target, Ftransform = self.__objects, self.__target, self.__transform
        contents = [ctypes.cast(object, target).contents for object in objects]
        return Ftransform(*contents)

    def __exit__(self, exc_type, exc_value, traceback):
        objects, Frelease, args = self.__objects, self.__release, self.__release_args
        if callable(Frelease):
            Frelease(*itertools.chain(objects, self.__release_args))
        return

    @classmethod
    def __pointer_pointers_thing(cls, object, length, transform, release):
        target = ctypes.POINTER(ctypes.c_void_p * getattr(length, 'value', length))
        return cls(object, target, transform, *release)

    @classmethod
    def pstrings(cls, object, length, *callable):
        def strings(object):
            return [ctypes.string_at(item) for item in object]
        return cls.__pointer_pointers_thing(object, length, strings, callable)

    @classmethod
    def pwstrings(cls, object, length, *callable):
        def wstrings(object):
            raise NotImplementedError   # FIXME
            return [ctypes.string_at(item) for item in object]
        return cls.__pointer_pointers_thing(object, length, wstrings, callable)

    @classmethod
    def __pointer_things(cls, object, target, length, release):
        target = ctypes.POINTER(target * getattr(length, 'value', length))
        def items(object):
            return [item.contents for item in object]
        return cls(object, target, items, *release)

    @classmethod
    def psingles(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_float, length, callable)

    @classmethod
    def pdoubles(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_double, length, callable)

    @classmethod
    def psint16s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_short, length, callable)

    @classmethod
    def puint16s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_ushort, length, callable)

    @classmethod
    def psint32s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_int, length, callable)

    @classmethod
    def puint32s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_uint, length, callable)

    assert(ctypes.sizeof(ctypes.c_long) == 8)
    @classmethod
    def psint64s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_long, length, callable)

    assert(ctypes.sizeof(ctypes.c_ulong) == 8)
    @classmethod
    def puint64s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_ulong, length, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def psint128s(cls, object, length, *callable):
        return cls.__pointer_things(object, ctypes.c_ulongdouble, length, callable)
        
    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def puint128s(cls, object, length, *callable):
        # FIXME: need to manually create a structure here because...ctypes.
        return cls.__pointer_things(object, ctypes.c_ulongdouble, length, callable)

    @classmethod
    def __pointer_thing(cls, object, target, release):
        return cls(object, target, None, *release)

    @classmethod
    def pstring(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_void_p)
        def string(object):
            return ctypes.string_at(object)
        return cls(object, target, string, *callable)

    @classmethod
    def pwstring(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_void_p)
        def wstring(object):
            raise NotImplementedError   # FIXME
        return cls(object, target, wstring, *callable)

    @classmethod
    def puint16(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_ushort)
        return cls.__pointer_thing(object, target, callable)
    @classmethod
    def psint16(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_short)
        return cls.__pointer_thing(object, target, callable)

    @classmethod
    def puint32(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_uint)
        return cls.__pointer_thing(object, target, callable)
    @classmethod
    def psint32(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_int)
        return cls.__pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_ulong) == 8)
    @classmethod
    def puint64(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_ulong)
        return cls.__pointer_thing(object, target, callable)
    assert(ctypes.sizeof(ctypes.c_long) == 8)
    @classmethod
    def psint64(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_long)
        return cls.__pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def puint128(cls, object, *callable):
        # FIXME: need to manually create a structure here because...ctypes.
        target = ctypes.POINTER(ctypes.c_ulong)
        return cls.__pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def psint128(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_long)
        return cls.__pointer_thing(object, target, callable)

    @classmethod
    def psingle(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_float)
        return cls.__pointer_thing(object, target, callable)

    @classmethod
    def pdouble(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_double)
        return cls.__pointer_thing(object, target, callable)

iterable = ((attribute, getattr(libwstp, attribute)) for attribute in dir(libwstp) if attribute.startswith('WSE'))
_WSErrors = {integer : name for name, integer in iterable if isinstance(integer, int)}
def WSError(integer):
    description = _WSErrors.get(integer)
    return "error[{:d}, {:s}]".format(integer, description) if description else "error[{:d}]".format(integer)

class InternalEnvironment(object):
    _state = None
    _lock, _links, _rlinks = threading.Lock(), None, weakref.WeakValueDictionary()

    @classmethod
    def initialized(cls):
        return cls._state is not None
    
    @classmethod
    def get(cls):
        if cls._state is None:
            raise UserWarning("environment is not initialized")
        return cls._state

    @classmethod
    def initialize(cls, parameters=None):
        assert((cls._state is None) and not(cls._links))

        params = libwstp.WSNewParameters(libwstp.WSREVISION, libwstp.WSAPI4REVISION) if parameters is None else parameters
        try:
            res = libwstp.WSSetEncodingParameter(params, libwstp.MLUTF8_ENC)
            if res != libwstp.WSEOK:
                raise RuntimeError("unable to set encoding type ({:s})".format(WSError(res)))
            res = libwstp.WSSetThreadSafeLinksParameter(params) 
            if res != libwstp.WSEOK:
                raise RuntimeError("unable to enable thread-safety ({:s})".format(WSError(res)))
        finally:
            libwstp.WSReleaseParameters(params) if parameters else None

        try:
            environment = libwstp.WSInitialize(params)
            if not environment:
                raise SystemError('unable to initialize WSTP environment')
        finally:
            libwstp.WSReleaseParameters(params) if parameters else None
        cls._state, cls._links = environment, {} if cls._links is None else cls._links
        return cls.get()

    @classmethod
    def deinitialize(cls):
        assert((cls._state is not None) and (cls._links is not None))

        environment = cls.get()
        with cls._lock:
            count = len(cls._links)
            if cls._links:
                ok, _ = False, logging.warning("unable to deinitialize environment due to {:d} link still open.".format(count, '' if count == 1 else 's'))
            else:
                ok, _ = True, libwstp.WSDeinitialize(environment)
            cls._state = None if ok else cls._state
        return ok

    _link_protocols = {
        'tcpip': 'TCPIP',
        'pipes': 'Pipes',
        'shared': 'SharedMemory',
        'intra': 'IntraProcess',
        'filemap': 'FileMap',
        'tcp': 'TCP',
    }

    @classmethod
    def __link(cls, name, protocol, type):
        assert(cls._links is not None)
        assert(type.lower() in {'connect', 'create', 'launch'})
        assert(protocol.lower() in cls._link_protocols)

        parameters = {'-linkprotocol': cls._link_protocols[protocol.lower()]}
        parameters.setdefault('-name', name) if name else None
        iterable = itertools.chain(["-link{:s}".format(type.lower())], *[(key, value) for key, value in parameters.items()])
        argv = [arg for arg in iterable]

        environment, error = cls.get(), ctypes.c_int(libwstp.WSEUNKNOWN)
        with cls._lock:
            index = 1 + len(cls._links)
            assert(index not in cls._links)

            link = libwstp.WSOpenString(environment, ' '.join(argv), ctypes.byref(error))
            if not(link) or error != libwstp.WSEOK:
                raise SystemError("unable to {:s} link{:s} with protocol {:s} ({:s}).".format(type.lower(), " \"{:s}\"".format(name) if name else '', parameters['-linkprotocol'], WSError(error)))
            cls._links[index] = link

        identifier = libwstp.WSToLinkID(link)
        cls._rlinks[identifier], description = index, libwstp.WSLinkName(link)
        logging.warning("created link #{:d} ({:#x}) with name \"{:s}\" and returning it as {!s}.".format(index, identifier, description.contents, link))
        return link

    @classmethod
    def connect(cls, protocol, name=None):
        return cls.__link(name, protocol, 'connect')
    @classmethod
    def create(cls, protocol, name=None):
        return cls.__link(name, protocol, 'create')
    @classmethod
    def launch(cls, protocol, name=None):
        return cls.__link(name, protocol, 'launch')

    @classmethod
    def unlink(cls, wslink):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)

        if identifier in cls._rlinks:
            available, index = len(cls._rlinks), cls._rlinks[identifier]
            return cls.__unlink(index)

        logging.error("unable to find link by its identifier {:s}.".format("{:#x}".format(identifier) if isinstance(wslink, int) else "{:#x} ({!s})".format(identifier, wslink)))
        return -1

    @classmethod
    def __unlink(cls, index):
        if index not in cls._links:
            logging.error("unable to find link #{:d}.".format(index))
            return -1

        with cls._lock:
            wslink = cls._links[index]
            identifier, name = libwstp.WSToLinkID(wslink), libwstp.WSLinkName(wslink)
            libwstp.WSClose(wslink)
            logging.warning("closed link #{:d} ({:#x}) with name \"{:s}\" ({!s}).".format(index, identifier, name.contents, cls._links.pop(index)))
        return len(cls._links)

    @classmethod
    def fetch(cls, wslink):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        if identifier not in cls._rlinks:
            raise KeyError("unable to find link by its identifier {:s}.".format("{:#x}".format(identifier) if isinstance(wslink, int) else "{:#x} ({!s})".format(identifier, wslink)))

        index = cls._rlinks[identifier]
        return cls._links[index]

    @classmethod
    def exists(cls, wslink):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        return identifier in cls._rlinks

    @classmethod
    def available(cls):
        return {identifier for identifier in cls._rlinks}

def initialize(*args, **kwargs):
    if InternalEnvironment.initialized():
        raise UserWarning("environment already initialized")
    return InternalEnvironment.initialize(*args, **kwargs)

def deinitialize():
    if not InternalEnvironment.initialized():
        raise UserWarning("environment is not initialized")
    return InternalEnvironment.deinitialize()

class Environment(object):
    def __init__(self):
        self.__environment = InternalEnvironment.get()

    def __create_link(self, environment_method, *args, **kwargs):
        link = environment_method(*args, **kwargs)
        identifier, name = libwstp.WSToLinkId(link), libwstp.WSLinkName(link)
        return self.__new_reference_link(identifier)

    def __new_reference_link(self, identifier):
        get, release = (functools.partial(F, identifier) for F in [self.__get_temporary_link, self.__release_temporary_link])
        return WSLink(identifier, get, release)
    def __get_temporary_link(self, identifier):
        return self.__environment.fetch(identifier)
    def __release_temporary_link(self, identifier):
        count = self.__environment.unlink(identifier) 
        if count < 0:
            raise RuntimeError("unable to release link with the given identifier ({:#x}).".format(identifier))
        return 

    def connect(self, *args, **kwargs):
        method = self.__environment.connect
        return self.__create_link(method, *args, **kwargs)
    def launch(self, *args, **kwargs):
        method = self.__environment.launch
        return self.__create_link(method, *args, **kwargs)
    def create(self, *args, **kwargs):
        method = self.__environment.create
        return self.__create_link(method, *args, **kwargs)
    def destroy(self, identifier):
        count = self.__environment.unlink(identifier)
        if count >= 0:
            logging.warning("destroyed link {:#x}...{:s}.".format(identifier, "{:s}{:d} left".format('only' if count == 1 else '', count) if count else 'none'))
        return 0 <= count
    def close(self):
        ok, destroyed = True, {}
        for identifier in self.__environment.available():
            result = destroyed[identifier] = self.destroy(identifier)
            ok = result and ok

        if not ok:
            items = {identifier for identifier, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy link{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))
        
        return InternalEnvironment.deinitialize()
        
    
# wstp32i4m.lib or wstp64i4m.lib
# libWSTP32i4.a, libWSTP32i4.so, libWSTP64i4.a, and libWSTP64i4.so.
# libWSTPi4.a.

if __name__ == '__main__':
    import ctypes, wstp, libwstp, importlib
    importlib.reload(libwstp)
    importlib.reload(wstp)

    #environment = wstp.initialize()
    env = wstp.initialize()
    x = wstp.deinitialize()
    
    #wstp.InternalEnvironment._state = ctypes.cast(env, ctypes.POINTER(libwstp.struct_ml_environment))
    #print(wstp.InternalEnvironment._state)
    #print(ctypes.cast(env, ctypes.POINTER(libwstp.struct_ml_environment)))
    #x = wstp.deinitialize()
    #wstp.self = wstp.InternalEnvironment
    #x = wstp.InternalEnvironment.get()
    #x = wstp.InternalEnvironment._links = {}
    #print(wstp.InternalEnvironment._links)
    #print(env

    env = wstp.initialize()
    p(wstp.InternalEnvironment.initialized())

    protocolnames_t = ctypes.POINTER(ctypes.POINTER(ctypes.c_char))
    length = ctypes.c_int(0)
    protocolnames = protocolnames_t()

    p(ctypes.sizeof(t))
    t = ctypes.c_double(1.3333333333333333333)
    with wstp.utils.pdouble(ctypes.pointer(t)) as x:
        p(x)

    with wstp.utils.pstrings(protocolnames, length, functools.partial(libwstp.WSReleaseLinkProtocolNames, env), length) as x:
        p(x)

    #p([hex(x) for x in x])
    #target = ctypes.POINTER(ctypes.c_void_p * length.value)
    #p(ctypes.POINTER(ctypes.c_int) * 5)
    #x = ctypes.cast(protocolnames, target)
    #p(protocolnames.contents.contents)
    #for item in x.contents:
    #    p(ctypes.string_at(item))

    #x = bytes.fromhex('6f72506172746e49')

    #p(x.contents[0])
    err = libwstp.WSGetAvailableLinkProtocolNames(env, ctypes.byref(protocolnames), ctypes.byref(length))
    assert(err == libwstp.WSEOK)
    ok = libwstp.WSReleaseLinkProtocolNames(env, protocolnames, length)
    x = libwstp.WSGetAvailableLinkProtocolNames.argtypes[1]
    p(x.contents)

    p(protocolnames.contents)

    # MLDECL(int, MLGetAvailableLinkProtocolNames, (MLEnvironment ep, char ***protocolNames, int *length));
    # MLDECL(void,     MLReleaseLinkProtocolNames, (MLEnvironment ep, char **protocolNames, int length));

