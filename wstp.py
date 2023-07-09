import builtins, os, operator, math, functools, itertools, sys, threading
import gc, weakref, codecs, ctypes, logging, libwstp

#def WSError(integer):
#    description = _WSErrors.get(integer)
#    return "error[{:d}, {:s}]".format(integer, description) if description else "error[{:d}]".format(integer)

### exception types
class WSTPException(Exception):
    iterable = ((attribute, getattr(libwstp, attribute)) for attribute in dir(libwstp) if attribute.startswith('WSE'))
    _WSErrorMap = {integer : name for name, integer in iterable if isinstance(integer, int)}
    del(iterable)

    @classmethod
    def get(cls, code, *args):
        return cls._WSErrorMap.get(code, *args) if args else cls._WSErrorCodes[code]

    @classmethod
    def string(cls, code):
        description = cls._WSErrorMap.get(code, cls._WSErrorMap[libwstp.WSEUNKNOWN])
        return "{:s}({:d})".format(description, code)

class WSTPSystemError(WSTPException, SystemError):
    pass

class WSTPErrorCode(WSTPException):
    def __init__(self, code):
        self.code, string = code, self.string(code)
        self.args = code, string

class WSTPErrorMessage(WSTPErrorCode):
    def __init__(self, code, description, *args, **kwargs):
        self.code, string = code, self.string(code)

        kwargs.setdefault('code', code), kwargs.setdefault('error', string)
        self.args = code, string, description.format(*args, **kwargs)

class WSTPEnvironmentError(WSTPSystemError):
    def __init__(self, environment, code):
        assert(isinstance(environment, libwstp.WSEnvironment))
        self.code, string, message = code, self.string(code), self.message(environment, code)
        self.args = code, string, message

    @staticmethod
    def message(environment, err):
        string = libwstp.WSErrorString(environment, err)
        return string.decode('ascii')

class WSTPEnvironmentErrorMessage(WSTPEnvironmentError):
    def __init__(self, environment, code, description='', *args, **kwargs):
        assert(isinstance(environment, libwstp.WSEnvironment))
        self.code, string, message = code, self.string(code), self.message(environment, code)
        kwargs.setdefault('code', code), kwargs.setdefault('error', string), kwargs.setdefault('message', message)

        self.args = code, string, description.format(*args, **kwargs) if description else message

### ctypes utilities
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

    @staticmethod
    def clone(object):
        pointer = ctypes.cast(ctypes.pointer(object), ctypes.c_void_p)
        result = ctypes.cast(ctypes.c_void_p(pointer.value), ctypes.POINTER(object.__class__))
        return result.contents

    @staticmethod
    def cast(object, target):
        pointer = ctypes.cast(ctypes.pointer(object), ctypes.c_void_p)
        result = ctypes.cast(ctypes.c_void_p(pointer.value), ctypes.POINTER(target))
        return result.contents

    @staticmethod
    def argument(func, number):
        assert(isinstance(func, ctypes._CFuncPtr))
        res = func.argtypes[number]
        assert(issubclass(res, ctypes._Pointer))
        return res._type_

    @classmethod
    def arguments(cls, func, number):
        assert(isinstance(func, ctypes._CFuncPtr))
        argtypes = func.argtypes
        if isinstance(number, slice):
            listable = [argtypes[index] for index in range(*number.indices(len(argtypes)))]
            assert(all(issubclass(arg, ctypes._Pointer) for arg in listable))
            return tuple(arg._type_ for arg in listable)
        return cls.arguments(func, slice(None, number) if number >= 0 else slice(number, None))

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

# FIXME: there's absolutely no reason for this to be a singleton, as we really
#        only need some place to store references to initialized environments
class InternalEnvironment(object):
    _lock, _state, _finalizers = threading.Lock(), weakref.WeakValueDictionary(), {}

    def __new__(cls):
        pass

    _encodings = {
        codecs.lookup('ascii'): libwstp.MLASCII_ENC,
        codecs.lookup('utf8'): libwstp.MLUTF8_ENC,
        codecs.lookup('utf16'): libwstp.MLUTF16_ENC,
        codecs.lookup('utf32'): libwstp.MLUTF32_ENC,

        'bytes': libwstp.MLBYTES_ENC, bytes: libwstp.MLBYTES_ENC, None: libwstp.MLBYTES_ENC,
        'ucs2': libwstp.MLUCS2_ENC,
        'old': libwstp.MLOLD_ENC,
    }

    @classmethod
    def create(cls, encoding, parameters=None):
        enc = cls._encodings[encoding] if encoding in cls._encodings else cls._encodings[codecs.lookup(encoding)]
        params = libwstp.WSNewParameters(libwstp.WSREVISION, libwstp.WSAPI4REVISION) if parameters is None else parameters
        try:
            res = libwstp.WSSetEncodingParameter(params, enc)
            if res != libwstp.WSEOK:
                raise WSTPErrorMessage(res, "unable to set encoding type ({:d})".format(enc))
            res = libwstp.WSSetThreadSafeLinksParameter(params)
            if res != libwstp.WSEOK:
                raise WSTPErrorMessage(res, 'unable to enable thread-safety')
        finally:
            libwstp.WSReleaseParameters(params) if parameters else None

        try:
            environment = libwstp.WSInitialize(params)
            if not environment:
                raise WSTPSystemError('unable to initialize WSTP environment')
        finally:
            libwstp.WSReleaseParameters(params) if parameters else None

        address = ctypes.addressof(environment.contents)
        assert(not(address in cls._state))

        finalizer = cls._finalizers.pop(address, None)
        assert(finalizer is None or not(finalizer.alive))

        with cls._lock:
            cls._state[address] = environment
            cls._finalizers[address] = weakref.finalize(environment, cls.destroy, utils.cast(environment, libwstp.WSEnvironment))
        logging.warning("created environment at {:#x}.".format(ctypes.addressof(environment.contents)))
        return Environment(environment)

    @classmethod
    def destroy(cls, environment):
        logging.warning("destroying environment at {:#x}.".format(ctypes.addressof(environment.contents)))
        libwstp.WSDeinitialize(environment)

    ### each of the following dicts are used for releasing objects associated with an environment

    ## WSLink
    _links = {}
    @classmethod
    def attach_links(cls, environment, links):
        address = id(links)
        assert(address not in cls._links)
        cls._links[address] = weakref.finalize(environment, cls.__finish_links, ctypes.cast(environment, libwstp.WSEnvironment), links)

    @classmethod
    def __finish_links(cls, environment, links):
        ok, destroyed = True, {}
        for identifier, link in links.items():
            result = destroyed[identifier] = cls.destroy_link(environment, identifier)
            ok = result and ok

        if not ok:
            items = {identifier for identifier, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy link{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))

        return ok

    @classmethod
    def destroy_link(cls, environment, identifier):
        wslink = libwstp.WSFromLinkID(environment, identifier)
        if not wslink:
            logging.warning("unable to close link {:#x}.".format(identifier))
            return False

        name = libwstp.WSLinkName(wslink)
        libwstp.WSClose(wslink)
        logging.warning("closed link {:#x} with name \"{:s}\".".format(identifier, name.decode('ascii')))
        return True

    ## WSLinkServer
    _servers = {}
    @classmethod
    def attach_servers(cls, environment, servers):
        address = id(servers)
        assert(address not in cls._servers)
        cls._servers[address] = weakref.finalize(environment, cls.__finish_servers, ctypes.cast(environment, libwstp.WSEnvironment), servers)

    @classmethod
    def __finish_servers(cls, environment, servers):
        ok, destroyed = True, {}
        for identifier, server in servers.items():
            result = destroyed[identifier] = cls.destroy_server(environment, server)
            ok = result and ok

        if not ok:
            items = {identifier for identifier, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy server{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))

        return ok

    @classmethod
    def destroy_server(cls, server):
        libwstp.WSShutdownLinkServer(server)
        logging.warning("shutdown server {!r}.".format(server))
        return True

    ## WSLinkServiceBrowser
    _browsers = {}
    @classmethod
    def attach_browsers(cls, environment, browsers):
        address = id(browsers)
        assert(address not in cls._browsers)
        cls._browsers[address] = weakref.finalize(environment, cls.__finish_browsers, ctypes.cast(environment, libwstp.WSEnvironment), browsers)

    @classmethod
    def __finish_browsers(cls, environment, browsers):
        ok, destroyed = True, {}
        for address, serviceRef in browsers.items():
            result = destroyed[address] = cls.destroy_browser(environment, serviceRef)
            ok = result and ok

        if not ok:
            items = {identifier for identifier, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy browser{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))

        return ok

    @classmethod
    def destroy_browser(cls, environment, serviceRef):
        libwstp.WSStopBrowsingForLinkServices(environment, serviceRef)
        logging.warning("stopped browser {!r}.".format(serviceRef))
        return True

    ## WSLinkServiceResolver
    _resolvers = {}
    @classmethod
    def attach_resolvers(cls, environment, resolvers):
        address = id(resolvers)
        assert(address not in cls._resolvers)
        cls._resolvers[address] = weakref.finalize(environment, cls.__finish_resolvers, ctypes.cast(environment, libwstp.WSEnvironment), resolvers)

    @classmethod
    def __finish_resolvers(cls, environment, resolvers):
        ok, destroyed = True, {}
        for address, serviceRef in resolvers.items():
            result = destroyed[address] = cls.destroy_resolver(environment, serviceRef)
            ok = result and ok

        if not ok:
            items = {address for address, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy resolver{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))

        return ok

    @classmethod
    def destroy_resolver(cls, environment, serviceRef):
        libwstp.WSStopResolvingLinkService(environment, serviceRef)
        logging.warning("stopped resolver {!r}.".format(serviceRef))
        return True

    ## WSLinkService
    _services = {}
    @classmethod
    def attach_services(cls, environment, services):
        address = id(services)
        assert(address not in cls._services)
        cls._services[address] = weakref.finalize(environment, cls.__finish_services, ctypes.cast(environment, libwstp.WSEnvironment), services)

    @classmethod
    def __finish_services(cls, environment, services):
        ok, destroyed = True, {}
        for address, serviceRef in services.items():
            result = destroyed[address] = cls.destroy_service(environment, serviceRef)
            ok = result and ok

        if not ok:
            items = {address for address, result in destroyed.items() if not result}
            assert(items)
            logging.fatal("unable to destroy service{:s} {:s}.".format('' if len(items) == 1 else 's', ', '.join(itertools.chain(map("{:#x}".format, items[:-1]), ["and {:#x}".format(*items[-1:])])) if len(items) > 1 else "{:#x}".format(*items)))
            logging.warning("ignoring previous {:d} error{:s} and deinitializing the environment anyways...".format(len(items), '' if len(items) == 1 else 's'))

        return ok

    @classmethod
    def destroy_service(cls, environment, serviceRef):
        libwstp.WSStopRegisteringLinkService(environment, serviceRef)
        logging.warning("stopped service {!r}.".format(serviceRef))
        return True

class Environment(object):
    def __init__(self, environment):
        self._environment, self._finalizers, self._contextrefs = environment, {}, {}

        self._links, self._servers = (weakref.WeakValueDictionary() for count in range(2))
        InternalEnvironment.attach_links(environment, self._links)
        InternalEnvironment.attach_servers(environment, self._servers)

        self._browsers, self._resolvers, self._services = (weakref.WeakValueDictionary() for count in range(3))
        InternalEnvironment.attach_browsers(environment, self._browsers)
        InternalEnvironment.attach_resolvers(environment, self._resolvers)
        InternalEnvironment.attach_services(environment, self._services)

    ### settings
    def set_environment_id(self, environment_id):
        '''MLSetEnvIDString'''
        env, environment_id = self._environment, ctypes.c_char_p(environment_id)
        err = libwstp.WSSetEnvIDString(env, environment_id)
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return

    @property
    def environment_data(self):
        '''MLEnvironmentData'''
        res, target = libwstp.WSEnvironmentData(self._environment), ctypes.POINTER(ctypes.py_object)
        return ctypes.cast(res, target).contents.value if res else None
    @environment_data.setter
    def environment_data(self, value):
        '''MLSetEnvironmentData'''
        self.__environment_data = object = ctypes.py_object(value)  # persist a reference
        err = libwstp.WSSetEnvironmentData(self._environment, ctypes.pointer(object))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return

    # random properties that depend on the WSEnvironment type.
    def version(self):
        env, inumb, rnumb, bnumb = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSVersionNumbers, -3)))
        libwstp.WSVersionNumbers(self._environment, ctypes.byref(inumb), ctypes.byref(rnumb), ctypes.byref(bnumb))
        return inumb, rnumb, bnumb

    def compiler_id(self):
        env, id = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSCompilerID, -1)))
        err = libwstp.WSCompilerID(env, ctypes.byref(id))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        with utils.pstring(id, functools.partial(libwstp.WSReleaseCompilerID, self._environment)) as id:
            result = id
        return result

    def ucs2_compiler_id(self):
        env, id, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSUCS2CompilerID, -2)))
        err = libwstp.WSUCS2CompilerID(env, ctypes.byref(id), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        with utils.puint16s(id, length, functools.partial(libwstp.WSReleaseUCS2CompilerID, self._environment), length) as id:
            result = id
        return result

    def utf8_compiler_id(self):
        env, id, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSUTF8CompilerID, -2)))
        err = libwstp.WSUTF8CompilerID(env, ctypes.byref(id), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        # FIXME:
        with utils.pstring(id, length, functools.partial(libwstp.WSReleaseUTF8CompilerID, self._environment), length) as id:
            result = id
        return result

    def utf16_compiler_id(self):
        env, id, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSUTF16CompilerID, -2)))
        err = libwstp.WSUTF16CompilerID(env, ctypes.byref(id), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        with utils.puint16s(id, length, functools.partial(libwstp.WSReleaseUTF16CompilerID, self._environment), length) as id:
            result = id
        return result

    def utf32_compiler_id(self):
        env, id, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSUTF32CompilerID, -2)))
        err = libwstp.WSUTF32CompilerID(env, ctypes.byref(id), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        with utils.puint32s(id, length, functools.partial(libwstp.WSReleaseUTF32CompilerID, self._environment), length) as id:
            result = id
        return result

    ### signal handling
    def set_signal_handler(signum, sigaction):
        '''MLSetSignalHandler'''
        err = libwstp.WSSetSignalHandler(self._environment, signum, sigaction)
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return

    def set_signal_handler_function(signum, sigfunc):
        '''MLSetSignalHandlerFromFunction'''
        err = libwstp.WSSetSignalHandlerFromFunction(self._environment, signum, sigfunc)
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return

    def unset_signal_handler_function(signum, sigfunc):
        '''MLUnsetSignalHandler'''
        err = libwstp.WSUnsetSignalHandler(self._environment, signum, sigfunc)
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return

    ### network interface api
    def get_address_list(self):
        '''MLGetNetworkAddressList/MLReleaseNetworkAddressList'''
        env, size = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetNetworkAddressList, -1)))
        addresses = libwstp.WSGetNetworkAddressList(env, ctypes.byref(size))
        with utils.pstrings(addresses, size, functools.partial(libwstp.WSReleaseNetworkAddressList, env), size) as addresses:
            result = addresses
        return result

    def get_domain_list(self):
        '''MLGetDomainNameList/MLReleaseDomainNameList'''
        env, size = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetDomainNameList, -1)))
        dnsnames = libwstp.WSGetDomainNameList(env, ctypes.byref(size))
        with utils.pstrings(dnsnames, size) as dnsnames:
            result = dnsnames
        return result

    @property
    def available_link_protocol_names(self):
        env, protocolnames, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetAvailableLinkProtocolNames, -2)))
        err = libwstp.WSGetAvailableLinkProtocolNames(env, ctypes.byref(protocolnames), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentErrorMessage(env, err, 'unable to get available link protocol names')
        with utils.pstrings(protocolnames, length, functools.partial(libwstp.WSReleaseLinkProtocolNames, env), length) as items:
            decoded = [item.decode('ascii') for item in items]
        return {string for string in decoded}

    def _available_link_protocol_names(self):
        available, candidates  = self.protocols, {ch for ch in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'}
        original = {proto : proto for proto in available}
        mixed, lower = {}, {proto.lower() : proto for proto in available if proto.upper() == proto}
        for proto in available:
            if proto.upper() == proto or proto.lower() == proto:
                continue
            iterable = (1 + index for index, character in enumerate(proto[1:]) if character in candidates)
            index = next(iterable, 0)
            if index:
                mixed[proto[:index].casefold()] = proto
            continue

        result = {}
        [result.update(items) for items in [original, lower, mixed]]
        return result

    ## WSLink
    def Connect(self, protocol, name=None):
        return self.__create_link(name, protocol, 'connect')
    def Create(self, protocol, name=None):
        return self.__create_link(name, protocol, 'create')
    def Launch(self, protocol, name=None):
        return self.__create_link(name, protocol, 'launch')
    def Close(self, wslink):
        env, identifier = self._environment, wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        link = self._links.pop(identifier, libwstp.WSFromLinkID(env, identifier))
        assert(isinstance(link, libwstp.WSLINK))
        return InternalEnvironment.destroy_link(env, identifier)

    def get_links(self):
        '''MLGetLinksFromEnvironment/MLReleaseLinksFromEnvironment'''
        # FIXME: probably not useful
        env, links, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetLinksFromEnvironment, -2)))
        err = libwstp.WSGetLinksFromEnvironment(env, ctypes.byref(links), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        # FIXME: each of these should be WSLink objects.
        with utils.ptargets(libwstp.MLINK, links, length, functools.partial(libwstp.WSReleaseLinksFromEnvironment, env)) as links:
            result = links
        return result

    def __create_link(self, name, protocol, type):
        parameters.setdefault('-name', name) if name else None

        available = self._available_link_protocol_names()
        assert(available.get(protocol, available.get(protocol.casefold())) is not None)
        selected = available[protocol] if protocol in available else available[protocol.casefold()]
        parameters = {'-linkprotocol': selected}

        assert(type.lower() in {'connect', 'create', 'launch'})
        iterable = itertools.chain(["-link{:s}".format(type.lower())], *[(key, value) for key, value in parameters.items()])
        argv = [arg for arg in iterable]

        env, error = self._environment, utils.argument(libwstp.WSOpenString, -1)(libwstp.WSEUNKNOWN)
        link = libwstp.WSOpenString(env, ' '.join(argv), ctypes.byref(error))
        if not(link) or error.value != libwstp.WSEOK:
            raise WSTPEnvironmentErrorMessage(env, error.value, "unable to {:s} link{:s} with protocol {:s} ({message})", type.lower(), " \"{:s}\"".format(name) if name else '', parameters['-linkprotocol'])

        identifier = libwstp.WSToLinkID(link)
        sanity = self._finalizers.get(identifier, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[identifier] = weakref.finalize(link, InternalEnvironment.destroy_link, env, identifier)
        self._links[identifier] = link
        index = len(self._links)

        description = libwstp.WSLinkName(link)
        logging.warning("created link #{:d} ({:#x}) with name \"{:s}\" and returning it as {!s}.".format(index, identifier, description.decode('ascii'), link))
        return link

    # interacts with links that are cached.
    def get_link(self, wslink):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        if identifier not in self._links:
            raise KeyError("unable to find link by its identifier {:s}.".format("{:#x}".format(identifier) if isinstance(wslink, int) else "{:#x} ({!s})".format(identifier, wslink)))
        return self._links[identifier]

    def exists_link(self, wslink):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        return identifier in self._links

    def available_links(self):
        return {identifier for identifier in self._links}

    ### service discovery and advertisement

    ## WSLinkServiceBrowserref/c/WSStopRegisteringLinkService
    def browse_for_link_services(self, callbackFunction, serviceProtocol, domain, context=None):
        '''MLBrowseForLinkServices'''
        env, ref = (item() for item in itertools.chain([lambda:self._ref/c/WSStopRegisteringLinkService_environment], utils.arguments(libwstp.WSBrowseForLinkServices, -1)))
        context = None if context is None else ctypes.py_object(context)
        err = libwstp.WSBrowseForLinkServices(env, callbackFunction, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(domain), context if context is None else ctypes.pointer(context), ctypes.byref(ref))
        if error.value != libwstp.WSEOK or not res:
            raise WSTPEnvironmentError(env, error.value)
        return self.__create_browser(ref, context)

    def __create_browser(self, ref, context):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address, ctx = ctypes.cast(ref, ctypes.c_long).value, None if context is None else ctypes.cast(context, ctypes.c_long).value

        sanity = self._finalizers.get(address, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[address] = weakref.finalize(ref, InternalEnvironment.destroy_browser, utils.clone(ref))
        self._browsers[address] = ref

        if all(item is not None for item in [context, ctx]):
            self._contextrefs[address] = context
            self._finalizers[ctx] = weakref.finalize(ref, self._contextrefs.pop, address, None)

        index = len(self._browsers)
        logging.warning("created browser #{:d} ({:#x}){:s} and returning it as {!s}.".format(index, address, " with context ({:#x})".format(ctypes.addressof(context.value)) if context else '', browser))
        return WSLinkServiceBrowser(self._environment, server, context.value)

    # interacts with browsers that are cached
    def get_browser(self, ref):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address = ref if isinstance(ref, int) else ctypes.cast(ref, ctypes.c_long).value
        if address not in self._browsers:
            raise KeyError("unable to find browser by its address ({:s}).".format(address))
        res, context = self._browsers[address], self._contextrefs[address]
        return WSLinkServiceBrowser(self._environment, res, context)

    def exists_browser(self, ref):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address = ref if isinstance(server, int) else ctypes.cast(ref, ctypes.c_long).value
        return address in self._browsers

    def available_servers(self):
        return {address for address in self._browsers}

    ## WSLinkServiceResolver
    def resolve_link_service(self, callbackFunction, serviceProtocol, serviceName, context=None):
        '''MLResolveLinkService'''
        env, ref = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSResolveLinkService, -1)))
        context = None if context is None else ctypes.py_object(context)
        err = libwstp.WSResolveLinkService(env, callbackFunction, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(serviceName), context if context is None else ctypes.pointer(context), ctypes.byref(ref))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)
        return self.__create_resolver(ref, context)

    def __create_resolver(self, ref, context):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address, ctx = ctypes.cast(ref, ctypes.c_long).value, None if context is None else ctypes.cast(context, ctypes.c_long).value

        sanity = self._finalizers.get(address, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[address] = weakref.finalize(ref, InternalEnvironment.destroy_resolver, utils.clone(ref))
        self._resolvers[address] = ref

        if all(item is not None for item in [context, ctx]):
            self._contextrefs[address] = context
            self._finalizers[ctx] = weakref.finalize(ref, self._contextrefs.pop, address, None)

        index = len(self._resolvers)
        logging.warning("created resolver #{:d} ({:#x}){:s} and returning it as {!s}.".format(index, address, " with context ({:#x})".format(ctypes.addressof(context.value)) if context else '', resolver))
        return WSLinkServiceResolver(self._environment, resolver, context.value)

    # interacts with resolvers that are cached
    def get_resolver(self, ref):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address = ref if isinstance(ref, int) else ctypes.cast(ref, ctypes.c_long).value
        if address not in self._resolvers:
            raise KeyError("unable to find resolver by its address ({:s}).".format(address))
        resolver, context = self._resolvers[address], self._contextrefs[address]
        return WSLinkServiceResolver(self._environment, resolver, context)

    def exists_resolver(self, ref):
        assert(operator.eq(*map(ctypes.sizeof, [ref, ctypes.c_long])))
        address = ref if isinstance(server, int) else ctypes.cast(ref, ctypes.c_long).value
        return address in self._resolvers

    def available_resolvers(self):
        return {address for address in self._resolvers}

    ## WSLinkServer
    def link_server(self, context=None):
        '''MLNewLinkServer'''
        env, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSNewLinkServer, -1)))
        context = None if context is None else ctypes.py_object(context)
        server = libwstp.WSNewLinkServer(env, context if context is None else ctypes.pointer(context), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        return self.__create_server(server, context)

    def new_link_server_with_port(self, port, context=None):
        '''MLNewLinkServerWithPort'''
        env, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSNewLinkServerWithPort, -1)))
        context = None if context is None else ctypes.py_object(context)
        server = libwstp.WSNewLinkServerWithPort(env, port, context if context is None else ctypes.pointer(context), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        return self.__create_server(server, context)

    def new_link_server_with_port_and_interface(port, iface, context=None):
        '''MLNewLinkServerWithPortAndInterface'''
        env, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSNewLinkServerWithPortAndInterface, -1)))
        context = None if context is None else ctypes.py_object(context)
        server = libwstp.WSNewLinkServerWithPortAndInterface(env, port, iface, context if context is None else ctypes.pointer(context), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        return self.__create_server(server, context)

    def __create_server(self, server, context):
        assert(operator.eq(*map(ctypes.sizeof, [server, ctypes.c_long])))
        address, ctx = ctypes.cast(server, ctypes.c_long).value, None if context is None else ctypes.cast(context, ctypes.c_long).value

        sanity = self._finalizers.get(address, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[address] = weakref.finalize(server, InternalEnvironment.destroy_server, utils.clone(server))
        self._servers[address] = server

        if all(item is not None for item in [context, ctx]):
            self._contextrefs[address] = context
            self._finalizers[ctx] = weakref.finalize(server, self._contextrefs.pop, address, None)

        index = len(self._servers)
        logging.warning("created server #{:d} ({:#x}){:s} and returning it as {!s}.".format(index, address, " with context ({:#x})".format(ctypes.addressof(context.value)) if context else '', server))
        return WSLinkServer(server, context.value)

    # interacts with servers that are cached
    def get_server(self, server):
        assert(operator.eq(*map(ctypes.sizeof, [server, ctypes.c_long])))
        address = server if isinstance(server, int) else ctypes.cast(server, ctypes.c_long).value
        if address not in self._servers:
            raise KeyError("unable to find server by its address ({:s}).".format(address))
        server, context = self._servers[address], self._contextrefs[address]
        return WSLinkServer(res, context)

    def exists_server(self, server):
        assert(operator.eq(*map(ctypes.sizeof, [server, ctypes.c_long])))
        address = server if isinstance(server, int) else ctypes.cast(server, ctypes.c_long).value
        return address in self._servers

    def available_servers(self):
        return {identifier for identifier in self._servers}

    ## WSLinkService
    def register_link_service_with_port_and_hostname(self, serviceProtocol, serviceName, port, hostname, function, domain, context=None):
        '''MLRegisterLinkServiceWithPortAndHostname'''
        env, ref, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSRegisterLinkServiceWithPortAndHostname, -2)))
        context = None if context is None else ctypes.py_object(context)
        mlink = libwstp.WSRegisterLinkServiceWithPortAndHostname(env, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(serviceName), port, ctypes.c_char_p(hostname), function, ctypes.c_char_p(domain), context if context is None else ctypes.pointer(context), ctypes.byref(ref), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        # FIXME
        return mlink, ref, context
    def register_link_service_with_hostname(self, serviceProtocol, serviceName, hostname, function, domain, context=None):
        '''MLRegisterLinkServiceWithHostname'''
        env, ref, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSRegisterLinkServiceWithHostname, -2)))
        context = None if context is None else ctypes.py_object(context)
        mlink = libwstp.WSRegisterLinkServiceWithHostname(env, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(serviceName), ctypes.c_char_p(hostname), function, ctypes.c_char_p(domain), context if context is None else ctypes.pointer(context), ctypes.byref(ref), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        # FIXME
        return mlink, ref, context
    def register_link_service(self, serviceProtocol, serviceName, function, domain, context=None):
        '''MLRegisterLinkService'''
        env, ref, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSRegisterLinkService, -2)))
        context = None if context is None else ctypes.py_object(context)
        mlink = libwstp.WSRegisterLinkService(env, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(serviceName), function, ctypes.c_char_p(domain), context if context is None else ctypes.pointer(context), ctypes.byref(ref), ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        # FIXME
        return mlink, ref, context

    #def register_link_service_using_link_protocol(self, serviceProtocol, serviceName, port, hostname, protocol, function, domain, context, ref, error):
    #    # MLDECL(MLINK, MLRegisterLinkServiceUsingLinkProtocol, (MLEnvironment env, const char *serviceProtocol, const char *serviceName, unsigned short port, const char *hostname, const char *protocol, MLRegisterCallbackFunction function, const char *domain, void *context, MLServiceRef *ref, int *error));
    #    env, ref, error = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSRegisterLinkServiceUsi, -2)))
    #    mlink = libwstp.WSRegisterLinkService(env, ctypes.c_char_p(serviceProtocol), ctypes.c_char_p(serviceName), port, ctypes.c_char_p(hostname), ctypes.c_char_p(protocol), function, ctypes.c_char_p(domain), context, ctypes.byref(ref), ctypes.byref(error))
    #    if error.value != libwstp.WSEOK:
    #        raise WSTPEnvironmentError(env, err)
    #    return mlink, ref
    #def register_link_service_from_link_server(self, serviceProtocol, serviceName, server, function, domain, context, ref, error):
    #    # MLDECL(void, MLRegisterLinkServiceFromLinkServer, (MLEnvironment env, const char *serviceProtocol, const char *serviceName, MLLinkServer server, MLRegisterCallbackFunction function, const char *domain, void *context, MLServiceRef *ref, int *error));

    def __create_service(self, link, ref, context):
        assert(operator.eq(*map(ctypes.sizeof, [server, ctypes.c_long])))
        address, ctx = ctypes.cast(server, ctypes.c_long).value, None if context is None else ctypes.cast(context, ctypes.c_long).value

        sanity = self._finalizers.get(address, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[address] = weakref.finalize(service, InternalEnvironment.destroy_service, utils.clone(server))
        self._services[address] = service

        if all(item is not None for item in [context, ctx]):
            self._contextrefs[address] = context
            self._finalizers[ctx] = weakref.finalize(service, self._contextrefs.pop, address, None)

        index = len(self._services)
        logging.warning("created service #{:d} ({:#x}){:s} and returning it as {!s}.".format(index, address, " with context ({:#x})".format(ctypes.addressof(context.value)) if context else '', service))
        return WSLinkService(service)

    #def stop_registering_link_service_for_link(self, link, ref):
    #    '''MLStopRegisteringLinkServiceForLink'''
    #    libwstp.WSStopRegisteringLinkServiceForLink(self._environment, link, ref)
    #    return True
    #def stop_registering_link_service(self, ref):
    #    '''MLStopRegisteringLinkService'''
    #    libwstp.WSStopRegisteringLinkService(self._environment, ref)
    #    return True

class WSLinkServiceReference(object):
    def __init__(self, environment, serviceRef, context):
        self._environment, self._ref, self._context = environment, serviceRef, context

    @property
    def protocol(self):
        '''MLServiceProtocolFromReference'''
        result = libwstp.WSServiceProtocolFromReference(self._environment, self._ref)
        # FIXME: context
        return result.contents

# FIXME: these need to be implemented in a way that a user can derive from it.
class WSLinkServiceBrowser(WSLinkServiceReference): pass
class WSLinkServiceResolver(WSLinkServiceReference): pass

class WSLinkServer(object):
    def __init__(self, linkserver, context):
        self._environment, self._linkserver, self._context = environment, linkserver, context

    @property
    def context(self):
        return self.context_from_link_server()

    @property
    def port(self):
        return self.port_from_link_server()

    def context_from_link_server(self):
        '''WSContextFromLinkServer'''
        env, server, error = (item() for item in itertools.chain([lambda: self._environment, lambda:self._linkserver], utils.arguments(libwstp.WSContextFromLinkServer, -1)))
        context = libwstp.WSContextFromLinkServer(server, ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        #FIXME: context
        return context if context is None else context.contents

    def register_callback_function_with_link_server(self, function):
        '''WSRegisterCallbackFunctionWithLinkServer'''
        libwstp.WSRegisterCallbackFunctionWithLinkServer(self._linkserver, function)
        return True

    def wait_for_new_link_from_link_server(self):
        '''WSWaitForNewLinkFromLinkServer'''
        env, server, error = (item() for item in itertools.chain([lambda:self._environment, lambda:self._linkserver], utils.arguments(libwstp.WSWaitForNewLinkFromLinkServer, -1)))
        mlink = libwstp.WSWaitForNewLinkFromLinkServer(server, ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)

        # FIXME: need to attach a link
        return mlink

    def port_from_link_server(self):
        '''WSPortFromLinkServer'''
        env, server, error = (item() for item in itertools.chain([lambda:self._environment, lambda:self._linkserver], utils.arguments(libwstp.WSPortFromLinkServer, -1)))
        port = libwstp.WSPortFromLinkServer(server, ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)
        return port

    def interface_from_link_server(self):
        '''WSInterfaceFromLinkServer'''
        env, server, error = (item() for item in itertools.chain([lambda:self._environment, lambda:self._linkserver], utils.arguments(libwstp.WSInterfaceFromLinkServer, -1)))
        iface = libwstp.WSInterfaceFromLinkServer(server, ctypes.byref(error))
        if error.value != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, error.value)

        with utils.pstring(iface, functools.partial(libwstp.WSReleaseInterfaceFromLinkServer, iface)) as string:
            result = string
        return result   # FIXME: const char*

    #def release_interface_from_link_server(self, iface):
    #    '''WSReleaseInterfaceFromLinkServer'''
    #    libwstp.WSReleaseInterfaceFromLinkServer(self._linkserver, ctypes.c_char_p(iface))
    #    return True

class WSLinkService(object):
    def __init__(self, resolverRef, context):
        self._ref, self._context = resolverRef, context

class WSLink(object):
    def __init__(self, mlink):
        self._mlink = mlink

    def get_linked_env_id_string(self):
        mlink, environment_id = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetLinkedEnvIDString, -1)))
        err = libwstp.WSGetLinkedEnvIDString(mlink, ctypes.byref(environment_id))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        with utils.pstring(environment_id, functools.partial(libwstp.WSReleaseEnvIDString, mlink)) as environment_id:
            result = environment_id
        return result

if __name__ == '__main__':
    import sys, ctypes, wstp, libwstp, importlib
    #importlib.reload(libwstp)
    #importlib.reload(wstp)

    # wstp32i4m.lib or wstp64i4m.lib
    # libWSTP32i4.a, libWSTP32i4.so, libWSTP64i4.a, and libWSTP64i4.so.
    # libWSTPi4.a.

    #environment = wstp.initialize()
    env = wstp.InternalEnvironment.create(sys.getdefaultencoding())
    methods = ['version', 'compiler_id', 'ucs2_compiler_id', 'utf8_compiler_id', 'utf16_compiler_id', 'utf32_compiler_id']

    for F, name in zip(map(operator.methodcaller, methods), methods):
        try: F(env)
        except Exception as E: print(name, E)
    sys.exit()

    x = ctypes.c_uint(0xdeaddead)
    y = ctypes.pointer(x)
    z = ctypes.pointer(y)
    p(z._objects)
    p(y._objects)
    p(x._objects)
    a = ctypes.cast(ctypes.pointer(z), ctypes.c_void_p)
    p(a._objects)
    p(a.value)
    b = ctypes.POINTER(ctypes.POINTER(ctypes.c_uint))
    c = ctypes.cast(ctypes.c_void_p(a.value), ctypes.POINTER(z.__class__))
    p(c._objects)
    p(hex(c.contents.contents.contents.value))
    b
    ctypes.pointer(z).__class__

    x = wstp.InternalEnvironment.create()
    x = weakref.ref(x)
    y = wstp.fucker
    print(weakref.getweakrefcount(x()))
    #print(sys.getrefcount(x))
    print(sys.getrefcount(x()))
    #print(gc.get_referrers(x()))
    #print([hex(x) for x in itertools.chain(*gc.get_referrers(x()))])
    #print(hex(id(gc.get_referrers(x()))))
    #print(hex(id(gc.get_referrers(x()))))

    #E = self = wstp.Environment()
    #self._environment = env
    #link = wstp.InternalEnvironment.create('intra')
    #print(link)
    #del(link)
    #print((wstp.InternalEnvironment._links))
    #x = wstp.deinitialize()
    #print('deinit',x)

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
    p(issubclass(x, ctypes.POINTER))
    assert(err == libwstp.WSEOK)
    ok = libwstp.WSReleaseLinkProtocolNames(env, protocolnames, length)
    x = libwstp.WSGetAvailableLinkProtocolNames.argtypes[1]
    f = libwstp.WSGetAvailableLinkProtocolNames
    p(isinstance(f, ctypes._CFuncPtr))
    p(issubclass(x, ctypes._Pointer))
    import _ctypes
    dir(_ctypes)
    x= x()
    p(x._b_base_)
    p(x._type_)
    dir(x)
    p(x.contents)
    p(libwstp.WSGetAvailableLinkProtocolNames.argtypes[1])

    p(protocolnames.contents)

    # MLDECL(int, MLGetAvailableLinkProtocolNames, (MLEnvironment ep, char ***protocolNames, int *length));
    # MLDECL(void,     MLReleaseLinkProtocolNames, (MLEnvironment ep, char **protocolNames, int length));

