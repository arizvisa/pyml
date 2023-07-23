import builtins, os, operator, math, functools, itertools, sys, threading
import gc, weakref, codecs, ctypes, logging, libwstp

### exception types
class WSTPException(Exception):
    pass

class WSTPSystemError(WSTPException, SystemError):
    pass

class WSTPErrorCode(WSTPException):
    def __init__(self, code):
        self.code, string = code, self.string(code)
        self.args = code, string

    @classmethod
    def get(cls, code, *args):
        return 'ERROR'

    @classmethod
    def string(cls, code):
        return "{:s}({:d})".format('ERROR', code)

class WSTPEnvironmentException(WSTPErrorCode):
    iterable = ((attribute, getattr(libwstp, attribute)) for attribute in dir(libwstp) if attribute.startswith('WSE'))
    _WSEnvironmentErrorMap = {integer : name for name, integer in iterable if isinstance(integer, int)}
    del(iterable)

    @classmethod
    def get(cls, code, *args):
        return cls._WSEnvironmentErrorMap.get(code, *args) if args else cls._WSEnvironmentErrorMap[code]

    @classmethod
    def string(cls, code):
        description = cls._WSEnvironmentErrorMap.get(code, cls._WSEnvironmentErrorMap[libwstp.WSEUNKNOWN])
        return "{:s}({:d})".format(description, code)

    def __init__(self, code, description, *args, **kwargs):
        self.code, string = code, self.string(code)

        kwargs.setdefault('code', code), kwargs.setdefault('error', string)
        self.args = code, string, description.format(*args, **kwargs)

class WSTPEnvironmentError(WSTPEnvironmentException):
    def __init__(self, environment, code):
        assert(isinstance(environment, libwstp.WSEnvironment))
        super(WSTPEnvironmentError, self).__init__(code, self.message(environment, code))

    @staticmethod
    def message(environment, err):
        string = libwstp.WSErrorString(environment, err)
        return string.decode('ascii')

class WSTPEnvironmentErrorMessage(WSTPEnvironmentError):
    def __init__(self, environment, code, description='', *args, **kwargs):
        assert(isinstance(environment, libwstp.WSEnvironment))
        message = self.message(environment, code)
        kwargs.setdefault('message', message)
        super(WSTPEnvironmentError, self).__init__(code, description.format(*args, **kwargs) if description else message)

class WSTPLinkException(WSTPSystemError):
    iterable = ((attribute, getattr(libwstp, attribute)) for attribute in dir(libwstp) if attribute.startswith('MLE'))
    _WSLinkErrorMap = {integer : name for name, integer in iterable if isinstance(integer, int)}
    del(iterable)

    @classmethod
    def get(cls, code, *args):
        return cls._WSLinkErrorMap.get(code, *args) if args else cls._WSLinkErrorMap[code]

    @classmethod
    def string(cls, code):
        description = cls._WSLinkErrorMap.get(code, cls._WSLinkErrorMap[libwstp.MLEUNKNOWN])
        return "{:s}({:d})".format(description, code)

class WSTPLinkErrorCode(WSTPLinkException):
    def __init__(self, code, description, *args, **kwargs):
        self.code, string = code, self.string(code)

        kwargs.setdefault('code', code), kwargs.setdefault('error', string)
        self.args = code, string, description.format(*args, **kwargs)

class WSTPLinkError(WSTPLinkErrorCode):
    def __init__(self, link):
        assert(isinstance(link, libwstp.WSLINK))
        code, message = libwstp.WSError(link), self.message(link)
        super(WSTPLinkErrorCode, self).__init__(code, message)

    @staticmethod
    def message(link):
        string = libwstp.WSErrorMessage(link)
        return string.decode('ascii')

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
            assert(any(issubclass(arg, ctypes._Pointer) for arg in listable))
            return tuple((arg._type_ if issubclass(arg, ctypes._Pointer) else arg) for arg in listable)
        return cls.arguments(func, slice(None, number) if number >= 0 else slice(number, None))

    @classmethod
    def pointer_pointers_thing(cls, object, length, transform, release):
        target = ctypes.POINTER(ctypes.c_void_p * getattr(length, 'value', length))
        return cls(object, target, transform, *release)

    @classmethod
    def pstrings(cls, object, length, *callable):
        def strings(object):
            return [ctypes.string_at(item) for item in object]
        return cls.pointer_pointers_thing(object, length, strings, callable)

    @classmethod
    def pwstrings(cls, object, length, *callable):
        def wstrings(object):
            raise NotImplementedError   # FIXME
            return [ctypes.string_at(item) for item in object]
        return cls.pointer_pointers_thing(object, length, wstrings, callable)

    @classmethod
    def pointer_things(cls, object, target, length, release):
        target = ctypes.POINTER(target * getattr(length, 'value', length))
        def items(object):
            return [item for item in object]
        return cls(object, target, items, *release)

    @classmethod
    def psingles(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_float, length, callable)

    @classmethod
    def pdoubles(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_double, length, callable)

    @classmethod
    def plongdoubles(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_longdouble, length, callable)

    @classmethod
    def psint16s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_short, length, callable)

    @classmethod
    def puint16s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_ushort, length, callable)

    @classmethod
    def psint32s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_int, length, callable)

    @classmethod
    def puint32s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_uint, length, callable)

    assert(ctypes.sizeof(ctypes.c_long) == 8)
    @classmethod
    def psint64s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_long, length, callable)

    assert(ctypes.sizeof(ctypes.c_ulong) == 8)
    @classmethod
    def puint64s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_ulong, length, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def psint128s(cls, object, length, *callable):
        return cls.pointer_things(object, ctypes.c_ulongdouble, length, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def puint128s(cls, object, length, *callable):
        # FIXME: need to manually create a structure here because...ctypes.
        return cls.pointer_things(object, ctypes.c_ulongdouble, length, callable)

    @classmethod
    def pbytes(cls, object, length, *callable):
        target = ctypes.POINTER(ctypes.c_ubyte * getattr(length, 'value', length))
        def transform_bytes(object):
            return bytearray(byte for byte in object)
        return cls(object, target, transform_bytes, *callable)

    @classmethod
    def pointer_thing(cls, object, target, release):
        return cls(object, target, None, *release)

    @classmethod
    def pstring(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_void_p)
        def string(object):
            return ctypes.string_at(ctypes.pointer(object))
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
        return cls.pointer_thing(object, target, callable)
    @classmethod
    def psint16(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_short)
        return cls.pointer_thing(object, target, callable)

    @classmethod
    def puint32(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_uint)
        return cls.pointer_thing(object, target, callable)
    @classmethod
    def psint32(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_int)
        return cls.pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_ulong) == 8)
    @classmethod
    def puint64(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_ulong)
        return cls.pointer_thing(object, target, callable)
    assert(ctypes.sizeof(ctypes.c_long) == 8)
    @classmethod
    def psint64(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_long)
        return cls.pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def puint128(cls, object, *callable):
        # FIXME: need to manually create a structure here because...ctypes.
        target = ctypes.POINTER(ctypes.c_ulong)
        return cls.pointer_thing(object, target, callable)

    assert(ctypes.sizeof(ctypes.c_longdouble) == 16)
    @classmethod
    def psint128(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_long)
        return cls.pointer_thing(object, target, callable)

    @classmethod
    def psingle(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_float)
        return cls.pointer_thing(object, target, callable)

    @classmethod
    def pdouble(cls, object, *callable):
        target = ctypes.POINTER(ctypes.c_double)
        return cls.pointer_thing(object, target, callable)

    class string(object):
        _ascii = codecs.lookup('ascii')
        _bytes = codecs.lookup('latin1')
        _utf8 = codecs.lookup('utf-8')
        _utf16 = codecs.lookup('utf-16')
        _utf32 = codecs.lookup('utf-32')
        @classmethod
        def _cast(cls, source, target):
            return ctypes.cast(ctypes.pointer(source), ctypes.POINTER(target)).contents
        @classmethod
        def put_utf8(cls, string):
            bytes, length = cls._utf8.encode(string)
            return length, (ctypes.c_ubyte * len(bytes))(*bytearray(bytes))
        @classmethod
        def put_utf16(cls, string):
            bytes, length = cls._utf16.encode(string)
            extra = len(bytes) - 2 * len(string)    # BOM
            return len(bytes), cls._cast((ctypes.c_ubyte * len(bytes))(*bytearray(bytes)), ctypes.c_ushort * (length + extra // 2))
        put_ucs2 = put_utf16
        @classmethod
        def put_utf32(cls, string):
            bytes, length = cls._utf32.encode(string)
            extra = len(bytes) - 4 * len(string)    # BOM
            return len(bytes), cls._cast((ctypes.c_ubyte * len(bytes))(*bytearray(bytes)), ctypes.c_uint * (length + extra // 4))
        @classmethod
        def put_bytes(cls, string):
            return len(string), libwstp.String(string)
        @classmethod
        def put_number(cls, number):
            # FIXME: it'd probably be better if we do everything ourselves to
            #        ensure python doesn't scientific-notation or infinity it.
            string = "{:f}".format(number) if isinstance(number, float) else "{:d}".format(number) if isinstance(number, int) else "{!s}".format(number)
            encoded, length = cls._ascii.encode(string)
            return cls.put_bytes(encoded)
        @classmethod
        def put_string(cls, string):
            encoded, length = (string, len(string)) if isinstance(string, (bytes, bytearray)) else cls._bytes.encode(string)
            return cls.put_bytes(encoded)
        @classmethod
        def get_utf8(cls, array):
            res, length = cls._utf8.decode(bytearray(array))
            return length, res
        @classmethod
        def get_utf16(cls, array):
            bytes = cls._cast(array, ctypes.c_ubyte * ctypes.sizeof(array))
            res, length = cls._utf16.decode(bytearray(array))
            return length, res
        get_ucs2 = get_utf16
        @classmethod
        def get_utf32(cls, array):
            bytes = cls._cast(array, ctypes.c_ubyte * ctypes.sizeof(array))
            res, length = cls._utf32.decode(bytearray(array))
            return length, res
        @classmethod
        def get_number(cls, array):
            res, length = cls._ascii.decode(bytearray(array))
            # XXX: is it right to straight-up convert this to a float/int?
            return length, res
        @classmethod
        def get_string(cls, array):
            res, length = cls._bytes.decode(bytearray(array))
            return length, res

    class collection(object):
        @classmethod
        def arguments(cls, func, skip, *args):
            assert(isinstance(func, ctypes._CFuncPtr))
            argtypes = func.argtypes
            if isinstance(skip, slice):
                listable = [argtypes[index] for index in range(*skip.indices(len(argtypes)))]
                assert(all(issubclass(arg, ctypes._Pointer) for arg in listable))
                return tuple(arg._type_ for arg in listable)
            return cls.arguments(func, slice(skip, None) if skip >= 0 else slice(None, skip), *args)

        @classmethod
        def make(cls, item, target):
            if item is None:
                return ctypes.cast(ctypes.c_void_p(0), target)
            elif issubclass(target, ctypes._Pointer):
                return ctypes.cast(ctypes.pointer(item), ctypes.POINTER(target)).contents
            global fuck
            fuck = item, target
            return target(item)

        @classmethod
        def put(cls, func, skip, *args):
            assert(isinstance(func, ctypes._CFuncPtr))
            argtypes = func.argtypes
            if isinstance(skip, slice):
                result, listable = cls(*args), [argtypes[index] for index in range(*skip.indices(len(argtypes)))]
                return [cls.make(item, target) for item, target in zip(result, listable)]
            return cls.put(func, slice(skip, None) if skip >= 0 else slice(None, skip), *args)

        @classmethod
        def get(cls, *callable):
            raise NotImplementedError

        def __new__(cls, type, data):
            res = (type * len(data))(*data)
            return ctypes.pointer(res), len(data)

    class array(collection):
        @classmethod
        def __flatten__(cls, type, data):
            if not isinstance(data, type):
                yield data
                return

            for item in data:
                for nitem in cls.__flatten__(type, item):
                    yield nitem
                continue
            return

        @classmethod
        def __reshape__(cls, list, dimensions):
            if len(dimensions) == 1:
                return list
            xy = functools.reduce(operator.mul, dimensions[1:])
            return [cls.__reshape__(list[x * xy : (x + 1) * xy], dimensions[1:]) for x in range(len(list) // xy)]

        @classmethod
        def data(cls, type, depths, data):
            count = functools.reduce(operator.mul, depths, 1)
            iterable = cls.__flatten__(data.__class__, data)
            res = (type * count)(*iterable)
            return ctypes.pointer(res)

        @classmethod
        def heads(cls, names):
            names = [(item if isinstance(item, (bytes,bytearray)) else item.encode('ascii')) for item in names]
            items = [ctypes.c_char_p(item) for item in names]
            target = ctypes.POINTER(ctypes.c_char) * len(items)
            res = (ctypes.c_char_p * len(items))(*items)
            return ctypes.cast(ctypes.pointer(res), ctypes.POINTER(ctypes.POINTER(ctypes.c_char) * len(items))).contents

        @classmethod
        def dims(cls, depths):
            res = (ctypes.c_int * len(depths))(*depths)
            return ctypes.pointer(res)

        def __new__(cls, type, data, dimensions, head=None):
            assert(not head or len(head) == len(dims))
            dims, heads = cls.dims(dimensions), cls.heads(head) if head else None
            return cls.data(type, dimensions, data), dims, heads, len(dimensions)

        @classmethod
        def get(cls, data, dims, headsp, depth, *callable):
            depth = getattr(depth, 'value', depth)
            iterable = iter(callable)

            [release, iterable] = next(iterable, None), iterable
            release_args = [arg for arg in iterable]

            with utils.pstrings(headsp, depth) as items:
                heads = [item for item in items]

            assert(len(data) == functools.reduce(operator.mul, dims, 1))
            reshaped = cls.__reshape__(data, dims)

            if release:
                release(headsp, depth, *release_args)
            return reshaped, dims, heads

# FIXME: there's absolutely no reason for this to be a singleton, as we really
#        only need some place to store references to initialized environments
class InternalEnvironment(object):
    _lock, _state, _finalizers = threading.Lock(), weakref.WeakValueDictionary(), {}

    def __new__(cls):
        pass

    _encodings = {
        codecs.lookup('ascii'): libwstp.MLASCII_ENC,
        codecs.lookup('utf8'): libwstp.MLUTF8_ENC,
        codecs.lookup('utf-16'): libwstp.MLUTF16_ENC, codecs.lookup('utf-16-le'): libwstp.MLUTF16_ENC, codecs.lookup('utf-16-be'): libwstp.MLUTF16_ENC,
        codecs.lookup('utf-32'): libwstp.MLUTF32_ENC, codecs.lookup('utf-32-le'): libwstp.MLUTF32_ENC, codecs.lookup('utf-32-be'): libwstp.MLUTF32_ENC,

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
        return WSEnvironment(environment)

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
        logging.warning("destroying environment containing {:d} link{:s} ({:s}).".format(len(links), '' if len(links) == 1 else 's', ', '.join(map("{:#x}".format, links))))

        # these should actually be gone by now, but we need to
        # verify that this dict is empty in a thread-safe way.
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
        logging.warning("destroying environment containing {:d} server{:s} ({:s}).".format(len(servers), '' if len(servers) == 1 else 's', ', '.join(map("{:#x}".format, servers))))

        # these servers should actually be gone by now, but we need
        # to verify that this dict is empty in a thread-safe way.
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
        logging.warning("destroying environment containing {:d} browser{:s} ({:s}).".format(len(browsers), '' if len(browsers) == 1 else 's', ', '.join(map("{:#x}".format, browsers))))

        # none of the browsers should actually exist by now, but we need
        # to verify that our tracking dict is empty in a thread-safe way.
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
        logging.warning("destroying environment containing {:d} resolver{:s} ({:s}).".format(len(resolvers), '' if len(resolvers) == 1 else 's', ', '.join(map("{:#x}".format, resolvers))))

        # all of the resolvers should be destroyed by now. however, we need
        # to verify that our tracking dict is empty in a threadsafe way.
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
        logging.warning("destroying environment containing {:d} service{:s} ({:s}).".format(len(services), '' if len(services) == 1 else 's', ', '.join(map("{:#x}".format, services))))

        # none of the services should exist by now, but we need to
        # verify that this dict is empty in a thread-safe way.
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

class WSEnvironment(object):
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
        return inumb.value, rnumb.value, bnumb.value

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
        available, candidates  = self.available_link_protocol_names, {ch for ch in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'}
        original = {proto : proto for proto in available}
        mixed, lower = {}, {proto.lower() : proto for proto in available if proto.upper() == proto}
        for proto in available:
            if proto.upper() == proto or proto.lower() == proto:
                continue
            iterable = (1 + index for index, character in enumerate(proto[1:]) if character in candidates)
            index = next(iterable, 0)
            if index:
                mixed[proto[:index].casefold()] = proto
            else:
                mixed[proto.casefold()] = proto
            continue

        result = {}
        [result.update(items) for items in [original, lower, mixed]]
        return result

    ## WSLink
    def Connect(self, name, *args, **parameters):
        return self.__create_link('connect', name, *args, **parameters)
    def Create(self, name, *args, **parameters):
        return self.__create_link('create', name, *args, **parameters)
    def Launch(self, name, *args, **parameters):
        return self.__create_link('launch', name, *args, **parameters)
    def Close(self, wslink):
        env, identifier = self._environment, wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        link = self._links.pop(identifier, libwstp.WSFromLinkID(env, identifier))
        assert(isinstance(link, libwstp.WSLINK))
        return InternalEnvironment.destroy_link(env, identifier)

    def get_links(self):
        '''MLGetLinksFromEnvironment/MLReleaseLinksFromEnvironment'''
        # FIXME: probably not useful since we're tracking these
        env, links, length = (item() for item in itertools.chain([lambda:self._environment], utils.arguments(libwstp.WSGetLinksFromEnvironment, -2)))
        err = libwstp.WSGetLinksFromEnvironment(env, ctypes.byref(links), ctypes.byref(length))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentError(env, err)

        # FIXME: each of these should be WSLink objects.
        with utils.ptargets(libwstp.MLINK, links, length, functools.partial(libwstp.WSReleaseLinksFromEnvironment, env)) as links:
            result = links
        return result

    def __create_link(self, type, name, *args, **parameters):
        if 'linkprotocol' in parameters:
            protocol = parameters.pop('linkprotocol')
            available = self._available_link_protocol_names()
            assert(available.get(protocol, available.get(protocol.casefold())) is not None)
            selected = available[protocol] if protocol in available else available[protocol.casefold()]
            parameters['linkprotocol'] = selected

        assert(type.casefold() in {item for item in map(operator.methodcaller('casefold'), ['connect', 'create', 'launch'])})
        parameters.setdefault('linkname', "{!s}".format(name)) if name else None

        iterable = itertools.chain(["-link{:s}".format(type.lower())], *[("-{:s}".format(key), value) for key, value in parameters.items()])
        argv = [arg for arg in itertools.chain(iterable, args)]

        env, error = self._environment, utils.argument(libwstp.WSOpenString, -1)(libwstp.WSEUNKNOWN)
        link = libwstp.WSOpenString(env, ' '.join(argv), ctypes.byref(error))
        if not(link) or error.value != libwstp.WSEOK:
            raise WSTPEnvironmentErrorMessage(env, error.value, "unable to {:s} link{:s} ({message})", type.lower(), " \"{:s}\"".format(name) if name else '')

        identifier = libwstp.WSToLinkID(link)
        sanity = self._finalizers.get(identifier, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[identifier] = weakref.finalize(link, InternalEnvironment.destroy_link, env, identifier)
        self._links[identifier] = link

        index, description = len(self._links), libwstp.WSLinkName(link)
        logging.warning("created link #{:d} ({:#x}) with name \"{:s}\" and returning it as {!s}.".format(index, identifier, description.decode('ascii'), link))
        return WSLink(link)

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

    def duplicate_link(self, wslink, name):
        identifier = wslink if isinstance(wslink, int) else libwstp.WSToLinkID(wslink)
        assert(identifier in self._links)
        parent, error = (item() for item in itertools.chain([lambda:self.links[identifier]], utils.arguments(libwstp.WSNewLinkServer, -1)))
        newlink = libwstp.WSDuplicateLink(parent, name, err)
        if not(newlink) or error.value != libwstp.WSEOK:
            raise WSTPEnvironmentException(error.value, 'unable to duplicate link id {:#x}', identifier)

        env, link, identifier = self._environment, newlink, libwstp.WSToLinkID(newlink)
        sanity = self._finalizers.get(identifier, None)
        assert((sanity is None) or not(sanity.alive))
        self._finalizers[identifier] = weakref.finalize(link, InternalEnvironment.destroy_link, env, identifier)
        self._links[identifier] = link

        index, origin, description = len(self._links), libwstp.WSLinkName(parent), libwstp.WSLinkName(newlink)
        logging.warning("duplicated link #{:d} ({:#x}) from \"{:s}\" with name \"{:s}\" and duplicated it as {!s}.".format(index, identifier, origin.decode('ascii'), description.decode('ascii'), link))

        return WSLink(newlink)

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

    def ready_parallel(self, links, waittime=None):
        assert(all(isinstance(link, (WSLink, libwstp.WSLINK)) for link in links))
        if waittime is not None:
            fraction, integer = math.modf(waittime)
            seconds, useconds = integer, fraction * 1e6
        else:
            seconds, useconds = -1, -1
        tv = libwstp.wstimeval(seconds, useconds)

        items = [(link._mlink if isinstance(link, WSLink) else link) for link in links]
        mlinks = (libwstp.WSLINK * len(items))(*items)
        index = libwstp.WSReadyParallel(self._environment, ctypes.byref(mlinks), len(items), tv)

        if index == libwstp.MLREADYPARALLELERROR:
            return False
        elif index == libwstp.MLREADYPARALLELTIMEDOUT:
            return False
        return links[index]

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

    def activate(self):
        mlink = self._mlink
        if not libwstp.WSActivate(mlink):
            raise WSTPLinkError(mlink)
        return

    def get_linked_env_id_string(self):
        mlink, environment_id = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetLinkedEnvIDString, -1)))
        err = libwstp.WSGetLinkedEnvIDString(mlink, ctypes.byref(environment_id))
        if err != libwstp.WSEOK:
            raise WSTPEnvironmentException(err, 'unable to get linked environment id')

        with utils.pstring(environment_id, functools.partial(libwstp.WSReleaseEnvIDString, mlink)) as environment_id:
            result = environment_id
        return result

    @property
    def name(self):
        mlink = self._mlink
        return libwstp.WSName(mlink)

    @property
    def link_name(self):
        '''WSLinkName/WSReleaseLinkName'''
        mlink = self._mlink
        return libwstp.WSLinkName(mlink)

    def ucs2_link_name(self):
        mlink, target, length = (item() for item in itertools.chain([lambda:self._mlink, lambda:ctypes.c_ushort], utils.arguments(libwstp.WSUCS2LinkName, -1)))
        res = libwstp.WSUCS2LinkName(mlink, ctypes.byref(length))
        if not res:
            raise WSTPLinkError(mlink)
        with utils.puint16s(res, length, functools.partial(libwstp.WSReleaseUCS2LinkName, mlink), length.value) as chars:
            pitems, size = ctypes.pointer((target * len(chars))(*chars)), ctypes.sizeof(target)
            pbytes = ctypes.cast(pitems, ctypes.POINTER(ctypes.c_ubyte * size * len(chars)))
            count, string = utils.string.get_ucs2(pbytes.contents)
            assert(count == len(chars) * size)
        return string
    def utf8_link_name(self):
        mlink, target, length = (item() for item in itertools.chain([lambda:self._mlink, lambda:ctypes.c_ubyte], utils.arguments(libwstp.WSUTF8LinkName, -1)))
        res = libwstp.WSUTF8LinkName(mlink, ctypes.byref(length))
        if not res:
            raise WSTPLinkError(mlink)
        with utils.pbytes(res, length, functools.partial(libwstp.WSReleaseUTF8LinkName, mlink), length.value) as bytes:
            count, string = utils.string.get_utf8(bytes)
            assert(len(bytes) == count)
        return string
    def utf16_link_name(self):
        mlink, target, length = (item() for item in itertools.chain([lambda:self._mlink, lambda:ctypes.c_ushort], utils.arguments(libwstp.WSUTF16LinkName, -1)))
        res = libwstp.WSUTF16LinkName(mlink, ctypes.byref(length))
        if not res:
            raise WSTPLinkError(mlink)
        with utils.puint16s(res, length, functools.partial(libwstp.WSReleaseUTF16LinkName, mlink), length.value) as chars:
            pitems, size = ctypes.pointer((target * len(chars))(*chars)), ctypes.sizeof(target)
            pbytes = ctypes.cast(pitems, ctypes.POINTER(ctypes.c_ubyte * size * len(chars)))
            count, string = utils.string.get_utf16(pbytes.contents)
            assert(count == len(chars) * size)
        return string
    def utf32_link_name(self):
        mlink, target, length = (item() for item in itertools.chain([lambda:self._mlink, lambda:ctypes.c_uint], utils.arguments(libwstp.WSUTF32LinkName, -1)))
        res = libwstp.WSUTF32LinkName(mlink, ctypes.byref(length))
        if not res:
            raise WSTPLinkError(mlink)
        with utils.puint32s(res, length, functools.partial(libwstp.WSReleaseUTF32LinkName, mlink), length.value) as chars:
            pitems, size = ctypes.pointer((target * len(chars))(*chars)), ctypes.sizeof(target)
            pbytes = ctypes.cast(pitems, ctypes.POINTER(ctypes.c_ubyte * size * len(chars)))
            count, string = utils.string.get_utf32(pbytes.contents)
            assert(count == len(chars) * size)
        return string

    @property
    def number(self):
        '''WSNumber'''
        mlink = self._mlink
        return libwstp.WSNumber(mlink)

    @property
    def is_link_loopback(self):
        '''MLIsLinkLoopback'''
        mlink = self._mlink
        return libwstp.WSIsLinkLoopback(mlink)

    def error(self):
        '''MLError'''
        mlink = self._mlink
        return libwstp.WSError(mlink)

    def clear_error(self):
        '''MLClearError'''
        mlink = self._mlink
        return libwstp.WSClearError(mlink)

    def set_error(self, err):
        '''MLSetError'''
        mlink = self._mlink
        return libwstp.WSSetError(mlink, err)

    def put_message(self, msg):
        '''MLPutMessage'''
        mlink = self._mlink
        if not libwstp.WSPutMessage(mlink, msg):
            raise WSTPLinkError(mlink)
        return
    def get_message(self):
        '''MLGetMessage'''
        mlink, mp, np = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetMessage, -2)))
        res = libwstp.WSGetMessage(mlink, ctypes.byref(mp), ctypes.byref(np))
        return res, mp.value, np.value
    def message_ready(self):
        '''MLMessageReady'''
        mlink = self._mlink
        res = libwstp.WSMessageReady(mlink)
        return True if res else False
    def put_message_with_arg(self, msg, arg):
        '''MLPutMessageWithArg'''
        mlink = self._mlink
        if not libwstp.WSPutMessageWithArg(mlink, msg, arg):
            raise WSTPLinkError(mlink)
        return

    def low_level_device_name(self):
        '''WSLowLevelDeviceName/WSReleaseLowLevelDeviceName'''
        mlink, name = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSLowLevelDeviceName, -1)))
        if not libwstp.WSLowLevelDeviceName(mlink, ctypes.byref(name)):
            raise WSTPLinkError(mlink)

        with utils.pstring(name, functools.partial(libwstp.WSReleaseLowLevelDeviceName, mlink)) as string:
            result = string
        return result

    def new_packet(self):
        mlink = self._mlink
        if not libwstp.WSNewPacket(mlink):
            raise WSTPLinkError(mlink)
        return
    def next_packet(self):
        mlink = self._mlink
        if not libwstp.WSNextPacket(mlink):
            raise WSTPLinkError(mlink)
        return
    @property
    def ready(self):
        mlink = self._mlink
        res = libwstp.WSReady(mlink)
        return True if res else False
    def wait_for_link_activity(self):
        mlink = self._mlink
        res = libwstp.WSWaitForLinkActivity(mlink)
        if res == libwstp.MLWAITERROR:
            return Exception
        elif res == libwstp.MLWAITSUCCESS:
            return True
        return False
    def wait_for_link_activity_with_callback(self, callback):
        assert(isinstance(callback, WSLinkWaitCallBackObject))
        mlink = self._mlink
        res = libwstp.WSWaitForLinkActivityWithCallback(mlink, callback)
        if res == libwstp.MLWAITERROR:
            return Exception
        elif res == libwstp.MLWAITSUCCESS:
            return True
        return False

    def get_next(self):
        mlink = self._mlink
        return libwstp.WSGetNext(mlink)
    def get_next_raw(self):
        mlink = self._mlink
        return libwstp.WSGetNextRaw(mlink)
    def get_type(self):
        mlink = self._mlink
        return libwstp.WSGetType(mlink)
    def get_raw_type(self):
        return libwstp.WSGetRawType(mlink)
    def take_last(self, eleft):
        mlink = self._mlink
        return libwstp.WSTakeLast(mlink, eleft)
    def fill(self):
        mlink = self._mlink
        if not libwstp.WSFill(mlink):
            raise WSTPLinkError(mlink)
        return
    def get_arg_count(self):
        mlink, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetArgCount, -1)))
        if not libwstp.WSGetArgCount(mlink, ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        return countp
    def get_raw_arg_count(self):
        mlink, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetRawArgCount, -1)))
        if not libwstp.WSGetRawArgCount(mlink, ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        return countp
    def bytes_to_get(self):
        mlink, leftp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSBytesToGet, -1)))
        if not libwstp.WSBytesToGet(mlink, ctypes.byref(leftp)):
            raise WSTPLinkError(mlink)
        return leftp
    def raw_bytes_to_get(self):
        mlink, leftp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSRawBytesToGet, -1)))
        if not libwstp.WSRawBytesToGet(mlink, ctypes.byref(leftp)):
            raise WSTPLinkError(mlink)
        return leftp
    def expressions_to_get(self):
        mlink, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSExpressionsToGet, -1)))
        if not libwstp.WSExpressionsToGet(mlink, ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        return countp
    def get_raw_data(self):
        mlink, gotp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetRawData, -1)))
        data = (ctypes.c_ubyte * self.bytes_to_get(mlink))()
        if not libwstp.WSGetRawData(mlink, ctypes.byref(data), ctypes.sizeof(data), ctypes.byref(gotp)):
            raise WSTPLinkError(mlink)
        return gotp, data
    def get_data(self):
        mlink, gotp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetData, -1)))
        data = (ctypes.c_ubyte * self.bytes_to_get(mlink))()        # libwstp.String?
        if not libwstp.WSGetData(mlink, ctypes.byref(data), ctypes.sizeof(data), ctypes.byref(gotp)):
            raise WSTPLinkError(mlink)
        return gotp, data

    def end_packet(self):
        mlink = self._mlink
        if not libwstp.WSEndPacket(mlink):
            raise WSTPLinkError(mlink)
        return
    def flush(self):
        mlink = self._mlink
        if not libwstp.WSFlush(mlink):
            raise WSTPLinkError(mlink)
        return
    def put_next(self, tok):
        mlink = self._mlink
        if not libwstp.WSPutNext(mlink, tok):
            raise WSTPLinkError(mlink)
        return
    def put_type(self, tok):
        mlink = self._mlink
        if not libwstp.WSPutType(mlink, tok):
            raise WSTPLinkError(mlink)
        return
    def put_raw_size(self, size):
        mlink = self._mlink
        if not libwstp.WSPutRawSize(mlink, size):
            raise WSTPLinkError(mlink)
        return
    def put_arg_count(self, argc):
        mlink = self._mlink
        if not libwstp.WSPutArgCount(mlink, argc):
            raise WSTPLinkError(mlink)
        return
    def put_composite(self, argc):
        mlink = self._mlink
        if not libwstp.WSPutComposite(mlink, argc):
            raise WSTPLinkError(mlink)
        return
    def bytes_to_put(self):
        mlink, leftp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSBytesToGet, -1)))
        if not libwstp.WSBytesToPut(mlink, ctypes.byref(leftp)):
            raise WSTPLinkError(mlink)
        return leftp
    def put_raw_data(self, data):
        buffer = ctypes.c_buffer(data)
        if not libwstp.WSPutRawData(mlink, ctypes.byref(buffer), len(data)):
            raise WSTPLinkError(mlink)
        return ctypes.sizeof(buffer)

    def get_binary_number(self, type):
        raise NotImplementedError('MLDECL( int,   MLGetBinaryNumber,  ( MLINK mlp, void *np, long type));')

    def get_integer_8(self):
        mlink, cp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetInteger8, -1)))
        if not libwstp.WSGetInteger8(mlink, ctypes.byref(cp)):
            raise WSTPLinkError(mlink)
        return cp
    def get_integer_16(self):
        mlink, hp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetInteger16, -1)))
        if not libwstp.WSGetInteger16(mlink, ctypes.byref(hp)):
            raise WSTPLinkError(mlink)
        return hp
    def get_integer_32(self):
        mlink, ip = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetInteger32, -1)))
        if not libwstp.WSGetInteger32(mlink, ctypes.byref(ip)):
            raise WSTPLinkError(mlink)
        return ip
    def get_integer_64(self):
        mlink, wp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetInteger64, -1)))
        if not libwstp.WSGetInteger64(mlink, ctypes.byref(wp)):
            raise WSTPLinkError(mlink)
        return wp
    def put_integer_8(self, i):
        mlink = self._mlink
        if not libwstp.WSPutInteger8(mlink, i):
            raise WSTPLinkError(mlink)
        return
    def put_integer_16(self, i):
        mlink = self._mlink
        if not libwstp.WSPutInteger16(mlink, i):
            raise WSTPLinkError(mlink)
        return
    def put_integer_32(self, h):
        mlink = self._mlink
        if not libwstp.WSPutInteger32(mlink, h):
            raise WSTPLinkError(mlink)
        return
    def put_integer_64(self, w):
        mlink = self._mlink
        if not libwstp.WSPutInteger64(mlink, w):
            raise WSTPLinkError(mlink)
        return

    def get_real_32(self):
        mlink, fp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetReal32, -1)))
        if not libwstp.WSGetReal32(mlink, ctypes.byref(fp)):
            raise WSTPLinkError(mlink)
        return fp
    def get_real_64(self):
        mlink, dp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetReal64, -1)))
        if not libwstp.WSGetReal64(mlink, ctypes.byref(dp)):
            raise WSTPLinkError(mlink)
        return dp
    def get_real_128(self):
        mlink, dp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetReal128, -1)))
        if not libwstp.WSGetReal128(mlink, ctypes.byref(dp)):
            raise WSTPLinkError(mlink)
        return dp
    def put_real_32(self, f):
        mlink = self._mlink
        if not libwstp.WSPutReal32(mlink, f):
            raise WSTPLinkError(mlink)
        return
    def put_real_64(self, d):
        mlink = self._mlink
        if not libwstp.WSPutReal64(mlink, d):
            raise WSTPLinkError(mlink)
        return
    def put_real_128(self, x):
        mlink = self._mlink
        if not libwstp.WSPutReal128(mlink, x):
            raise WSTPLinkError(mlink)
        return

    def get_byte_string(self, missing):
        mlink, sp, lenp, _ = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetByteString, -3)))
        if not libwstp.WSGetByteString(mlink, ctypes.byref(sp), ctypes.byref(lenp), missing):
            raise WSTPLinkError(mlink)

        with utils.pbytes(sp, lenp, functools.partial(libwstp.WSReleaseByteString, mlink)) as bytes:
            result = bytes
        return lenp, result
    def get_string(self):
        mlink, sp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetString, -1)))
        if not libwstp.WSGetString(mlink, ctypes.byref(sp)):
            raise WSTPLinkError(mlink)

        with utils.pstring(sp, functools.partial(libwstp.WSReleaseString, mlink)) as string:
            result = string
        return result
    def put_byte_string(self, s):
        mlink, string = self._mlink, (ctypes.c_ubyte * len(s))(s)
        if not libwstp.WSPutByteString(mlink, ctypes.byref(string), len(s)):
            raise WSTPLinkError(mlink)
        return
    def put_string(self, s):
        mlink = self._mlink
        if not libwstp.WSPutString(mlink, ctypes.c_char_p(s)):
            raise WSTPLinkError(mlink)
        return

    def get_ucs2_string(self):
        mlink, sp, lenp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetUCS2String, -2)))
        if not libwstp.WSGetUCS2String(mlink, ctypes.byref(sp), ctypes.byref(lenp)):
            raise WSTPLinkError(mlink)
        with utils.pbytes(sp, lenp, functools.partial(libwstp.WSReleaseUCS2String, mlink), lenp) as string:
            count, result = utils.string.get_ucs2((ctypes.c_ubyte * len(string))(*string))
            assert(count == lenp.value)
        return result
    def get_utf8_string(self):
        mlink, sp, bytes, chars = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetUTF8String, -3)))
        if not libwstp.WSGetUTF8String(mlink, ctypes.byref(sp), ctypes.byref(bytes), ctypes.byref(chars)):
            raise WSTPLinkError(mlink)
        with utils.pbytes(sp, bytes, functools.partial(libwstp.WSReleaseUTF8String, mlink), bytes) as string:
            count, result = utils.string.get_utf8(string)
            assert(count == bytes.value)
        return result
    def get_utf16_string(self):
        mlink, sp, bytes, chars = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetUTF16String, -3)))
        if not libwstp.WSGetUTF16String(mlink, ctypes.byref(sp), ctypes.byref(bytes), ctypes.byref(chars)):
            raise WSTPLinkError(mlink)
        with utils.pbytes(sp, bytes, functools.partial(libwstp.WSReleaseUTF16String, mlink), bytes) as string:
            count, result = utils.string.get_utf16((ctypes.c_ubyte * len(string))(*string))
            assert(count == bytes.value)
        return result
    def get_utf32_string(self):
        mlink, sp, bytes, chars = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetUTF32String, -3)))
        if not libwstp.WSGetUTF32String(mlink, ctypes.byref(sp), ctypes.byref(bytes), ctypes.byref(chars)):
            raise WSTPLinkError(mlink)
        with utils.pbytes(sp, chars, functools.partial(libwstp.WSReleaseUTF32String, mlink), bytes) as string:
            count, result = utils.string.get_utf32((ctypes.c_ubyte * len(string))(*string))
            assert((count == bytes.value) and (len(result) == chars.value))
        return result
    def put_ucs2_string(self, s):
        length, res = utils.string.put_ucs2(s)
        mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), libwstp.WSPutUCS2String.argtypes[1]).contents
        if not libwstp.WSPutUCS2String(mlink, ctypes.byref(s), length):
            raise WSTPLinkError(mlink)
        return
    def put_utf8_string(self, s):
        length, res = utils.string.put_utf8(s)
        mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), libwstp.WSPutUTF8String.argtypes[1]).contents
        if not libwstp.WSPutUTF8String(mlink, ctypes.byref(s), length):
            raise WSTPLinkError(mlink)
        return
    def put_utf16_string(self, s):
        length, res = utils.string.put_utf16(s)
        mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), libwstp.WSPutUTF16String.argtypes[1]).contents
        if not libwstp.WSPutUTF16String(mlink, ctypes.byref(s), length):
            raise WSTPLinkError(mlink)
        return
    def put_utf32_string(self, s):
        length, res = utils.string.put_utf32(s)
        mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), libwstp.WSPutUTF32String.argtypes[1]).contents
        if not libwstp.WSPutUTF32String(mlink, ctypes.byref(s), length):
            raise WSTPLinkError(mlink)
        return

    def get_number_as_string(self):
        mlink, sp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetNumberAsString, -1)))
        if not libwstp.WSGetNumberAsString(mlink, ctypes.byref(sp)):
            raise WSTPLinkError(mlink)
        with utils.pstring(sp, functools.partial(libwstp.WSReleaseString, mlink)) as string:
            length, result = utils.string.get_number(string)
        return result
    def put_real_number_as_string(self, s):
        length, res = utils.string.put_number(s)
        mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), ctypes.POINTER(libwstp.WSPutRealNumberAsString.argtypes[1])).contents
        if not libwstp.WSPutRealNumberAsString(mlink, s):
            raise WSTPLinkError(mlink)
        return
    #def get_number_as_byte_string(self, missing):
    #    mlink, sp, lenp, _ = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetNumberAsByteString, -3)))
    #    if not libwstp.WSGetNumberAsByteString(mlink, ctypes.byref(sp), ctypes.byref(lenp), missing):
    #        raise WSTPLinkError(mlink)
    #    with utils.pbytes(sp, lenp, functools.partial(libwstp.WSReleaseByteString, mlink)) as bytes:
    #        length, result = utils.string.get_bytes(bytes)
    #    assert(lenp.value == length), (length, lenp)
    #    return lenp, result
    #def put_real_number_as_byte_string(self, s):
    #    length, res = utils.string.put_bytes(s)
    #    mlink, s = self._mlink, ctypes.cast(ctypes.pointer(res), libwstp.WSPutRealNumberAsByteString.argtypes[1]).contents
    #    if not libwstp.WSPutRealNumberAsByteString(mlink, ctypes.byref(s)):
    #        raise WSTPLinkError(mlink)
    #    return

    def put_size(self, size):
        mlink = self._mlink
        if not libwstp.WSPutSize(mlink, size):
            raise WSTPLinkError(mlink)
        return
    def put_data(self, buff):
        mlink, (length, data) = self._mlink, utils.string.put_bytes(buff)
        if not libwstp.WSPutData(mlink, data, length):
            raise WSTPLinkError(mlink)
        return

    def test_string(self, name):
        mlink, string = self._mlink, ctypes.c_char_p(name)    # libwstp.String
        if not libwstp.WSTestString(mlink, string):
            raise WSTPLinkError(mlink)
        return
    def test_ucs2_string(self, name):
        self, string = self._mlink, (ctypes.c_ushort * len(name))(name)
        if not libwstp.WSTestUCS2String(mlink, string, len(name)):
            raise WSTPLinkError(mlink)
        return
    def test_utf8_string(self, name):
        mlink, string = self._mlink, (ctypes.c_ubyte * len(name))(name)
        if not libwstp.WSTestUTF8String(mlink, string, len(name)):
            raise WSTPLinkError(mlink)
        return
    def test_utf16_string(self, name):
        mlink, string = self._mlink, (ctypes.c_ushort * len(name))(name)
        if not libwstp.WSTestUTF16String(mlink, string, len(name)):
            raise WSTPLinkError(mlink)
        return
    def test_utf32_string(self, name):
        mlink, string = self._mlink, (ctypes.c_uint * len(name))(name)
        if not libwstp.WSTestUTF32String(mlink, string, len(name)):
            raise WSTPLinkError(mlink)
        return

    def get_symbol(self):
        mlink, sp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetSymbol, -1)))
        if not libwstp.WSGetSymbol(mlink, ctypes.byref(sp)):
            raise WSTPLinkError(mlink)
        with utils.pstring(sp, functools.partial(libwstp.WSReleaseSymbol, mlink)) as string:
            print(string)
            length, res = utils.string.get_string(string)
        return res
    def put_symbol(self, s):
        mlink, (length, res) = self._mlink, utils.string.put_string(s)
        if not libwstp.WSPutSymbol(mlink, res):
            raise WSTPLinkError(mlink)
        return
    def test_symbol(self, s):
        mlink, sp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WTestSymbol, -1)))
        if not libwstp.WSTestSymbol(mlink, ctypes.c_char_p(s)):
            raise WSTPLinkError(mlink)
        return

    def get_function(self):
        mlink, sp, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSGetFunction, -2)))
        if not libwstp.WSGetFunction(mlink, ctypes.byref(sp), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.pstrings(sp, countp, functools.partial(libwstp.WSReleaseSymbol, env)) as items:
            decoded = [item.decode('ascii') for item in items]
        return decoded
    def put_function(self, s, argc):
        mlink = self._mlink
        if not libwstp.WSPutFunction(mlink, ctypes.c_char(s), argc):
            raise WSTPLinkError(mlink)
        return
    def test_head(self, s):
        mlink, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.arguments(libwstp.WSTestHead, -1)))
        if not libwstp.WSTestHead(mlink, ctypes.c_char_p(s), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        return countp

    def get_byte_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetByteArray, 1)))
        if not libwstp.WSGetByteArray(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item.value for item in items]
        with utils.pstrings(headsp, depth) as items:
            heads = [item.contents for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.pbytes(datap, count) as items:
            data = [item for item in items]
        libwstp.WSReleaseByteArray(mlink, datap, dimsp, headsp, depth)
        return data, dims, heads, depth
    def get_integer_8_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetInteger8Array, 1)))
        if not libwstp.WSGetInteger8Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.pbytes(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseInteger8Array, mlink, datap, dimsp))
    def get_integer_16_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetInteger16Array, 1)))
        if not libwstp.WSGetInteger16Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.psint16s(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseInteger16Array, mlink, datap, dimsp))
    def get_integer_32_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetInteger32Array, 1)))
        if not libwstp.WSGetInteger32Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.psint32s(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseInteger32Array, mlink, datap, dimsp))
    def get_integer_64_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetInteger64Array, 1)))
        if not libwstp.WSGetInteger64Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.psint64s(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseInteger64Array, mlink, datap, dimsp))

    def put_integer_8_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_ubyte
        parameters = utils.array.put(libwstp.WSPutInteger8Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutInteger8Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def put_integer_16_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_short
        parameters = utils.array.put(libwstp.WSPutInteger16Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutInteger16Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def put_integer_32_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_int
        parameters = utils.array.put(libwstp.WSPutInteger32Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutInteger32Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def put_integer_64_array(self, data, dims, heads=None):
        mlink, target = self._mlink, libwstp.wsint64
        parameters = utils.array.put(libwstp.WSPutInteger64Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutInteger64Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return

    def get_real_32_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetReal32Array, 1)))
        if not libwstp.WSGetReal32Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.psingles(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseReal32Array, mlink, datap, dimsp))
    def get_real_64_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetReal64Array, 1)))
        if not libwstp.WSGetReal64Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.pdoubles(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseReal64Array, mlink, datap, dimsp))
    def get_real_128_array(self):
        mlink, datap, dimsp, headsp, depthp = (item() for item in itertools.chain([lambda:self._mlink], utils.array.arguments(libwstp.WSGetReal128Array, 1)))
        if not libwstp.WSGetReal128Array(mlink, ctypes.byref(datap), ctypes.byref(dimsp), ctypes.byref(headsp), ctypes.byref(depthp)):
            raise WSTPLinkError(mlink)
        depth = depthp.value
        with utils.puint32s(dimsp, depth) as items:
            dims = [item for item in items]
        count = functools.reduce(operator.mul, dims, 1)
        with utils.plongdoubles(datap, count) as items:
            data = [item for item in items]
        return utils.array.get(data, dims, headsp, depth, functools.partial(libwstp.WSReleaseReal128Array, mlink, datap, dimsp))

    def put_real_32_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_float
        parameters = utils.array.put(libwstp.WSPutReal32Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutReal32Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def put_real_64_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_double
        parameters = utils.array.put(libwstp.WSPutReal64Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutReal64Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def put_real_128_array(self, data, dims, heads=None):
        mlink, target = self._mlink, ctypes.c_longdouble
        parameters = utils.array.put(libwstp.WSPutReal128Array, 1, target, data, dims, heads or [])
        if not libwstp.WSPutReal128Array(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return

    def get_integer_8_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetInteger8List, 1)))
        if not libwstp.WSGetInteger8List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.pbytes(datap, countp, functools.partial(libwstp.WSReleaseInteger8List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_integer_8_list(self, data):
        mlink, target = self._mlink, ctypes.c_ubyte
        parameters = utils.collection.put(libwstp.WSPutInteger8List, 1, target, data)
        if not libwstp.WSPutInteger8List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def get_integer_16_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetInteger16List, 1)))
        if not libwstp.WSGetInteger16List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.psint16s(datap, countp, functools.partial(libwstp.WSReleaseInteger16List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_integer_16_list(self, data):
        mlink, target = self._mlink, ctypes.c_short
        parameters = utils.collection.put(libwstp.WSPutInteger16List, 1, target, data)
        if not libwstp.WSPutInteger16List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def get_integer_32_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetInteger32List, 1)))
        if not libwstp.WSGetInteger32List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.psint32s(datap, countp, functools.partial(libwstp.WSReleaseInteger32List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_integer_32_list(self, data):
        mlink, target = self._mlink, ctypes.c_int
        parameters = utils.collection.put(libwstp.WSPutInteger32List, 1, target, data)
        if not libwstp.WSPutInteger32List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def get_integer_64_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetInteger64List, 1)))
        if not libwstp.WSGetInteger64List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.psint64s(datap, countp, functools.partial(libwstp.WSReleaseInteger64List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_integer_64_list(self, data):
        mlink, target = self._mlink, libwstp.wsint64
        parameters = utils.collection.put(libwstp.WSPutInteger64List, 1, target, data)
        if not libwstp.WSPutInteger64List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return

    def get_real_32_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetReal32List, 1)))
        if not libwstp.WSGetReal32List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.psingles(datap, countp, functools.partial(libwstp.WSReleaseReal32List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_real_32_list(self, data):
        mlink, target = self._mlink, ctypes.c_float
        parameters = utils.collection.put(libwstp.WSPutReal32List, 1, target, data)
        if not libwstp.WSPutReal32List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def get_real_64_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetReal64List, 1)))
        if not libwstp.WSGetReal64List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.pdoubles(datap, countp, functools.partial(libwstp.WSReleaseReal64List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_real_64_list(self, data):
        mlink, target = self._mlink, ctypes.c_double
        parameters = utils.collection.put(libwstp.WSPutReal64List, 1, target, data)
        if not libwstp.WSPutReal64List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return
    def get_real_128_list(self):
        mlink, datap, countp = (item() for item in itertools.chain([lambda:self._mlink], utils.collection.arguments(libwstp.WSGetReal128List, 1)))
        if not libwstp.WSGetReal128List(mlink, ctypes.byref(datap), ctypes.byref(countp)):
            raise WSTPLinkError(mlink)
        with utils.plongdoubles(datap, countp, functools.partial(libwstp.WSReleaseReal128List, mlink), countp.value) as items:
            data = [item for item in items]
        return data
    def put_real_128_list(self, data):
        mlink, target = self._mlink, ctypes.c_longdouble
        parameters = utils.collection.put(libwstp.WSPutReal128List, 1, target, data)
        if not libwstp.WSPutReal128List(mlink, *parameters):
            raise WSTPLinkError(mlink)
        return

if __name__ == '__main__':
    import sys, ctypes, wstp, libwstp, importlib
    #importlib.reload(libwstp)
    #importlib.reload(wstp)

    # wstp32i4m.lib or wstp64i4m.lib
    # libWSTP32i4.a, libWSTP32i4.so, libWSTP64i4.a, and libWSTP64i4.so.
    # libWSTPi4.a.

    #environment = wstp.initialize()
    env = wstp.InternalEnvironment.create(sys.getdefaultencoding())
    link = env.Connect('FUCK')
    link.activate()
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

    #E = self = wstp.WSEnvironment()
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

