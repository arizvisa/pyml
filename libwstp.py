r"""Wrapper for wstp.h

Generated with:
ctypesgen -l WSTP64i4 wstp.h -o libwstp.py

Do not modify this file.
"""

__docformat__ = "restructuredtext"

# Begin preamble for Python

import ctypes
import sys
from ctypes import *  # noqa: F401, F403

_int_types = (ctypes.c_int16, ctypes.c_int32)
if hasattr(ctypes, "c_int64"):
    # Some builds of ctypes apparently do not have ctypes.c_int64
    # defined; it's a pretty good bet that these builds do not
    # have 64-bit pointers.
    _int_types += (ctypes.c_int64,)
for t in _int_types:
    if ctypes.sizeof(t) == ctypes.sizeof(ctypes.c_size_t):
        c_ptrdiff_t = t
del t
del _int_types



class UserString:
    def __init__(self, seq):
        if isinstance(seq, bytes):
            self.data = seq
        elif isinstance(seq, UserString):
            self.data = seq.data[:]
        else:
            self.data = str(seq).encode()

    def __bytes__(self):
        return self.data

    def __str__(self):
        return self.data.decode()

    def __repr__(self):
        return repr(self.data)

    def __int__(self):
        return int(self.data.decode())

    def __long__(self):
        return int(self.data.decode())

    def __float__(self):
        return float(self.data.decode())

    def __complex__(self):
        return complex(self.data.decode())

    def __hash__(self):
        return hash(self.data)

    def __le__(self, string):
        if isinstance(string, UserString):
            return self.data <= string.data
        else:
            return self.data <= string

    def __lt__(self, string):
        if isinstance(string, UserString):
            return self.data < string.data
        else:
            return self.data < string

    def __ge__(self, string):
        if isinstance(string, UserString):
            return self.data >= string.data
        else:
            return self.data >= string

    def __gt__(self, string):
        if isinstance(string, UserString):
            return self.data > string.data
        else:
            return self.data > string

    def __eq__(self, string):
        if isinstance(string, UserString):
            return self.data == string.data
        else:
            return self.data == string

    def __ne__(self, string):
        if isinstance(string, UserString):
            return self.data != string.data
        else:
            return self.data != string

    def __contains__(self, char):
        return char in self.data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.__class__(self.data[index])

    def __getslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        return self.__class__(self.data[start:end])

    def __add__(self, other):
        if isinstance(other, UserString):
            return self.__class__(self.data + other.data)
        elif isinstance(other, bytes):
            return self.__class__(self.data + other)
        else:
            return self.__class__(self.data + str(other).encode())

    def __radd__(self, other):
        if isinstance(other, bytes):
            return self.__class__(other + self.data)
        else:
            return self.__class__(str(other).encode() + self.data)

    def __mul__(self, n):
        return self.__class__(self.data * n)

    __rmul__ = __mul__

    def __mod__(self, args):
        return self.__class__(self.data % args)

    # the following methods are defined in alphabetical order:
    def capitalize(self):
        return self.__class__(self.data.capitalize())

    def center(self, width, *args):
        return self.__class__(self.data.center(width, *args))

    def count(self, sub, start=0, end=sys.maxsize):
        return self.data.count(sub, start, end)

    def decode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.decode(encoding, errors))
            else:
                return self.__class__(self.data.decode(encoding))
        else:
            return self.__class__(self.data.decode())

    def encode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.encode(encoding, errors))
            else:
                return self.__class__(self.data.encode(encoding))
        else:
            return self.__class__(self.data.encode())

    def endswith(self, suffix, start=0, end=sys.maxsize):
        return self.data.endswith(suffix, start, end)

    def expandtabs(self, tabsize=8):
        return self.__class__(self.data.expandtabs(tabsize))

    def find(self, sub, start=0, end=sys.maxsize):
        return self.data.find(sub, start, end)

    def index(self, sub, start=0, end=sys.maxsize):
        return self.data.index(sub, start, end)

    def isalpha(self):
        return self.data.isalpha()

    def isalnum(self):
        return self.data.isalnum()

    def isdecimal(self):
        return self.data.isdecimal()

    def isdigit(self):
        return self.data.isdigit()

    def islower(self):
        return self.data.islower()

    def isnumeric(self):
        return self.data.isnumeric()

    def isspace(self):
        return self.data.isspace()

    def istitle(self):
        return self.data.istitle()

    def isupper(self):
        return self.data.isupper()

    def join(self, seq):
        return self.data.join(seq)

    def ljust(self, width, *args):
        return self.__class__(self.data.ljust(width, *args))

    def lower(self):
        return self.__class__(self.data.lower())

    def lstrip(self, chars=None):
        return self.__class__(self.data.lstrip(chars))

    def partition(self, sep):
        return self.data.partition(sep)

    def replace(self, old, new, maxsplit=-1):
        return self.__class__(self.data.replace(old, new, maxsplit))

    def rfind(self, sub, start=0, end=sys.maxsize):
        return self.data.rfind(sub, start, end)

    def rindex(self, sub, start=0, end=sys.maxsize):
        return self.data.rindex(sub, start, end)

    def rjust(self, width, *args):
        return self.__class__(self.data.rjust(width, *args))

    def rpartition(self, sep):
        return self.data.rpartition(sep)

    def rstrip(self, chars=None):
        return self.__class__(self.data.rstrip(chars))

    def split(self, sep=None, maxsplit=-1):
        return self.data.split(sep, maxsplit)

    def rsplit(self, sep=None, maxsplit=-1):
        return self.data.rsplit(sep, maxsplit)

    def splitlines(self, keepends=0):
        return self.data.splitlines(keepends)

    def startswith(self, prefix, start=0, end=sys.maxsize):
        return self.data.startswith(prefix, start, end)

    def strip(self, chars=None):
        return self.__class__(self.data.strip(chars))

    def swapcase(self):
        return self.__class__(self.data.swapcase())

    def title(self):
        return self.__class__(self.data.title())

    def translate(self, *args):
        return self.__class__(self.data.translate(*args))

    def upper(self):
        return self.__class__(self.data.upper())

    def zfill(self, width):
        return self.__class__(self.data.zfill(width))


class MutableString(UserString):
    """mutable string objects

    Python strings are immutable objects.  This has the advantage, that
    strings may be used as dictionary keys.  If this property isn't needed
    and you insist on changing string values in place instead, you may cheat
    and use MutableString.

    But the purpose of this class is an educational one: to prevent
    people from inventing their own mutable string class derived
    from UserString and than forget thereby to remove (override) the
    __hash__ method inherited from UserString.  This would lead to
    errors that would be very hard to track down.

    A faster and better solution is to rewrite your program using lists."""

    def __init__(self, string=""):
        self.data = string

    def __hash__(self):
        raise TypeError("unhashable type (it is mutable)")

    def __setitem__(self, index, sub):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + sub + self.data[index + 1 :]

    def __delitem__(self, index):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + self.data[index + 1 :]

    def __setslice__(self, start, end, sub):
        start = max(start, 0)
        end = max(end, 0)
        if isinstance(sub, UserString):
            self.data = self.data[:start] + sub.data + self.data[end:]
        elif isinstance(sub, bytes):
            self.data = self.data[:start] + sub + self.data[end:]
        else:
            self.data = self.data[:start] + str(sub).encode() + self.data[end:]

    def __delslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        self.data = self.data[:start] + self.data[end:]

    def immutable(self):
        return UserString(self.data)

    def __iadd__(self, other):
        if isinstance(other, UserString):
            self.data += other.data
        elif isinstance(other, bytes):
            self.data += other
        else:
            self.data += str(other).encode()
        return self

    def __imul__(self, n):
        self.data *= n
        return self


class String(MutableString, ctypes.Union):

    _fields_ = [("raw", ctypes.POINTER(ctypes.c_char)), ("data", ctypes.c_char_p)]

    def __init__(self, obj=b""):
        if isinstance(obj, (bytes, UserString)):
            self.data = bytes(obj)
        else:
            self.raw = obj

    def __len__(self):
        return self.data and len(self.data) or 0

    def from_param(cls, obj):
        # Convert None or 0
        if obj is None or obj == 0:
            return cls(ctypes.POINTER(ctypes.c_char)())

        # Convert from String
        elif isinstance(obj, String):
            return obj

        # Convert from bytes
        elif isinstance(obj, bytes):
            return cls(obj)

        # Convert from str
        elif isinstance(obj, str):
            return cls(obj.encode())

        # Convert from c_char_p
        elif isinstance(obj, ctypes.c_char_p):
            return obj

        # Convert from POINTER(ctypes.c_char)
        elif isinstance(obj, ctypes.POINTER(ctypes.c_char)):
            return obj

        # Convert from raw pointer
        elif isinstance(obj, int):
            return cls(ctypes.cast(obj, ctypes.POINTER(ctypes.c_char)))

        # Convert from ctypes.c_char array
        elif isinstance(obj, ctypes.c_char * len(obj)):
            return obj

        # Convert from object
        else:
            return String.from_param(obj._as_parameter_)

    from_param = classmethod(from_param)


def ReturnString(obj, func=None, arguments=None):
    return String.from_param(obj)


# As of ctypes 1.0, ctypes does not support custom error-checking
# functions on callbacks, nor does it support custom datatypes on
# callbacks, so we must ensure that all callbacks return
# primitive datatypes.
#
# Non-primitive return values wrapped with UNCHECKED won't be
# typechecked, and will be converted to ctypes.c_void_p.
def UNCHECKED(type):
    if hasattr(type, "_type_") and isinstance(type._type_, str) and type._type_ != "P":
        return type
    else:
        return ctypes.c_void_p


# ctypes doesn't have direct support for variadic functions, so we have to write
# our own wrapper class
class _variadic_function(object):
    def __init__(self, func, restype, argtypes, errcheck):
        self.func = func
        self.func.restype = restype
        self.argtypes = argtypes
        if errcheck:
            self.func.errcheck = errcheck

    def _as_parameter_(self):
        # So we can pass this variadic function as a function pointer
        return self.func

    def __call__(self, *args):
        fixed_args = []
        i = 0
        for argtype in self.argtypes:
            # Typecheck what we can
            fixed_args.append(argtype.from_param(args[i]))
            i += 1
        return self.func(*fixed_args + list(args[i:]))


def ord_if_char(value):
    """
    Simple helper used for casts to simple builtin types:  if the argument is a
    string type, it will be converted to it's ordinal value.

    This function will raise an exception if the argument is string with more
    than one characters.
    """
    return ord(value) if (isinstance(value, bytes) or isinstance(value, str)) else value

# End preamble

_libs = {}
_libdirs = []

# Begin loader

"""
Load libraries - appropriately for all our supported platforms
"""
# ----------------------------------------------------------------------------
# Copyright (c) 2008 David James
# Copyright (c) 2006-2008 Alex Holkner
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of pyglet nor the names of its
#    contributors may be used to endorse or promote products
#    derived from this software without specific prior written
#    permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------

import ctypes
import ctypes.util
import glob
import os.path
import platform
import re
import sys


def _environ_path(name):
    """Split an environment variable into a path-like list elements"""
    if name in os.environ:
        return os.environ[name].split(":")
    return []


class LibraryLoader:
    """
    A base class For loading of libraries ;-)
    Subclasses load libraries for specific platforms.
    """

    # library names formatted specifically for platforms
    name_formats = ["%s"]

    class Lookup:
        """Looking up calling conventions for a platform"""

        mode = ctypes.DEFAULT_MODE

        def __init__(self, path):
            super(LibraryLoader.Lookup, self).__init__()
            self.access = dict(cdecl=ctypes.CDLL(path, self.mode))

        def get(self, name, calling_convention="cdecl"):
            """Return the given name according to the selected calling convention"""
            if calling_convention not in self.access:
                raise LookupError(
                    "Unknown calling convention '{}' for function '{}'".format(
                        calling_convention, name
                    )
                )
            return getattr(self.access[calling_convention], name)

        def has(self, name, calling_convention="cdecl"):
            """Return True if this given calling convention finds the given 'name'"""
            if calling_convention not in self.access:
                return False
            return hasattr(self.access[calling_convention], name)

        def __getattr__(self, name):
            return getattr(self.access["cdecl"], name)

    def __init__(self):
        self.other_dirs = []

    def __call__(self, libname):
        """Given the name of a library, load it."""
        paths = self.getpaths(libname)

        for path in paths:
            # noinspection PyBroadException
            try:
                return self.Lookup(path)
            except Exception:  # pylint: disable=broad-except
                pass

        raise ImportError("Could not load %s." % libname)

    def getpaths(self, libname):
        """Return a list of paths where the library might be found."""
        if os.path.isabs(libname):
            yield libname
        else:
            # search through a prioritized series of locations for the library

            # we first search any specific directories identified by user
            for dir_i in self.other_dirs:
                for fmt in self.name_formats:
                    # dir_i should be absolute already
                    yield os.path.join(dir_i, fmt % libname)

            # check if this code is even stored in a physical file
            try:
                this_file = __file__
            except NameError:
                this_file = None

            # then we search the directory where the generated python interface is stored
            if this_file is not None:
                for fmt in self.name_formats:
                    yield os.path.abspath(os.path.join(os.path.dirname(__file__), fmt % libname))

            # now, use the ctypes tools to try to find the library
            for fmt in self.name_formats:
                path = ctypes.util.find_library(fmt % libname)
                if path:
                    yield path

            # then we search all paths identified as platform-specific lib paths
            for path in self.getplatformpaths(libname):
                yield path

            # Finally, we'll try the users current working directory
            for fmt in self.name_formats:
                yield os.path.abspath(os.path.join(os.path.curdir, fmt % libname))

    def getplatformpaths(self, _libname):  # pylint: disable=no-self-use
        """Return all the library paths available in this platform"""
        return []


# Darwin (Mac OS X)


class DarwinLibraryLoader(LibraryLoader):
    """Library loader for MacOS"""

    name_formats = [
        "lib%s.dylib",
        "lib%s.so",
        "lib%s.bundle",
        "%s.dylib",
        "%s.so",
        "%s.bundle",
        "%s",
    ]

    class Lookup(LibraryLoader.Lookup):
        """
        Looking up library files for this platform (Darwin aka MacOS)
        """

        # Darwin requires dlopen to be called with mode RTLD_GLOBAL instead
        # of the default RTLD_LOCAL.  Without this, you end up with
        # libraries not being loadable, resulting in "Symbol not found"
        # errors
        mode = ctypes.RTLD_GLOBAL

    def getplatformpaths(self, libname):
        if os.path.pathsep in libname:
            names = [libname]
        else:
            names = [fmt % libname for fmt in self.name_formats]

        for directory in self.getdirs(libname):
            for name in names:
                yield os.path.join(directory, name)

    @staticmethod
    def getdirs(libname):
        """Implements the dylib search as specified in Apple documentation:

        http://developer.apple.com/documentation/DeveloperTools/Conceptual/
            DynamicLibraries/Articles/DynamicLibraryUsageGuidelines.html

        Before commencing the standard search, the method first checks
        the bundle's ``Frameworks`` directory if the application is running
        within a bundle (OS X .app).
        """

        dyld_fallback_library_path = _environ_path("DYLD_FALLBACK_LIBRARY_PATH")
        if not dyld_fallback_library_path:
            dyld_fallback_library_path = [
                os.path.expanduser("~/lib"),
                "/usr/local/lib",
                "/usr/lib",
            ]

        dirs = []

        if "/" in libname:
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
        else:
            dirs.extend(_environ_path("LD_LIBRARY_PATH"))
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
            dirs.extend(_environ_path("LD_RUN_PATH"))

        if hasattr(sys, "frozen") and getattr(sys, "frozen") == "macosx_app":
            dirs.append(os.path.join(os.environ["RESOURCEPATH"], "..", "Frameworks"))

        dirs.extend(dyld_fallback_library_path)

        return dirs


# Posix


class PosixLibraryLoader(LibraryLoader):
    """Library loader for POSIX-like systems (including Linux)"""

    _ld_so_cache = None

    _include = re.compile(r"^\s*include\s+(?P<pattern>.*)")

    name_formats = ["lib%s.so", "%s.so", "%s"]

    class _Directories(dict):
        """Deal with directories"""

        def __init__(self):
            dict.__init__(self)
            self.order = 0

        def add(self, directory):
            """Add a directory to our current set of directories"""
            if len(directory) > 1:
                directory = directory.rstrip(os.path.sep)
            # only adds and updates order if exists and not already in set
            if not os.path.exists(directory):
                return
            order = self.setdefault(directory, self.order)
            if order == self.order:
                self.order += 1

        def extend(self, directories):
            """Add a list of directories to our set"""
            for a_dir in directories:
                self.add(a_dir)

        def ordered(self):
            """Sort the list of directories"""
            return (i[0] for i in sorted(self.items(), key=lambda d: d[1]))

    def _get_ld_so_conf_dirs(self, conf, dirs):
        """
        Recursive function to help parse all ld.so.conf files, including proper
        handling of the `include` directive.
        """

        try:
            with open(conf) as fileobj:
                for dirname in fileobj:
                    dirname = dirname.strip()
                    if not dirname:
                        continue

                    match = self._include.match(dirname)
                    if not match:
                        dirs.add(dirname)
                    else:
                        for dir2 in glob.glob(match.group("pattern")):
                            self._get_ld_so_conf_dirs(dir2, dirs)
        except IOError:
            pass

    def _create_ld_so_cache(self):
        # Recreate search path followed by ld.so.  This is going to be
        # slow to build, and incorrect (ld.so uses ld.so.cache, which may
        # not be up-to-date).  Used only as fallback for distros without
        # /sbin/ldconfig.
        #
        # We assume the DT_RPATH and DT_RUNPATH binary sections are omitted.

        directories = self._Directories()
        for name in (
            "LD_LIBRARY_PATH",
            "SHLIB_PATH",  # HP-UX
            "LIBPATH",  # OS/2, AIX
            "LIBRARY_PATH",  # BE/OS
        ):
            if name in os.environ:
                directories.extend(os.environ[name].split(os.pathsep))

        self._get_ld_so_conf_dirs("/etc/ld.so.conf", directories)

        bitage = platform.architecture()[0]

        unix_lib_dirs_list = []
        if bitage.startswith("64"):
            # prefer 64 bit if that is our arch
            unix_lib_dirs_list += ["/lib64", "/usr/lib64"]

        # must include standard libs, since those paths are also used by 64 bit
        # installs
        unix_lib_dirs_list += ["/lib", "/usr/lib"]
        if sys.platform.startswith("linux"):
            # Try and support multiarch work in Ubuntu
            # https://wiki.ubuntu.com/MultiarchSpec
            if bitage.startswith("32"):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ["/lib/i386-linux-gnu", "/usr/lib/i386-linux-gnu"]
            elif bitage.startswith("64"):
                # Assume Intel/AMD x86 compatible
                unix_lib_dirs_list += [
                    "/lib/x86_64-linux-gnu",
                    "/usr/lib/x86_64-linux-gnu",
                ]
            else:
                # guess...
                unix_lib_dirs_list += glob.glob("/lib/*linux-gnu")
        directories.extend(unix_lib_dirs_list)

        cache = {}
        lib_re = re.compile(r"lib(.*)\.s[ol]")
        # ext_re = re.compile(r"\.s[ol]$")
        for our_dir in directories.ordered():
            try:
                for path in glob.glob("%s/*.s[ol]*" % our_dir):
                    file = os.path.basename(path)

                    # Index by filename
                    cache_i = cache.setdefault(file, set())
                    cache_i.add(path)

                    # Index by library name
                    match = lib_re.match(file)
                    if match:
                        library = match.group(1)
                        cache_i = cache.setdefault(library, set())
                        cache_i.add(path)
            except OSError:
                pass

        self._ld_so_cache = cache

    def getplatformpaths(self, libname):
        if self._ld_so_cache is None:
            self._create_ld_so_cache()

        result = self._ld_so_cache.get(libname, set())
        for i in result:
            # we iterate through all found paths for library, since we may have
            # actually found multiple architectures or other library types that
            # may not load
            yield i


# Windows


class WindowsLibraryLoader(LibraryLoader):
    """Library loader for Microsoft Windows"""

    name_formats = ["%s.dll", "lib%s.dll", "%slib.dll", "%s"]

    class Lookup(LibraryLoader.Lookup):
        """Lookup class for Windows libraries..."""

        def __init__(self, path):
            super(WindowsLibraryLoader.Lookup, self).__init__(path)
            self.access["stdcall"] = ctypes.windll.LoadLibrary(path)


# Platform switching

# If your value of sys.platform does not appear in this dict, please contact
# the Ctypesgen maintainers.

loaderclass = {
    "darwin": DarwinLibraryLoader,
    "cygwin": WindowsLibraryLoader,
    "win32": WindowsLibraryLoader,
    "msys": WindowsLibraryLoader,
}

load_library = loaderclass.get(sys.platform, PosixLibraryLoader)()


def add_library_search_dirs(other_dirs):
    """
    Add libraries to search paths.
    If library paths are relative, convert them to absolute with respect to this
    file's directory
    """
    for path in other_dirs:
        if not os.path.isabs(path):
            path = os.path.abspath(path)
        load_library.other_dirs.append(path)


del loaderclass

# End loader

add_library_search_dirs([])

# Begin libraries
_libs["WSTP64i4"] = load_library("WSTP64i4")

# 1 libraries
# End libraries

# No modules

wint = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 941

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 949
class struct__wint(Structure):
    pass

struct__wint.__slots__ = [
    'low',
    'hi',
]
struct__wint._fields_ = [
    ('low', wint),
    ('hi', wint),
]

wint64 = struct__wint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 949

mllong32 = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 988

wslong32 = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 989

mlulong32 = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 990

wsulong32 = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 991

wsint64 = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 994

wsint64 = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 995

mluint64 = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 996

wsuint64 = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 997

mlbigint = wsint64# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 999

mlbiguint = mluint64# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1000

uchar_ct = c_ubyte# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1281

ucharp_ct = POINTER(uchar_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1282

ucharpp_ct = POINTER(ucharp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1283

ucharppp_ct = POINTER(ucharpp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1284

ushort_ct = c_ushort# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1285

ushortp_ct = POINTER(ushort_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1286

ushortpp_ct = POINTER(ushortp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1287

ushortppp_ct = POINTER(ushortpp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1288

uint_ct = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1289

uintp_ct = POINTER(c_uint)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1290

uintpp_ct = POINTER(uintp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1291

int_ct = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1292

voidp_ct = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1293

voidpp_ct = POINTER(voidp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1294

charp_ct = String# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1295

charpp_ct = POINTER(charp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1296

charppp_ct = POINTER(charpp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1297

long_ct = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1298

longp_ct = POINTER(long_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1299

longpp_ct = POINTER(longp_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1300

ulong_ct = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1301

ulongp_ct = POINTER(ulong_ct)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1302

kushortp_ct = POINTER(c_ushort)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1307

kushortpp_ct = POINTER(POINTER(c_ushort))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1308

kuintp_ct = POINTER(c_uint)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1309

kuintpp_ct = POINTER(POINTER(c_uint))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1310

kucharp_ct = POINTER(c_ubyte)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1311

kucharpp_ct = POINTER(POINTER(c_ubyte))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1312

kcharp_ct = String# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1313

kcharpp_ct = POINTER(POINTER(c_char))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1314

kvoidp_ct = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1315

WSPointer = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1318

MLENVPARAM = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1321

WSEnvironmentParameter = MLENVPARAM# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1322

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1327
class struct_ml_environment(Structure):
    pass

WSENV = POINTER(struct_ml_environment)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1327

WSEnvironment = WSENV# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1328

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1333
class struct_MLink(Structure):
    pass

WSLINK = POINTER(struct_MLink)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1333

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1338
class struct_MLinkMark(Structure):
    pass

WSMARK = POINTER(struct_MLinkMark)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1338

MLINKMark = WSMARK# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1339

mlapi_token = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1346

mlapi__token = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1349

mlapi__tokenp = POINTER(mlapi__token)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1350

wsapi_packet = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1355

mlapi_error = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1358

mlapi__error = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1359

long_st = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1361

longp_st = longp_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1362

longpp_st = POINTER(longp_st)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1363

long_et = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1365

mlapi_result = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1371

__MLProcPtr__ = CFUNCTYPE(UNCHECKED(c_long), )# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1382

dev_voidp = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1395

dev_type = dev_voidp# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1396

dev_typep = POINTER(dev_type)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1397

devproc_error = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1398

devproc_selector = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1399

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1419
class struct_read_buf(Structure):
    pass

struct_read_buf.__slots__ = [
    'length',
    'ptr',
]
struct_read_buf._fields_ = [
    ('length', c_ushort),
    ('ptr', POINTER(c_ubyte)),
]

read_buf = struct_read_buf# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1419

read_bufp = POINTER(read_buf)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1421

read_bufpp = POINTER(read_bufp)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1422

MLDeviceProcPtr = CFUNCTYPE(UNCHECKED(devproc_error), dev_type, devproc_selector, dev_voidp, dev_voidp)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1424

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1425
for _lib in _libs.values():
    if not _lib.has("MLDeviceMain", "cdecl"):
        continue
    MLDeviceMain = _lib.get("MLDeviceMain", "cdecl")
    MLDeviceMain.argtypes = [dev_type, devproc_selector, dev_voidp, dev_voidp]
    MLDeviceMain.restype = devproc_error
    break

MLDeviceUPP = MLDeviceProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1427

dev_main_type = MLDeviceUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1431

dev_main_typep = POINTER(dev_main_type)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1432

WSAllocatorProcPtr = CFUNCTYPE(UNCHECKED(POINTER(c_ubyte)), c_ulong)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1464

MLAllocatorUPP = WSAllocatorProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1469

WSDeallocatorProcPtr = CFUNCTYPE(UNCHECKED(None), POINTER(None))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1476

MLDeallocatorUPP = WSDeallocatorProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1478

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1491
if _libs["WSTP64i4"].has("WSAllocatorCast", "cdecl"):
    WSAllocatorCast = _libs["WSTP64i4"].get("WSAllocatorCast", "cdecl")
    WSAllocatorCast.argtypes = [WSAllocatorProcPtr]
    WSAllocatorCast.restype = __MLProcPtr__

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1492
if _libs["WSTP64i4"].has("WSDeallocatorCast", "cdecl"):
    WSDeallocatorCast = _libs["WSTP64i4"].get("WSDeallocatorCast", "cdecl")
    WSDeallocatorCast.argtypes = [WSDeallocatorProcPtr]
    WSDeallocatorCast.restype = __MLProcPtr__

WSAllocator = MLAllocatorUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1497

WSAllocatorp = POINTER(WSAllocator)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1498

WSDeallocator = MLDeallocatorUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1502

WSDeallocatorp = POINTER(WSDeallocator)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1503

_uint32_nt = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2490

_sint32_nt = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2491

uchar_nt = c_ubyte# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2717

ucharp_nt = POINTER(uchar_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2718

ucharpp_nt = POINTER(ucharp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2719

short_nt = c_short# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2721

shortp_nt = POINTER(short_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2722

shortpp_nt = POINTER(shortp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2723

int_nt = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2725

intp_nt = POINTER(int_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2726

intpp_nt = POINTER(intp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2727

long_nt = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2729

longp_nt = POINTER(long_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2730

longpp_nt = POINTER(longp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2731

int64_nt = wsint64# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2733

int64p_nt = POINTER(int64_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2734

int64pp_nt = POINTER(int64p_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2735

float_nt = c_float# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2737

floatp_nt = POINTER(float_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2738

floatpp_nt = POINTER(floatp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2739

double_nt = c_double# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2741

doublep_nt = POINTER(double_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2742

doublepp_nt = POINTER(doublep_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2743

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2747
class struct__i87extended_nt(Structure):
    pass

struct__i87extended_nt.__slots__ = [
    'w',
]
struct__i87extended_nt._fields_ = [
    ('w', c_ushort * int(5)),
]

wsextended_double = c_longdouble# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2774

extended_nt = c_longdouble# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2776

extendedp_nt = POINTER(extended_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2777

extendedpp_nt = POINTER(extendedp_nt)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2778

dev_world = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3158

dev_cookie = WSLINK# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3159

dev_worldp = POINTER(dev_world)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3161

dev_cookiep = POINTER(dev_cookie)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3162

dev_allocator = MLAllocatorUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3165

dev_deallocator = MLDeallocatorUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3169

world_main_type = dev_main_type# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3173

dev_mode = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3220

dev_modep = POINTER(dev_mode)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3233

dev_options = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3239

devyield_result = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3337

devyield_place = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3338

devyield_count = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3339

devyield_sleep = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3340

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3351
class struct_MLYieldParams(Structure):
    pass

WSYieldParameters = POINTER(struct_MLYieldParams)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3351

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3356
class union_anon_1(Union):
    pass

union_anon_1.__slots__ = [
    'l',
    'd',
    'p',
]
union_anon_1._fields_ = [
    ('l', c_long),
    ('d', c_double),
    ('p', POINTER(None)),
]

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3355
class struct_MLYieldData(Structure):
    pass

struct_MLYieldData.__slots__ = [
    'private_data',
]
struct_MLYieldData._fields_ = [
    ('private_data', union_anon_1 * int(8)),
]

MLYieldDataPointer = POINTER(struct_MLYieldData)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3357

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3359
for _lib in _libs.values():
    if not _lib.has("MLNewYieldData", "cdecl"):
        continue
    MLNewYieldData = _lib.get("MLNewYieldData", "cdecl")
    MLNewYieldData.argtypes = [MLYieldDataPointer]
    MLNewYieldData.restype = None
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3360
for _lib in _libs.values():
    if not _lib.has("MLFreeYieldData", "cdecl"):
        continue
    MLFreeYieldData = _lib.get("MLFreeYieldData", "cdecl")
    MLFreeYieldData.argtypes = [MLYieldDataPointer]
    MLFreeYieldData.restype = None
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3361
for _lib in _libs.values():
    if not _lib.has("MLResetYieldData", "cdecl"):
        continue
    MLResetYieldData = _lib.get("MLResetYieldData", "cdecl")
    MLResetYieldData.argtypes = [MLYieldDataPointer, devyield_place]
    MLResetYieldData.restype = WSYieldParameters
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3362
for _lib in _libs.values():
    if not _lib.has("MLSetYieldParameter", "cdecl"):
        continue
    MLSetYieldParameter = _lib.get("MLSetYieldParameter", "cdecl")
    MLSetYieldParameter.argtypes = [WSYieldParameters, c_ulong, POINTER(None), POINTER(c_ulong)]
    MLSetYieldParameter.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3363
for _lib in _libs.values():
    if not _lib.has("MLYieldParameter", "cdecl"):
        continue
    MLYieldParameter = _lib.get("MLYieldParameter", "cdecl")
    MLYieldParameter.argtypes = [WSYieldParameters, c_ulong, POINTER(None), POINTER(c_ulong)]
    MLYieldParameter.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3364
for _lib in _libs.values():
    if not _lib.has("MLSetSleepYP", "cdecl"):
        continue
    MLSetSleepYP = _lib.get("MLSetSleepYP", "cdecl")
    MLSetSleepYP.argtypes = [WSYieldParameters, devyield_sleep]
    MLSetSleepYP.restype = devyield_sleep
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3365
for _lib in _libs.values():
    if not _lib.has("MLSetCountYP", "cdecl"):
        continue
    MLSetCountYP = _lib.get("MLSetCountYP", "cdecl")
    MLSetCountYP.argtypes = [WSYieldParameters, devyield_count]
    MLSetCountYP.restype = devyield_count
    break

enum_anon_2 = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3368

MLSleepParameter = 1# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3368

MLCountParameter = (MLSleepParameter + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3368

MLPlaceParameter = (MLCountParameter + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3368

WSYielderProcPtr = CFUNCTYPE(UNCHECKED(c_int), WSLINK, WSYieldParameters)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3374

MLDeviceYielderProcPtr = WSYielderProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3375

MLYielderUPP = WSYielderProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3377

MLDeviceYielderUPP = WSYielderProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3377

WSYieldFunctionType = MLYielderUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3382

WSYieldFunctionObject = MLYielderUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3384

dev_yielder = WSYieldFunctionObject# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3386

dev_yielderp = POINTER(dev_yielder)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3387

dev_message = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3390

dev_messagep = POINTER(dev_message)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3391

WSHandlerProcPtr = CFUNCTYPE(UNCHECKED(None), WSLINK, c_int, c_int)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3394

MLDeviceHandlerProcPtr = WSHandlerProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3395

MLHandlerUPP = WSHandlerProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3398

MLDeviceHandlerUPP = WSHandlerProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3398

WSMessageHandlerType = MLHandlerUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3403

WSMessageHandlerObject = MLHandlerUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3405

dev_msghandler = WSMessageHandlerObject# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3408

dev_msghandlerp = POINTER(dev_msghandler)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3409

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3422
if _libs["WSTP64i4"].has("WSSleepYP", "cdecl"):
    WSSleepYP = _libs["WSTP64i4"].get("WSSleepYP", "cdecl")
    WSSleepYP.argtypes = [WSYieldParameters]
    WSSleepYP.restype = devyield_sleep

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3423
if _libs["WSTP64i4"].has("WSCountYP", "cdecl"):
    WSCountYP = _libs["WSTP64i4"].get("WSCountYP", "cdecl")
    WSCountYP.argtypes = [WSYieldParameters]
    WSCountYP.restype = devyield_count

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3425
if _libs["WSTP64i4"].has("WSCreateYieldFunction", "cdecl"):
    WSCreateYieldFunction = _libs["WSTP64i4"].get("WSCreateYieldFunction", "cdecl")
    WSCreateYieldFunction.argtypes = [WSEnvironment, WSYieldFunctionType, POINTER(None)]
    WSCreateYieldFunction.restype = WSYieldFunctionObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3427
if _libs["WSTP64i4"].has("WSDestroyYieldFunction", "cdecl"):
    WSDestroyYieldFunction = _libs["WSTP64i4"].get("WSDestroyYieldFunction", "cdecl")
    WSDestroyYieldFunction.argtypes = [WSYieldFunctionObject]
    WSDestroyYieldFunction.restype = WSYieldFunctionType

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3429
if _libs["WSTP64i4"].has("WSCallYieldFunction", "cdecl"):
    WSCallYieldFunction = _libs["WSTP64i4"].get("WSCallYieldFunction", "cdecl")
    WSCallYieldFunction.argtypes = [WSYieldFunctionObject, WSLINK, WSYieldParameters]
    WSCallYieldFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3431
if _libs["WSTP64i4"].has("WSCreateMessageHandler", "cdecl"):
    WSCreateMessageHandler = _libs["WSTP64i4"].get("WSCreateMessageHandler", "cdecl")
    WSCreateMessageHandler.argtypes = [WSEnvironment, WSMessageHandlerType, POINTER(None)]
    WSCreateMessageHandler.restype = WSMessageHandlerObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3434
if _libs["WSTP64i4"].has("WSDestroyMessageHandler", "cdecl"):
    WSDestroyMessageHandler = _libs["WSTP64i4"].get("WSDestroyMessageHandler", "cdecl")
    WSDestroyMessageHandler.argtypes = [WSMessageHandlerObject]
    WSDestroyMessageHandler.restype = WSMessageHandlerType

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3436
if _libs["WSTP64i4"].has("WSCallMessageHandler", "cdecl"):
    WSCallMessageHandler = _libs["WSTP64i4"].get("WSCallMessageHandler", "cdecl")
    WSCallMessageHandler.argtypes = [WSMessageHandlerObject, WSLINK, c_int, c_int]
    WSCallMessageHandler.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3440
if _libs["WSTP64i4"].has("WSYielderCast", "cdecl"):
    WSYielderCast = _libs["WSTP64i4"].get("WSYielderCast", "cdecl")
    WSYielderCast.argtypes = [WSYielderProcPtr]
    WSYielderCast.restype = __MLProcPtr__

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3441
if _libs["WSTP64i4"].has("WSHandlerCast", "cdecl"):
    WSHandlerCast = _libs["WSTP64i4"].get("WSHandlerCast", "cdecl")
    WSHandlerCast.argtypes = [WSHandlerProcPtr]
    WSHandlerCast.restype = __MLProcPtr__

MLSigHandlerProcPtr = CFUNCTYPE(UNCHECKED(None), c_int)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3453

WSSignalHandlerType = MLSigHandlerProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3455

MLSignalHandlerObject = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3456

WSParametersPointer = String# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3482

MLParameters = c_char * int(356)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3483

WSUserProcPtr = CFUNCTYPE(UNCHECKED(None), WSLINK)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3490

MLUserUPP = WSUserProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3493

WSUserFunctionType = MLUserUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3496

WSUserFunctionTypePointer = POINTER(WSUserFunctionType)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3497

WSUserFunction = MLUserUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3499

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3576
if _libs["WSTP64i4"].has("WSNewParameters", "cdecl"):
    WSNewParameters = _libs["WSTP64i4"].get("WSNewParameters", "cdecl")
    WSNewParameters.argtypes = [c_ulong, c_ulong]
    WSNewParameters.restype = WSEnvironmentParameter

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3577
if _libs["WSTP64i4"].has("WSReleaseParameters", "cdecl"):
    WSReleaseParameters = _libs["WSTP64i4"].get("WSReleaseParameters", "cdecl")
    WSReleaseParameters.argtypes = [WSEnvironmentParameter]
    WSReleaseParameters.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3578
if _libs["WSTP64i4"].has("WSSetAllocParameter", "cdecl"):
    WSSetAllocParameter = _libs["WSTP64i4"].get("WSSetAllocParameter", "cdecl")
    WSSetAllocParameter.argtypes = [WSEnvironmentParameter, WSAllocator, WSDeallocator]
    WSSetAllocParameter.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3579
if _libs["WSTP64i4"].has("WSSetThreadSafeLinksParameter", "cdecl"):
    WSSetThreadSafeLinksParameter = _libs["WSTP64i4"].get("WSSetThreadSafeLinksParameter", "cdecl")
    WSSetThreadSafeLinksParameter.argtypes = [WSEnvironmentParameter]
    WSSetThreadSafeLinksParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3580
if _libs["WSTP64i4"].has("WSAllocParameter", "cdecl"):
    WSAllocParameter = _libs["WSTP64i4"].get("WSAllocParameter", "cdecl")
    WSAllocParameter.argtypes = [WSEnvironmentParameter, POINTER(WSAllocator), POINTER(WSDeallocator)]
    WSAllocParameter.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3581
if _libs["WSTP64i4"].has("WSSetResourceParameter", "cdecl"):
    WSSetResourceParameter = _libs["WSTP64i4"].get("WSSetResourceParameter", "cdecl")
    WSSetResourceParameter.argtypes = [WSEnvironmentParameter, String]
    WSSetResourceParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3582
if _libs["WSTP64i4"].has("WSSetDeviceParameter", "cdecl"):
    WSSetDeviceParameter = _libs["WSTP64i4"].get("WSSetDeviceParameter", "cdecl")
    WSSetDeviceParameter.argtypes = [WSEnvironmentParameter, String]
    WSSetDeviceParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3583
if _libs["WSTP64i4"].has("WSErrorParameter", "cdecl"):
    WSErrorParameter = _libs["WSTP64i4"].get("WSErrorParameter", "cdecl")
    WSErrorParameter.argtypes = [WSEnvironmentParameter]
    WSErrorParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3584
if _libs["WSTP64i4"].has("WSSetEncodingParameter", "cdecl"):
    WSSetEncodingParameter = _libs["WSTP64i4"].get("WSSetEncodingParameter", "cdecl")
    WSSetEncodingParameter.argtypes = [WSEnvironmentParameter, c_uint]
    WSSetEncodingParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3585
if _libs["WSTP64i4"].has("WSDoNotHandleSignalParameter", "cdecl"):
    WSDoNotHandleSignalParameter = _libs["WSTP64i4"].get("WSDoNotHandleSignalParameter", "cdecl")
    WSDoNotHandleSignalParameter.argtypes = [WSEnvironmentParameter, c_int]
    WSDoNotHandleSignalParameter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3589
if _libs["WSTP64i4"].has("WSStopHandlingSignal", "cdecl"):
    WSStopHandlingSignal = _libs["WSTP64i4"].get("WSStopHandlingSignal", "cdecl")
    WSStopHandlingSignal.argtypes = [WSEnvironment, c_int]
    WSStopHandlingSignal.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3590
if _libs["WSTP64i4"].has("WSHandleSignal", "cdecl"):
    WSHandleSignal = _libs["WSTP64i4"].get("WSHandleSignal", "cdecl")
    WSHandleSignal.argtypes = [WSEnvironment, c_int]
    WSHandleSignal.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3592
if _libs["WSTP64i4"].has("WSSetEnvironmentData", "cdecl"):
    WSSetEnvironmentData = _libs["WSTP64i4"].get("WSSetEnvironmentData", "cdecl")
    WSSetEnvironmentData.argtypes = [WSEnvironment, POINTER(None)]
    WSSetEnvironmentData.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3593
if _libs["WSTP64i4"].has("WSEnvironmentData", "cdecl"):
    WSEnvironmentData = _libs["WSTP64i4"].get("WSEnvironmentData", "cdecl")
    WSEnvironmentData.argtypes = [WSEnvironment]
    WSEnvironmentData.restype = POINTER(c_ubyte)
    WSEnvironmentData.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3594
if _libs["WSTP64i4"].has("WSSetSignalHandler", "cdecl"):
    WSSetSignalHandler = _libs["WSTP64i4"].get("WSSetSignalHandler", "cdecl")
    WSSetSignalHandler.argtypes = [WSEnvironment, c_int, POINTER(None)]
    WSSetSignalHandler.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3595
if _libs["WSTP64i4"].has("WSSetSignalHandlerFromFunction", "cdecl"):
    WSSetSignalHandlerFromFunction = _libs["WSTP64i4"].get("WSSetSignalHandlerFromFunction", "cdecl")
    WSSetSignalHandlerFromFunction.argtypes = [WSEnvironment, c_int, WSSignalHandlerType]
    WSSetSignalHandlerFromFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3596
if _libs["WSTP64i4"].has("WSUnsetSignalHandler", "cdecl"):
    WSUnsetSignalHandler = _libs["WSTP64i4"].get("WSUnsetSignalHandler", "cdecl")
    WSUnsetSignalHandler.argtypes = [WSEnvironment, c_int, WSSignalHandlerType]
    WSUnsetSignalHandler.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3598
if _libs["WSTP64i4"].has("WSSetSymbolReplacement", "cdecl"):
    WSSetSymbolReplacement = _libs["WSTP64i4"].get("WSSetSymbolReplacement", "cdecl")
    WSSetSymbolReplacement.argtypes = [WSLINK, String, c_int, String, c_int]
    WSSetSymbolReplacement.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3599
if _libs["WSTP64i4"].has("WSClearSymbolReplacement", "cdecl"):
    WSClearSymbolReplacement = _libs["WSTP64i4"].get("WSClearSymbolReplacement", "cdecl")
    WSClearSymbolReplacement.argtypes = [WSLINK, c_long]
    WSClearSymbolReplacement.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3600
if _libs["WSTP64i4"].has("WSClearAllSymbolReplacements", "cdecl"):
    WSClearAllSymbolReplacements = _libs["WSTP64i4"].get("WSClearAllSymbolReplacements", "cdecl")
    WSClearAllSymbolReplacements.argtypes = [WSLINK]
    WSClearAllSymbolReplacements.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3603
if _libs["WSTP64i4"].has("WSInitialize", "cdecl"):
    WSInitialize = _libs["WSTP64i4"].get("WSInitialize", "cdecl")
    WSInitialize.argtypes = [WSEnvironmentParameter]
    WSInitialize.restype = WSEnvironment

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3606
if _libs["WSTP64i4"].has("WSDeinitialize", "cdecl"):
    WSDeinitialize = _libs["WSTP64i4"].get("WSDeinitialize", "cdecl")
    WSDeinitialize.argtypes = [WSEnvironment]
    WSDeinitialize.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3610
if _libs["WSTP64i4"].has("WSVersionNumbers", "cdecl"):
    WSVersionNumbers = _libs["WSTP64i4"].get("WSVersionNumbers", "cdecl")
    WSVersionNumbers.argtypes = [WSEnvironment, POINTER(c_int), POINTER(c_int), POINTER(c_int)]
    WSVersionNumbers.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3612
if _libs["WSTP64i4"].has("WSCompilerID", "cdecl"):
    WSCompilerID = _libs["WSTP64i4"].get("WSCompilerID", "cdecl")
    WSCompilerID.argtypes = [WSEnvironment, POINTER(POINTER(c_char))]
    WSCompilerID.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3613
if _libs["WSTP64i4"].has("WSReleaseCompilerID", "cdecl"):
    WSReleaseCompilerID = _libs["WSTP64i4"].get("WSReleaseCompilerID", "cdecl")
    WSReleaseCompilerID.argtypes = [WSEnvironment, String]
    WSReleaseCompilerID.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3615
if _libs["WSTP64i4"].has("WSUCS2CompilerID", "cdecl"):
    WSUCS2CompilerID = _libs["WSTP64i4"].get("WSUCS2CompilerID", "cdecl")
    WSUCS2CompilerID.argtypes = [WSEnvironment, POINTER(POINTER(c_ushort)), POINTER(c_int)]
    WSUCS2CompilerID.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3616
if _libs["WSTP64i4"].has("WSReleaseUCS2CompilerID", "cdecl"):
    WSReleaseUCS2CompilerID = _libs["WSTP64i4"].get("WSReleaseUCS2CompilerID", "cdecl")
    WSReleaseUCS2CompilerID.argtypes = [WSEnvironment, POINTER(c_ushort), c_int]
    WSReleaseUCS2CompilerID.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3618
if _libs["WSTP64i4"].has("WSUTF8CompilerID", "cdecl"):
    WSUTF8CompilerID = _libs["WSTP64i4"].get("WSUTF8CompilerID", "cdecl")
    WSUTF8CompilerID.argtypes = [WSEnvironment, POINTER(POINTER(c_ubyte)), POINTER(c_int)]
    WSUTF8CompilerID.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3619
if _libs["WSTP64i4"].has("WSReleaseUTF8CompilerID", "cdecl"):
    WSReleaseUTF8CompilerID = _libs["WSTP64i4"].get("WSReleaseUTF8CompilerID", "cdecl")
    WSReleaseUTF8CompilerID.argtypes = [WSEnvironment, POINTER(c_ubyte), c_int]
    WSReleaseUTF8CompilerID.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3621
if _libs["WSTP64i4"].has("WSUTF16CompilerID", "cdecl"):
    WSUTF16CompilerID = _libs["WSTP64i4"].get("WSUTF16CompilerID", "cdecl")
    WSUTF16CompilerID.argtypes = [WSEnvironment, POINTER(POINTER(c_ushort)), POINTER(c_int)]
    WSUTF16CompilerID.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3622
if _libs["WSTP64i4"].has("WSReleaseUTF16CompilerID", "cdecl"):
    WSReleaseUTF16CompilerID = _libs["WSTP64i4"].get("WSReleaseUTF16CompilerID", "cdecl")
    WSReleaseUTF16CompilerID.argtypes = [WSEnvironment, POINTER(c_ushort), c_int]
    WSReleaseUTF16CompilerID.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3624
if _libs["WSTP64i4"].has("WSUTF32CompilerID", "cdecl"):
    WSUTF32CompilerID = _libs["WSTP64i4"].get("WSUTF32CompilerID", "cdecl")
    WSUTF32CompilerID.argtypes = [WSEnvironment, POINTER(POINTER(c_uint)), POINTER(c_int)]
    WSUTF32CompilerID.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3625
if _libs["WSTP64i4"].has("WSReleaseUTF32CompilerID", "cdecl"):
    WSReleaseUTF32CompilerID = _libs["WSTP64i4"].get("WSReleaseUTF32CompilerID", "cdecl")
    WSReleaseUTF32CompilerID.argtypes = [WSEnvironment, POINTER(c_uint), c_int]
    WSReleaseUTF32CompilerID.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3631
if _libs["WSTP64i4"].has("WSBegin", "cdecl"):
    WSBegin = _libs["WSTP64i4"].get("WSBegin", "cdecl")
    WSBegin.argtypes = [WSEnvironmentParameter]
    WSBegin.restype = WSEnvironment

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3634
if _libs["WSTP64i4"].has("WSEnd", "cdecl"):
    WSEnd = _libs["WSTP64i4"].get("WSEnd", "cdecl")
    WSEnd.argtypes = [WSEnvironment]
    WSEnd.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3638
if _libs["WSTP64i4"].has("WSSetEnvIDString", "cdecl"):
    WSSetEnvIDString = _libs["WSTP64i4"].get("WSSetEnvIDString", "cdecl")
    WSSetEnvIDString.argtypes = [WSEnvironment, String]
    WSSetEnvIDString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3639
if _libs["WSTP64i4"].has("WSGetLinkedEnvIDString", "cdecl"):
    WSGetLinkedEnvIDString = _libs["WSTP64i4"].get("WSGetLinkedEnvIDString", "cdecl")
    WSGetLinkedEnvIDString.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSGetLinkedEnvIDString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3640
if _libs["WSTP64i4"].has("WSReleaseEnvIDString", "cdecl"):
    WSReleaseEnvIDString = _libs["WSTP64i4"].get("WSReleaseEnvIDString", "cdecl")
    WSReleaseEnvIDString.argtypes = [WSLINK, String]
    WSReleaseEnvIDString.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3647
if _libs["WSTP64i4"].has("WSGetNetworkAddressList", "cdecl"):
    WSGetNetworkAddressList = _libs["WSTP64i4"].get("WSGetNetworkAddressList", "cdecl")
    WSGetNetworkAddressList.argtypes = [WSEnvironment, POINTER(c_ulong)]
    WSGetNetworkAddressList.restype = POINTER(POINTER(c_char))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3648
if _libs["WSTP64i4"].has("WSReleaseNetworkAddressList", "cdecl"):
    WSReleaseNetworkAddressList = _libs["WSTP64i4"].get("WSReleaseNetworkAddressList", "cdecl")
    WSReleaseNetworkAddressList.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), c_ulong]
    WSReleaseNetworkAddressList.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3651
if _libs["WSTP64i4"].has("WSGetDomainNameList", "cdecl"):
    WSGetDomainNameList = _libs["WSTP64i4"].get("WSGetDomainNameList", "cdecl")
    WSGetDomainNameList.argtypes = [WSEnvironment, POINTER(c_ulong)]
    WSGetDomainNameList.restype = POINTER(POINTER(c_char))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3652
if _libs["WSTP64i4"].has("WSReleaseDomainNameList", "cdecl"):
    WSReleaseDomainNameList = _libs["WSTP64i4"].get("WSReleaseDomainNameList", "cdecl")
    WSReleaseDomainNameList.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), c_ulong]
    WSReleaseDomainNameList.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3659
if _libs["WSTP64i4"].has("WSGetAvailableLinkProtocolNames", "cdecl"):
    WSGetAvailableLinkProtocolNames = _libs["WSTP64i4"].get("WSGetAvailableLinkProtocolNames", "cdecl")
    WSGetAvailableLinkProtocolNames.argtypes = [WSEnvironment, POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetAvailableLinkProtocolNames.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3660
if _libs["WSTP64i4"].has("WSReleaseLinkProtocolNames", "cdecl"):
    WSReleaseLinkProtocolNames = _libs["WSTP64i4"].get("WSReleaseLinkProtocolNames", "cdecl")
    WSReleaseLinkProtocolNames.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), c_int]
    WSReleaseLinkProtocolNames.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3665
if _libs["WSTP64i4"].has("WSGetLinksFromEnvironment", "cdecl"):
    WSGetLinksFromEnvironment = _libs["WSTP64i4"].get("WSGetLinksFromEnvironment", "cdecl")
    WSGetLinksFromEnvironment.argtypes = [WSEnvironment, POINTER(POINTER(WSLINK)), POINTER(c_int)]
    WSGetLinksFromEnvironment.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3666
if _libs["WSTP64i4"].has("WSReleaseLinksFromEnvironment", "cdecl"):
    WSReleaseLinksFromEnvironment = _libs["WSTP64i4"].get("WSReleaseLinksFromEnvironment", "cdecl")
    WSReleaseLinksFromEnvironment.argtypes = [WSEnvironment, POINTER(WSLINK), c_int]
    WSReleaseLinksFromEnvironment.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3695
if _libs["WSTP64i4"].has("WSNumericsQuery", "cdecl"):
    WSNumericsQuery = _libs["WSTP64i4"].get("WSNumericsQuery", "cdecl")
    WSNumericsQuery.argtypes = [WSEnvironment, c_ulong, POINTER(None), POINTER(None), POINTER(c_long)]
    WSNumericsQuery.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3703
if _libs["WSTP64i4"].has("WSValid", "cdecl"):
    WSValid = _libs["WSTP64i4"].get("WSValid", "cdecl")
    WSValid.argtypes = [WSLINK]
    WSValid.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3709
if _libs["WSTP64i4"].has("WSFilterArgv", "cdecl"):
    WSFilterArgv = _libs["WSTP64i4"].get("WSFilterArgv", "cdecl")
    WSFilterArgv.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), POINTER(POINTER(c_char))]
    WSFilterArgv.restype = POINTER(POINTER(c_char))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3712
if _libs["WSTP64i4"].has("WSFeatureString", "cdecl"):
    WSFeatureString = _libs["WSTP64i4"].get("WSFeatureString", "cdecl")
    WSFeatureString.argtypes = [WSLINK, String, c_long]
    WSFeatureString.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3713
if _libs["WSTP64i4"].has("WSOpenArgv", "cdecl"):
    WSOpenArgv = _libs["WSTP64i4"].get("WSOpenArgv", "cdecl")
    WSOpenArgv.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), POINTER(POINTER(c_char)), POINTER(c_int)]
    WSOpenArgv.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3714
if _libs["WSTP64i4"].has("WSOpenArgcArgv", "cdecl"):
    WSOpenArgcArgv = _libs["WSTP64i4"].get("WSOpenArgcArgv", "cdecl")
    WSOpenArgcArgv.argtypes = [WSEnvironment, c_int, POINTER(POINTER(c_char)), POINTER(c_int)]
    WSOpenArgcArgv.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3715
if _libs["WSTP64i4"].has("WSOpenString", "cdecl"):
    WSOpenString = _libs["WSTP64i4"].get("WSOpenString", "cdecl")
    WSOpenString.argtypes = [WSEnvironment, String, POINTER(c_int)]
    WSOpenString.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3716
if _libs["WSTP64i4"].has("WSLoopbackOpen", "cdecl"):
    WSLoopbackOpen = _libs["WSTP64i4"].get("WSLoopbackOpen", "cdecl")
    WSLoopbackOpen.argtypes = [WSEnvironment, POINTER(c_int)]
    WSLoopbackOpen.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3717
if _libs["WSTP64i4"].has("WSStringToArgv", "cdecl"):
    WSStringToArgv = _libs["WSTP64i4"].get("WSStringToArgv", "cdecl")
    WSStringToArgv.argtypes = [String, String, POINTER(POINTER(c_char)), c_int]
    WSStringToArgv.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3718
if _libs["WSTP64i4"].has("WSScanString", "cdecl"):
    WSScanString = _libs["WSTP64i4"].get("WSScanString", "cdecl")
    WSScanString.argtypes = [POINTER(POINTER(c_char)), POINTER(POINTER(POINTER(c_char))), POINTER(POINTER(c_char)), POINTER(POINTER(c_char))]
    WSScanString.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3719
if _libs["WSTP64i4"].has("WSPrintArgv", "cdecl"):
    WSPrintArgv = _libs["WSTP64i4"].get("WSPrintArgv", "cdecl")
    WSPrintArgv.argtypes = [String, POINTER(POINTER(c_char)), POINTER(POINTER(POINTER(c_char))), POINTER(POINTER(c_char))]
    WSPrintArgv.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3721
if _libs["WSTP64i4"].has("WSErrorMessage", "cdecl"):
    WSErrorMessage = _libs["WSTP64i4"].get("WSErrorMessage", "cdecl")
    WSErrorMessage.argtypes = [WSLINK]
    WSErrorMessage.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3722
if _libs["WSTP64i4"].has("WSErrorString", "cdecl"):
    WSErrorString = _libs["WSTP64i4"].get("WSErrorString", "cdecl")
    WSErrorString.argtypes = [WSEnvironment, c_long]
    WSErrorString.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3724
if _libs["WSTP64i4"].has("WSUCS2ErrorMessage", "cdecl"):
    WSUCS2ErrorMessage = _libs["WSTP64i4"].get("WSUCS2ErrorMessage", "cdecl")
    WSUCS2ErrorMessage.argtypes = [WSLINK, POINTER(c_int)]
    WSUCS2ErrorMessage.restype = POINTER(c_ushort)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3725
if _libs["WSTP64i4"].has("WSUTF8ErrorMessage", "cdecl"):
    WSUTF8ErrorMessage = _libs["WSTP64i4"].get("WSUTF8ErrorMessage", "cdecl")
    WSUTF8ErrorMessage.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF8ErrorMessage.restype = POINTER(c_ubyte)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3726
if _libs["WSTP64i4"].has("WSUTF16ErrorMessage", "cdecl"):
    WSUTF16ErrorMessage = _libs["WSTP64i4"].get("WSUTF16ErrorMessage", "cdecl")
    WSUTF16ErrorMessage.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF16ErrorMessage.restype = POINTER(c_ushort)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3727
if _libs["WSTP64i4"].has("WSUTF32ErrorMessage", "cdecl"):
    WSUTF32ErrorMessage = _libs["WSTP64i4"].get("WSUTF32ErrorMessage", "cdecl")
    WSUTF32ErrorMessage.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF32ErrorMessage.restype = POINTER(c_uint)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3729
if _libs["WSTP64i4"].has("WSReleaseErrorMessage", "cdecl"):
    WSReleaseErrorMessage = _libs["WSTP64i4"].get("WSReleaseErrorMessage", "cdecl")
    WSReleaseErrorMessage.argtypes = [WSLINK, String]
    WSReleaseErrorMessage.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3730
if _libs["WSTP64i4"].has("WSReleaseUCS2ErrorMessage", "cdecl"):
    WSReleaseUCS2ErrorMessage = _libs["WSTP64i4"].get("WSReleaseUCS2ErrorMessage", "cdecl")
    WSReleaseUCS2ErrorMessage.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUCS2ErrorMessage.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3731
if _libs["WSTP64i4"].has("WSReleaseUTF8ErrorMessage", "cdecl"):
    WSReleaseUTF8ErrorMessage = _libs["WSTP64i4"].get("WSReleaseUTF8ErrorMessage", "cdecl")
    WSReleaseUTF8ErrorMessage.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseUTF8ErrorMessage.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3732
if _libs["WSTP64i4"].has("WSReleaseUTF16ErrorMessage", "cdecl"):
    WSReleaseUTF16ErrorMessage = _libs["WSTP64i4"].get("WSReleaseUTF16ErrorMessage", "cdecl")
    WSReleaseUTF16ErrorMessage.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUTF16ErrorMessage.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3733
if _libs["WSTP64i4"].has("WSReleaseUTF32ErrorMessage", "cdecl"):
    WSReleaseUTF32ErrorMessage = _libs["WSTP64i4"].get("WSReleaseUTF32ErrorMessage", "cdecl")
    WSReleaseUTF32ErrorMessage.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSReleaseUTF32ErrorMessage.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3735
if _libs["WSTP64i4"].has("WSOpen", "cdecl"):
    WSOpen = _libs["WSTP64i4"].get("WSOpen", "cdecl")
    WSOpen.argtypes = [c_int, POINTER(POINTER(c_char))]
    WSOpen.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3736
if _libs["WSTP64i4"].has("WSOpenInEnv", "cdecl"):
    WSOpenInEnv = _libs["WSTP64i4"].get("WSOpenInEnv", "cdecl")
    WSOpenInEnv.argtypes = [WSEnvironment, c_int, POINTER(POINTER(c_char)), POINTER(c_int)]
    WSOpenInEnv.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3738
if _libs["WSTP64i4"].has("WSDuplicateLink", "cdecl"):
    WSDuplicateLink = _libs["WSTP64i4"].get("WSDuplicateLink", "cdecl")
    WSDuplicateLink.argtypes = [WSLINK, String, POINTER(c_int)]
    WSDuplicateLink.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3740
if _libs["WSTP64i4"].has("WSConnect", "cdecl"):
    WSConnect = _libs["WSTP64i4"].get("WSConnect", "cdecl")
    WSConnect.argtypes = [WSLINK]
    WSConnect.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3741
if _libs["WSTP64i4"].has("WSActivate", "cdecl"):
    WSActivate = _libs["WSTP64i4"].get("WSActivate", "cdecl")
    WSActivate.argtypes = [WSLINK]
    WSActivate.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3745
class struct_feature_set(Structure):
    pass

feature_setp = POINTER(struct_feature_set)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3745

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3747
if _libs["WSTP64i4"].has("WSEstablish", "cdecl"):
    WSEstablish = _libs["WSTP64i4"].get("WSEstablish", "cdecl")
    WSEstablish.argtypes = [WSLINK, feature_setp]
    WSEstablish.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3749
if _libs["WSTP64i4"].has("WSEstablishString", "cdecl"):
    WSEstablishString = _libs["WSTP64i4"].get("WSEstablishString", "cdecl")
    WSEstablishString.argtypes = [WSLINK, String]
    WSEstablishString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3751
if _libs["WSTP64i4"].has("WSClose", "cdecl"):
    WSClose = _libs["WSTP64i4"].get("WSClose", "cdecl")
    WSClose.argtypes = [WSLINK]
    WSClose.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3753
if _libs["WSTP64i4"].has("WSSetUserData", "cdecl"):
    WSSetUserData = _libs["WSTP64i4"].get("WSSetUserData", "cdecl")
    WSSetUserData.argtypes = [WSLINK, POINTER(None), WSUserFunction]
    WSSetUserData.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3754
if _libs["WSTP64i4"].has("WSUserData", "cdecl"):
    WSUserData = _libs["WSTP64i4"].get("WSUserData", "cdecl")
    WSUserData.argtypes = [WSLINK, POINTER(WSUserFunctionType)]
    WSUserData.restype = POINTER(c_ubyte)
    WSUserData.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3755
if _libs["WSTP64i4"].has("WSSetUserBlock", "cdecl"):
    WSSetUserBlock = _libs["WSTP64i4"].get("WSSetUserBlock", "cdecl")
    WSSetUserBlock.argtypes = [WSLINK, POINTER(None)]
    WSSetUserBlock.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3756
if _libs["WSTP64i4"].has("WSUserBlock", "cdecl"):
    WSUserBlock = _libs["WSTP64i4"].get("WSUserBlock", "cdecl")
    WSUserBlock.argtypes = [WSLINK]
    WSUserBlock.restype = POINTER(c_ubyte)
    WSUserBlock.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3759
if _libs["WSTP64i4"].has("WSUserCast", "cdecl"):
    WSUserCast = _libs["WSTP64i4"].get("WSUserCast", "cdecl")
    WSUserCast.argtypes = [WSUserProcPtr]
    WSUserCast.restype = __MLProcPtr__

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3762
if _libs["WSTP64i4"].has("WSLogStreamToFile", "cdecl"):
    WSLogStreamToFile = _libs["WSTP64i4"].get("WSLogStreamToFile", "cdecl")
    WSLogStreamToFile.argtypes = [WSLINK, String]
    WSLogStreamToFile.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3763
if _libs["WSTP64i4"].has("WSDisableLoggingStream", "cdecl"):
    WSDisableLoggingStream = _libs["WSTP64i4"].get("WSDisableLoggingStream", "cdecl")
    WSDisableLoggingStream.argtypes = [WSLINK]
    WSDisableLoggingStream.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3764
if _libs["WSTP64i4"].has("WSEnableLoggingStream", "cdecl"):
    WSEnableLoggingStream = _libs["WSTP64i4"].get("WSEnableLoggingStream", "cdecl")
    WSEnableLoggingStream.argtypes = [WSLINK]
    WSEnableLoggingStream.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3765
if _libs["WSTP64i4"].has("WSStopLoggingStreamToFile", "cdecl"):
    WSStopLoggingStreamToFile = _libs["WSTP64i4"].get("WSStopLoggingStreamToFile", "cdecl")
    WSStopLoggingStreamToFile.argtypes = [WSLINK, String]
    WSStopLoggingStreamToFile.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3766
if _libs["WSTP64i4"].has("WSStopLoggingStream", "cdecl"):
    WSStopLoggingStream = _libs["WSTP64i4"].get("WSStopLoggingStream", "cdecl")
    WSStopLoggingStream.argtypes = [WSLINK]
    WSStopLoggingStream.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3768
if _libs["WSTP64i4"].has("WSLogFileNameForLink", "cdecl"):
    WSLogFileNameForLink = _libs["WSTP64i4"].get("WSLogFileNameForLink", "cdecl")
    WSLogFileNameForLink.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSLogFileNameForLink.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3769
if _libs["WSTP64i4"].has("WSReleaseLogFileNameForLink", "cdecl"):
    WSReleaseLogFileNameForLink = _libs["WSTP64i4"].get("WSReleaseLogFileNameForLink", "cdecl")
    WSReleaseLogFileNameForLink.argtypes = [WSLINK, String]
    WSReleaseLogFileNameForLink.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3780
if _libs["WSTP64i4"].has("WSName", "cdecl"):
    WSName = _libs["WSTP64i4"].get("WSName", "cdecl")
    WSName.argtypes = [WSLINK]
    WSName.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3781
if _libs["WSTP64i4"].has("WSLinkName", "cdecl"):
    WSLinkName = _libs["WSTP64i4"].get("WSLinkName", "cdecl")
    WSLinkName.argtypes = [WSLINK]
    WSLinkName.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3783
if _libs["WSTP64i4"].has("WSUCS2LinkName", "cdecl"):
    WSUCS2LinkName = _libs["WSTP64i4"].get("WSUCS2LinkName", "cdecl")
    WSUCS2LinkName.argtypes = [WSLINK, POINTER(c_int)]
    WSUCS2LinkName.restype = POINTER(c_ushort)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3784
if _libs["WSTP64i4"].has("WSUTF8LinkName", "cdecl"):
    WSUTF8LinkName = _libs["WSTP64i4"].get("WSUTF8LinkName", "cdecl")
    WSUTF8LinkName.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF8LinkName.restype = POINTER(c_ubyte)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3785
if _libs["WSTP64i4"].has("WSUTF16LinkName", "cdecl"):
    WSUTF16LinkName = _libs["WSTP64i4"].get("WSUTF16LinkName", "cdecl")
    WSUTF16LinkName.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF16LinkName.restype = POINTER(c_ushort)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3786
if _libs["WSTP64i4"].has("WSUTF32LinkName", "cdecl"):
    WSUTF32LinkName = _libs["WSTP64i4"].get("WSUTF32LinkName", "cdecl")
    WSUTF32LinkName.argtypes = [WSLINK, POINTER(c_int)]
    WSUTF32LinkName.restype = POINTER(c_uint)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3788
if _libs["WSTP64i4"].has("WSReleaseLinkName", "cdecl"):
    WSReleaseLinkName = _libs["WSTP64i4"].get("WSReleaseLinkName", "cdecl")
    WSReleaseLinkName.argtypes = [WSLINK, String]
    WSReleaseLinkName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3789
if _libs["WSTP64i4"].has("WSReleaseUCS2LinkName", "cdecl"):
    WSReleaseUCS2LinkName = _libs["WSTP64i4"].get("WSReleaseUCS2LinkName", "cdecl")
    WSReleaseUCS2LinkName.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUCS2LinkName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3790
if _libs["WSTP64i4"].has("WSReleaseUTF8LinkName", "cdecl"):
    WSReleaseUTF8LinkName = _libs["WSTP64i4"].get("WSReleaseUTF8LinkName", "cdecl")
    WSReleaseUTF8LinkName.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseUTF8LinkName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3791
if _libs["WSTP64i4"].has("WSReleaseUTF16LinkName", "cdecl"):
    WSReleaseUTF16LinkName = _libs["WSTP64i4"].get("WSReleaseUTF16LinkName", "cdecl")
    WSReleaseUTF16LinkName.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUTF16LinkName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3792
if _libs["WSTP64i4"].has("WSReleaseUTF32LinkName", "cdecl"):
    WSReleaseUTF32LinkName = _libs["WSTP64i4"].get("WSReleaseUTF32LinkName", "cdecl")
    WSReleaseUTF32LinkName.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSReleaseUTF32LinkName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3794
if _libs["WSTP64i4"].has("WSNumber", "cdecl"):
    WSNumber = _libs["WSTP64i4"].get("WSNumber", "cdecl")
    WSNumber.argtypes = [WSLINK]
    WSNumber.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3795
if _libs["WSTP64i4"].has("WSToLinkID", "cdecl"):
    WSToLinkID = _libs["WSTP64i4"].get("WSToLinkID", "cdecl")
    WSToLinkID.argtypes = [WSLINK]
    WSToLinkID.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3796
if _libs["WSTP64i4"].has("WSFromLinkID", "cdecl"):
    WSFromLinkID = _libs["WSTP64i4"].get("WSFromLinkID", "cdecl")
    WSFromLinkID.argtypes = [WSEnvironment, c_long]
    WSFromLinkID.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3798
if _libs["WSTP64i4"].has("WSSetName", "cdecl"):
    WSSetName = _libs["WSTP64i4"].get("WSSetName", "cdecl")
    WSSetName.argtypes = [WSLINK, String]
    if sizeof(c_int) == sizeof(c_void_p):
        WSSetName.restype = ReturnString
    else:
        WSSetName.restype = String
        WSSetName.errcheck = ReturnString

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3804
if _libs["WSTP64i4"].has("WSInit", "cdecl"):
    WSInit = _libs["WSTP64i4"].get("WSInit", "cdecl")
    WSInit.argtypes = [WSAllocator, WSDeallocator, POINTER(None)]
    WSInit.restype = POINTER(c_ubyte)
    WSInit.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3805
if _libs["WSTP64i4"].has("WSDeinit", "cdecl"):
    WSDeinit = _libs["WSTP64i4"].get("WSDeinit", "cdecl")
    WSDeinit.argtypes = [POINTER(None)]
    WSDeinit.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3806
if _libs["WSTP64i4"].has("WSEnclosingEnvironment", "cdecl"):
    WSEnclosingEnvironment = _libs["WSTP64i4"].get("WSEnclosingEnvironment", "cdecl")
    WSEnclosingEnvironment.argtypes = [POINTER(None)]
    WSEnclosingEnvironment.restype = POINTER(c_ubyte)
    WSEnclosingEnvironment.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3807
if _libs["WSTP64i4"].has("WLinkEnvironment", "cdecl"):
    WLinkEnvironment = _libs["WSTP64i4"].get("WLinkEnvironment", "cdecl")
    WLinkEnvironment.argtypes = [WSLINK]
    WLinkEnvironment.restype = POINTER(c_ubyte)
    WLinkEnvironment.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3810
if _libs["WSTP64i4"].has("WSEnableLinkLock", "cdecl"):
    WSEnableLinkLock = _libs["WSTP64i4"].get("WSEnableLinkLock", "cdecl")
    WSEnableLinkLock.argtypes = [WSLINK]
    WSEnableLinkLock.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3811
if _libs["WSTP64i4"].has("WSDisableLinkLock", "cdecl"):
    WSDisableLinkLock = _libs["WSTP64i4"].get("WSDisableLinkLock", "cdecl")
    WSDisableLinkLock.argtypes = [WSLINK]
    WSDisableLinkLock.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3814
if _libs["WSTP64i4"].has("WSLinkEnvironment", "cdecl"):
    WSLinkEnvironment = _libs["WSTP64i4"].get("WSLinkEnvironment", "cdecl")
    WSLinkEnvironment.argtypes = [WSLINK]
    WSLinkEnvironment.restype = WSEnvironment

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3817
if _libs["WSTP64i4"].has("WSIsLinkLoopback", "cdecl"):
    WSIsLinkLoopback = _libs["WSTP64i4"].get("WSIsLinkLoopback", "cdecl")
    WSIsLinkLoopback.argtypes = [WSLINK]
    WSIsLinkLoopback.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3820
if _libs["WSTP64i4"].has("WSDefaultYieldFunction", "cdecl"):
    WSDefaultYieldFunction = _libs["WSTP64i4"].get("WSDefaultYieldFunction", "cdecl")
    WSDefaultYieldFunction.argtypes = [WSEnvironment]
    WSDefaultYieldFunction.restype = WSYieldFunctionObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3822
if _libs["WSTP64i4"].has("WSSetDefaultYieldFunction", "cdecl"):
    WSSetDefaultYieldFunction = _libs["WSTP64i4"].get("WSSetDefaultYieldFunction", "cdecl")
    WSSetDefaultYieldFunction.argtypes = [WSEnvironment, WSYieldFunctionObject]
    WSSetDefaultYieldFunction.restype = c_int

WSLinkServer = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3841

WSNewLinkCallbackFunction = CFUNCTYPE(UNCHECKED(None), WSLinkServer, WSLINK)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3843

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3845
if _libs["WSTP64i4"].has("WSNewLinkServer", "cdecl"):
    WSNewLinkServer = _libs["WSTP64i4"].get("WSNewLinkServer", "cdecl")
    WSNewLinkServer.argtypes = [WSEnvironment, POINTER(None), POINTER(c_int)]
    WSNewLinkServer.restype = WSLinkServer

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3847
if _libs["WSTP64i4"].has("WSNewLinkServerWithPort", "cdecl"):
    WSNewLinkServerWithPort = _libs["WSTP64i4"].get("WSNewLinkServerWithPort", "cdecl")
    WSNewLinkServerWithPort.argtypes = [WSEnvironment, c_ushort, POINTER(None), POINTER(c_int)]
    WSNewLinkServerWithPort.restype = WSLinkServer

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3850
if _libs["WSTP64i4"].has("WSNewLinkServerWithPortAndInterface", "cdecl"):
    WSNewLinkServerWithPortAndInterface = _libs["WSTP64i4"].get("WSNewLinkServerWithPortAndInterface", "cdecl")
    WSNewLinkServerWithPortAndInterface.argtypes = [WSEnvironment, c_ushort, String, POINTER(None), POINTER(c_int)]
    WSNewLinkServerWithPortAndInterface.restype = WSLinkServer

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3853
if _libs["WSTP64i4"].has("WSShutdownLinkServer", "cdecl"):
    WSShutdownLinkServer = _libs["WSTP64i4"].get("WSShutdownLinkServer", "cdecl")
    WSShutdownLinkServer.argtypes = [WSLinkServer]
    WSShutdownLinkServer.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3855
if _libs["WSTP64i4"].has("WSRegisterCallbackFunctionWithLinkServer", "cdecl"):
    WSRegisterCallbackFunctionWithLinkServer = _libs["WSTP64i4"].get("WSRegisterCallbackFunctionWithLinkServer", "cdecl")
    WSRegisterCallbackFunctionWithLinkServer.argtypes = [WSLinkServer, WSNewLinkCallbackFunction]
    WSRegisterCallbackFunctionWithLinkServer.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3857
if _libs["WSTP64i4"].has("WSWaitForNewLinkFromLinkServer", "cdecl"):
    WSWaitForNewLinkFromLinkServer = _libs["WSTP64i4"].get("WSWaitForNewLinkFromLinkServer", "cdecl")
    WSWaitForNewLinkFromLinkServer.argtypes = [WSLinkServer, POINTER(c_int)]
    WSWaitForNewLinkFromLinkServer.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3859
if _libs["WSTP64i4"].has("WSPortFromLinkServer", "cdecl"):
    WSPortFromLinkServer = _libs["WSTP64i4"].get("WSPortFromLinkServer", "cdecl")
    WSPortFromLinkServer.argtypes = [WSLinkServer, POINTER(c_int)]
    WSPortFromLinkServer.restype = c_ushort

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3861
if _libs["WSTP64i4"].has("WSInterfaceFromLinkServer", "cdecl"):
    WSInterfaceFromLinkServer = _libs["WSTP64i4"].get("WSInterfaceFromLinkServer", "cdecl")
    WSInterfaceFromLinkServer.argtypes = [WSLinkServer, POINTER(c_int)]
    WSInterfaceFromLinkServer.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3863
if _libs["WSTP64i4"].has("WSContextFromLinkServer", "cdecl"):
    WSContextFromLinkServer = _libs["WSTP64i4"].get("WSContextFromLinkServer", "cdecl")
    WSContextFromLinkServer.argtypes = [WSLinkServer, POINTER(c_int)]
    WSContextFromLinkServer.restype = POINTER(c_ubyte)
    WSContextFromLinkServer.errcheck = lambda v,*a : cast(v, c_void_p)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3865
if _libs["WSTP64i4"].has("WSReleaseInterfaceFromLinkServer", "cdecl"):
    WSReleaseInterfaceFromLinkServer = _libs["WSTP64i4"].get("WSReleaseInterfaceFromLinkServer", "cdecl")
    WSReleaseInterfaceFromLinkServer.argtypes = [WSLinkServer, String]
    WSReleaseInterfaceFromLinkServer.restype = None

WSServiceRef = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3893

MLBrowseCallbackFunction = CFUNCTYPE(UNCHECKED(None), WSEnvironment, WSServiceRef, c_int, String, POINTER(None))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3895

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3898
if _libs["WSTP64i4"].has("WSBrowseForLinkServices", "cdecl"):
    WSBrowseForLinkServices = _libs["WSTP64i4"].get("WSBrowseForLinkServices", "cdecl")
    WSBrowseForLinkServices.argtypes = [WSEnvironment, MLBrowseCallbackFunction, String, String, POINTER(None), POINTER(WSServiceRef)]
    WSBrowseForLinkServices.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3902
if _libs["WSTP64i4"].has("WSStopBrowsingForLinkServices", "cdecl"):
    WSStopBrowsingForLinkServices = _libs["WSTP64i4"].get("WSStopBrowsingForLinkServices", "cdecl")
    WSStopBrowsingForLinkServices.argtypes = [WSEnvironment, WSServiceRef]
    WSStopBrowsingForLinkServices.restype = None

MLResolveCallbackFunction = CFUNCTYPE(UNCHECKED(None), WSEnvironment, WSServiceRef, String, String, String, c_int, POINTER(None))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3904

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3907
if _libs["WSTP64i4"].has("WSResolveLinkService", "cdecl"):
    WSResolveLinkService = _libs["WSTP64i4"].get("WSResolveLinkService", "cdecl")
    WSResolveLinkService.argtypes = [WSEnvironment, MLResolveCallbackFunction, String, String, POINTER(None), POINTER(WSServiceRef)]
    WSResolveLinkService.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3911
if _libs["WSTP64i4"].has("WSStopResolvingLinkService", "cdecl"):
    WSStopResolvingLinkService = _libs["WSTP64i4"].get("WSStopResolvingLinkService", "cdecl")
    WSStopResolvingLinkService.argtypes = [WSEnvironment, WSServiceRef]
    WSStopResolvingLinkService.restype = None

MLRegisterCallbackFunction = CFUNCTYPE(UNCHECKED(None), WSEnvironment, WSServiceRef, c_int, String, POINTER(None))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3913

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3916
if _libs["WSTP64i4"].has("WSRegisterLinkServiceWithPortAndHostname", "cdecl"):
    WSRegisterLinkServiceWithPortAndHostname = _libs["WSTP64i4"].get("WSRegisterLinkServiceWithPortAndHostname", "cdecl")
    WSRegisterLinkServiceWithPortAndHostname.argtypes = [WSEnvironment, String, String, c_ushort, String, MLRegisterCallbackFunction, String, POINTER(None), POINTER(WSServiceRef), POINTER(c_int)]
    WSRegisterLinkServiceWithPortAndHostname.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3920
if _libs["WSTP64i4"].has("WSRegisterLinkServiceWithHostname", "cdecl"):
    WSRegisterLinkServiceWithHostname = _libs["WSTP64i4"].get("WSRegisterLinkServiceWithHostname", "cdecl")
    WSRegisterLinkServiceWithHostname.argtypes = [WSEnvironment, String, String, String, MLRegisterCallbackFunction, String, POINTER(None), POINTER(WSServiceRef), POINTER(c_int)]
    WSRegisterLinkServiceWithHostname.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3924
if _libs["WSTP64i4"].has("WSRegisterLinkService", "cdecl"):
    WSRegisterLinkService = _libs["WSTP64i4"].get("WSRegisterLinkService", "cdecl")
    WSRegisterLinkService.argtypes = [WSEnvironment, String, String, MLRegisterCallbackFunction, String, POINTER(None), POINTER(WSServiceRef), POINTER(c_int)]
    WSRegisterLinkService.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3928
if _libs["WSTP64i4"].has("MLRegisterLinkServiceUsingLinkProtocol", "cdecl"):
    MLRegisterLinkServiceUsingLinkProtocol = _libs["WSTP64i4"].get("MLRegisterLinkServiceUsingLinkProtocol", "cdecl")
    MLRegisterLinkServiceUsingLinkProtocol.argtypes = [WSEnvironment, String, String, c_ushort, String, String, MLRegisterCallbackFunction, String, POINTER(None), POINTER(WSServiceRef), POINTER(c_int)]
    MLRegisterLinkServiceUsingLinkProtocol.restype = WSLINK

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3932
if _libs["WSTP64i4"].has("WSRegisterLinkServiceFromLinkServer", "cdecl"):
    WSRegisterLinkServiceFromLinkServer = _libs["WSTP64i4"].get("WSRegisterLinkServiceFromLinkServer", "cdecl")
    WSRegisterLinkServiceFromLinkServer.argtypes = [WSEnvironment, String, String, WSLinkServer, MLRegisterCallbackFunction, String, POINTER(None), POINTER(WSServiceRef), POINTER(c_int)]
    WSRegisterLinkServiceFromLinkServer.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3936
if _libs["WSTP64i4"].has("WSStopRegisteringLinkService", "cdecl"):
    WSStopRegisteringLinkService = _libs["WSTP64i4"].get("WSStopRegisteringLinkService", "cdecl")
    WSStopRegisteringLinkService.argtypes = [WSEnvironment, WSServiceRef]
    WSStopRegisteringLinkService.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3938
if _libs["WSTP64i4"].has("WSStopRegisteringLinkServiceForLink", "cdecl"):
    WSStopRegisteringLinkServiceForLink = _libs["WSTP64i4"].get("WSStopRegisteringLinkServiceForLink", "cdecl")
    WSStopRegisteringLinkServiceForLink.argtypes = [WSEnvironment, WSLINK, WSServiceRef]
    WSStopRegisteringLinkServiceForLink.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3940
if _libs["WSTP64i4"].has("WSServiceProtocolFromReference", "cdecl"):
    WSServiceProtocolFromReference = _libs["WSTP64i4"].get("WSServiceProtocolFromReference", "cdecl")
    WSServiceProtocolFromReference.argtypes = [WSEnvironment, WSServiceRef]
    WSServiceProtocolFromReference.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4154
if _libs["WSTP64i4"].has("WSError", "cdecl"):
    WSError = _libs["WSTP64i4"].get("WSError", "cdecl")
    WSError.argtypes = [WSLINK]
    WSError.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4155
if _libs["WSTP64i4"].has("WSClearError", "cdecl"):
    WSClearError = _libs["WSTP64i4"].get("WSClearError", "cdecl")
    WSClearError.argtypes = [WSLINK]
    WSClearError.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4156
if _libs["WSTP64i4"].has("WSSetError", "cdecl"):
    WSSetError = _libs["WSTP64i4"].get("WSSetError", "cdecl")
    WSSetError.argtypes = [WSLINK, c_int]
    WSSetError.restype = c_int

enum_anon_3 = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSTerminateMessage = 1# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSInterruptMessage = (WSTerminateMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSAbortMessage = (WSInterruptMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSEndPacketMessage = (WSAbortMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSSynchronizeMessage = (WSEndPacketMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSImDyingMessage = (WSSynchronizeMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSWaitingAcknowledgment = (WSImDyingMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSMarkTopLevelMessage = (WSWaitingAcknowledgment + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSLinkClosingMessage = (WSMarkTopLevelMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSAuthenticateFailure = (WSLinkClosingMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSSuspendActivitiesMessage = (WSAuthenticateFailure + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSResumeActivitiesMessage = (WSSuspendActivitiesMessage + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSFirstUserMessage = 128# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

WSLastUserMessage = 255# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4171

devinfo_selector = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4177

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4190
if _libs["WSTP64i4"].has("WSPutMessage", "cdecl"):
    WSPutMessage = _libs["WSTP64i4"].get("WSPutMessage", "cdecl")
    WSPutMessage.argtypes = [WSLINK, c_int]
    WSPutMessage.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4191
if _libs["WSTP64i4"].has("WSGetMessage", "cdecl"):
    WSGetMessage = _libs["WSTP64i4"].get("WSGetMessage", "cdecl")
    WSGetMessage.argtypes = [WSLINK, POINTER(c_int), POINTER(c_int)]
    WSGetMessage.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4192
if _libs["WSTP64i4"].has("WSMessageReady", "cdecl"):
    WSMessageReady = _libs["WSTP64i4"].get("WSMessageReady", "cdecl")
    WSMessageReady.argtypes = [WSLINK]
    WSMessageReady.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4194
if _libs["WSTP64i4"].has("WSPutMessageWithArg", "cdecl"):
    WSPutMessageWithArg = _libs["WSTP64i4"].get("WSPutMessageWithArg", "cdecl")
    WSPutMessageWithArg.argtypes = [WSLINK, c_int, c_int]
    WSPutMessageWithArg.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4197
if _libs["WSTP64i4"].has("WSGetMessageHandler", "cdecl"):
    WSGetMessageHandler = _libs["WSTP64i4"].get("WSGetMessageHandler", "cdecl")
    WSGetMessageHandler.argtypes = [WSLINK]
    WSGetMessageHandler.restype = WSMessageHandlerObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4198
if _libs["WSTP64i4"].has("WSMessageHandler", "cdecl"):
    WSMessageHandler = _libs["WSTP64i4"].get("WSMessageHandler", "cdecl")
    WSMessageHandler.argtypes = [WSLINK]
    WSMessageHandler.restype = WSMessageHandlerObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4200
if _libs["WSTP64i4"].has("WSGetYieldFunction", "cdecl"):
    WSGetYieldFunction = _libs["WSTP64i4"].get("WSGetYieldFunction", "cdecl")
    WSGetYieldFunction.argtypes = [WSLINK]
    WSGetYieldFunction.restype = WSYieldFunctionObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4201
if _libs["WSTP64i4"].has("WSYieldFunction", "cdecl"):
    WSYieldFunction = _libs["WSTP64i4"].get("WSYieldFunction", "cdecl")
    WSYieldFunction.argtypes = [WSLINK]
    WSYieldFunction.restype = WSYieldFunctionObject

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4203
if _libs["WSTP64i4"].has("WSSetMessageHandler", "cdecl"):
    WSSetMessageHandler = _libs["WSTP64i4"].get("WSSetMessageHandler", "cdecl")
    WSSetMessageHandler.argtypes = [WSLINK, WSMessageHandlerObject]
    WSSetMessageHandler.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4204
if _libs["WSTP64i4"].has("WSSetYieldFunction", "cdecl"):
    WSSetYieldFunction = _libs["WSTP64i4"].get("WSSetYieldFunction", "cdecl")
    WSSetYieldFunction.argtypes = [WSLINK, WSYieldFunctionObject]
    WSSetYieldFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4207
if _libs["WSTP64i4"].has("WSDeviceInformation", "cdecl"):
    WSDeviceInformation = _libs["WSTP64i4"].get("WSDeviceInformation", "cdecl")
    WSDeviceInformation.argtypes = [WSLINK, devinfo_selector, POINTER(None), POINTER(c_long)]
    WSDeviceInformation.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4209
if _libs["WSTP64i4"].has("WSLowLevelDeviceName", "cdecl"):
    WSLowLevelDeviceName = _libs["WSTP64i4"].get("WSLowLevelDeviceName", "cdecl")
    WSLowLevelDeviceName.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSLowLevelDeviceName.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4210
if _libs["WSTP64i4"].has("WSReleaseLowLevelDeviceName", "cdecl"):
    WSReleaseLowLevelDeviceName = _libs["WSTP64i4"].get("WSReleaseLowLevelDeviceName", "cdecl")
    WSReleaseLowLevelDeviceName.argtypes = [WSLINK, String]
    WSReleaseLowLevelDeviceName.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4231
if _libs["WSTP64i4"].has("WSGetNext", "cdecl"):
    WSGetNext = _libs["WSTP64i4"].get("WSGetNext", "cdecl")
    WSGetNext.argtypes = [WSLINK]
    WSGetNext.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4232
if _libs["WSTP64i4"].has("WSGetNextRaw", "cdecl"):
    WSGetNextRaw = _libs["WSTP64i4"].get("WSGetNextRaw", "cdecl")
    WSGetNextRaw.argtypes = [WSLINK]
    WSGetNextRaw.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4233
if _libs["WSTP64i4"].has("WSGetType", "cdecl"):
    WSGetType = _libs["WSTP64i4"].get("WSGetType", "cdecl")
    WSGetType.argtypes = [WSLINK]
    WSGetType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4234
if _libs["WSTP64i4"].has("WSGetRawType", "cdecl"):
    WSGetRawType = _libs["WSTP64i4"].get("WSGetRawType", "cdecl")
    WSGetRawType.argtypes = [WSLINK]
    WSGetRawType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4235
if _libs["WSTP64i4"].has("WSGetRawData", "cdecl"):
    WSGetRawData = _libs["WSTP64i4"].get("WSGetRawData", "cdecl")
    WSGetRawData.argtypes = [WSLINK, POINTER(c_ubyte), c_int, POINTER(c_int)]
    WSGetRawData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4236
if _libs["WSTP64i4"].has("WSGetData", "cdecl"):
    WSGetData = _libs["WSTP64i4"].get("WSGetData", "cdecl")
    WSGetData.argtypes = [WSLINK, String, c_int, POINTER(c_int)]
    WSGetData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4237
if _libs["WSTP64i4"].has("WSGetArgCount", "cdecl"):
    WSGetArgCount = _libs["WSTP64i4"].get("WSGetArgCount", "cdecl")
    WSGetArgCount.argtypes = [WSLINK, POINTER(c_int)]
    WSGetArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4238
if _libs["WSTP64i4"].has("WSGetRawArgCount", "cdecl"):
    WSGetRawArgCount = _libs["WSTP64i4"].get("WSGetRawArgCount", "cdecl")
    WSGetRawArgCount.argtypes = [WSLINK, POINTER(c_int)]
    WSGetRawArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4239
if _libs["WSTP64i4"].has("WSBytesToGet", "cdecl"):
    WSBytesToGet = _libs["WSTP64i4"].get("WSBytesToGet", "cdecl")
    WSBytesToGet.argtypes = [WSLINK, POINTER(c_int)]
    WSBytesToGet.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4240
if _libs["WSTP64i4"].has("WSRawBytesToGet", "cdecl"):
    WSRawBytesToGet = _libs["WSTP64i4"].get("WSRawBytesToGet", "cdecl")
    WSRawBytesToGet.argtypes = [WSLINK, POINTER(c_int)]
    WSRawBytesToGet.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4241
if _libs["WSTP64i4"].has("WSExpressionsToGet", "cdecl"):
    WSExpressionsToGet = _libs["WSTP64i4"].get("WSExpressionsToGet", "cdecl")
    WSExpressionsToGet.argtypes = [WSLINK, POINTER(c_int)]
    WSExpressionsToGet.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4243
if _libs["WSTP64i4"].has("WSNewPacket", "cdecl"):
    WSNewPacket = _libs["WSTP64i4"].get("WSNewPacket", "cdecl")
    WSNewPacket.argtypes = [WSLINK]
    WSNewPacket.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4245
if _libs["WSTP64i4"].has("WSTakeLast", "cdecl"):
    WSTakeLast = _libs["WSTP64i4"].get("WSTakeLast", "cdecl")
    WSTakeLast.argtypes = [WSLINK, c_int]
    WSTakeLast.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4247
if _libs["WSTP64i4"].has("WSFill", "cdecl"):
    WSFill = _libs["WSTP64i4"].get("WSFill", "cdecl")
    WSFill.argtypes = [WSLINK]
    WSFill.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4267
if _libs["WSTP64i4"].has("WSPutNext", "cdecl"):
    WSPutNext = _libs["WSTP64i4"].get("WSPutNext", "cdecl")
    WSPutNext.argtypes = [WSLINK, c_int]
    WSPutNext.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4268
if _libs["WSTP64i4"].has("WSPutType", "cdecl"):
    WSPutType = _libs["WSTP64i4"].get("WSPutType", "cdecl")
    WSPutType.argtypes = [WSLINK, c_int]
    WSPutType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4269
if _libs["WSTP64i4"].has("WSPutRawSize", "cdecl"):
    WSPutRawSize = _libs["WSTP64i4"].get("WSPutRawSize", "cdecl")
    WSPutRawSize.argtypes = [WSLINK, c_int]
    WSPutRawSize.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4270
if _libs["WSTP64i4"].has("WSPutRawData", "cdecl"):
    WSPutRawData = _libs["WSTP64i4"].get("WSPutRawData", "cdecl")
    WSPutRawData.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSPutRawData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4271
if _libs["WSTP64i4"].has("WSPutArgCount", "cdecl"):
    WSPutArgCount = _libs["WSTP64i4"].get("WSPutArgCount", "cdecl")
    WSPutArgCount.argtypes = [WSLINK, c_int]
    WSPutArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4272
if _libs["WSTP64i4"].has("WSPutComposite", "cdecl"):
    WSPutComposite = _libs["WSTP64i4"].get("WSPutComposite", "cdecl")
    WSPutComposite.argtypes = [WSLINK, c_int]
    WSPutComposite.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4273
if _libs["WSTP64i4"].has("WSBytesToPut", "cdecl"):
    WSBytesToPut = _libs["WSTP64i4"].get("WSBytesToPut", "cdecl")
    WSBytesToPut.argtypes = [WSLINK, POINTER(c_int)]
    WSBytesToPut.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4274
if _libs["WSTP64i4"].has("WSEndPacket", "cdecl"):
    WSEndPacket = _libs["WSTP64i4"].get("WSEndPacket", "cdecl")
    WSEndPacket.argtypes = [WSLINK]
    WSEndPacket.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4275
if _libs["WSTP64i4"].has("WSFlush", "cdecl"):
    WSFlush = _libs["WSTP64i4"].get("WSFlush", "cdecl")
    WSFlush.argtypes = [WSLINK]
    WSFlush.restype = c_int

decoder_mask = c_ulong# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4325

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5646
if _libs["WSTP64i4"].has("WSGetBinaryNumber", "cdecl"):
    WSGetBinaryNumber = _libs["WSTP64i4"].get("WSGetBinaryNumber", "cdecl")
    WSGetBinaryNumber.argtypes = [WSLINK, POINTER(None), c_long]
    WSGetBinaryNumber.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5656
if _libs["WSTP64i4"].has("WSGetShortInteger", "cdecl"):
    WSGetShortInteger = _libs["WSTP64i4"].get("WSGetShortInteger", "cdecl")
    WSGetShortInteger.argtypes = [WSLINK, POINTER(c_short)]
    WSGetShortInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5657
if _libs["WSTP64i4"].has("WSGetInteger", "cdecl"):
    WSGetInteger = _libs["WSTP64i4"].get("WSGetInteger", "cdecl")
    WSGetInteger.argtypes = [WSLINK, POINTER(c_int)]
    WSGetInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5658
if _libs["WSTP64i4"].has("WSGetLongInteger", "cdecl"):
    WSGetLongInteger = _libs["WSTP64i4"].get("WSGetLongInteger", "cdecl")
    WSGetLongInteger.argtypes = [WSLINK, POINTER(c_long)]
    WSGetLongInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5661
if _libs["WSTP64i4"].has("WSGetInteger16", "cdecl"):
    WSGetInteger16 = _libs["WSTP64i4"].get("WSGetInteger16", "cdecl")
    WSGetInteger16.argtypes = [WSLINK, POINTER(c_short)]
    WSGetInteger16.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5662
if _libs["WSTP64i4"].has("WSGetInteger32", "cdecl"):
    WSGetInteger32 = _libs["WSTP64i4"].get("WSGetInteger32", "cdecl")
    WSGetInteger32.argtypes = [WSLINK, POINTER(c_int)]
    WSGetInteger32.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5663
if _libs["WSTP64i4"].has("WSGetInteger64", "cdecl"):
    WSGetInteger64 = _libs["WSTP64i4"].get("WSGetInteger64", "cdecl")
    WSGetInteger64.argtypes = [WSLINK, POINTER(wsint64)]
    WSGetInteger64.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5666
if _libs["WSTP64i4"].has("WSGetInteger8", "cdecl"):
    WSGetInteger8 = _libs["WSTP64i4"].get("WSGetInteger8", "cdecl")
    WSGetInteger8.argtypes = [WSLINK, POINTER(c_ubyte)]
    WSGetInteger8.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5678
if _libs["WSTP64i4"].has("WSGetFloat", "cdecl"):
    WSGetFloat = _libs["WSTP64i4"].get("WSGetFloat", "cdecl")
    WSGetFloat.argtypes = [WSLINK, POINTER(c_float)]
    WSGetFloat.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5679
if _libs["WSTP64i4"].has("WSGetDouble", "cdecl"):
    WSGetDouble = _libs["WSTP64i4"].get("WSGetDouble", "cdecl")
    WSGetDouble.argtypes = [WSLINK, POINTER(c_double)]
    WSGetDouble.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5680
if _libs["WSTP64i4"].has("WSGetReal", "cdecl"):
    WSGetReal = _libs["WSTP64i4"].get("WSGetReal", "cdecl")
    WSGetReal.argtypes = [WSLINK, POINTER(c_double)]
    WSGetReal.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5681
if _libs["WSTP64i4"].has("WSGetLongDouble", "cdecl"):
    WSGetLongDouble = _libs["WSTP64i4"].get("WSGetLongDouble", "cdecl")
    WSGetLongDouble.argtypes = [WSLINK, POINTER(wsextended_double)]
    WSGetLongDouble.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5684
if _libs["WSTP64i4"].has("WSGetReal32", "cdecl"):
    WSGetReal32 = _libs["WSTP64i4"].get("WSGetReal32", "cdecl")
    WSGetReal32.argtypes = [WSLINK, POINTER(c_float)]
    WSGetReal32.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5685
if _libs["WSTP64i4"].has("WSGetReal64", "cdecl"):
    WSGetReal64 = _libs["WSTP64i4"].get("WSGetReal64", "cdecl")
    WSGetReal64.argtypes = [WSLINK, POINTER(c_double)]
    WSGetReal64.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5686
if _libs["WSTP64i4"].has("WSGetReal128", "cdecl"):
    WSGetReal128 = _libs["WSTP64i4"].get("WSGetReal128", "cdecl")
    WSGetReal128.argtypes = [WSLINK, POINTER(wsextended_double)]
    WSGetReal128.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5714
if _libs["WSTP64i4"].has("WSGet8BitCharacters", "cdecl"):
    WSGet8BitCharacters = _libs["WSTP64i4"].get("WSGet8BitCharacters", "cdecl")
    WSGet8BitCharacters.argtypes = [WSLINK, POINTER(c_long), POINTER(c_ubyte), c_long, POINTER(c_long), c_long]
    WSGet8BitCharacters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5715
if _libs["WSTP64i4"].has("WSGet7BitCharacters", "cdecl"):
    WSGet7BitCharacters = _libs["WSTP64i4"].get("WSGet7BitCharacters", "cdecl")
    WSGet7BitCharacters.argtypes = [WSLINK, POINTER(c_long), String, c_long, POINTER(c_long)]
    WSGet7BitCharacters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5717
if _libs["WSTP64i4"].has("WSGetUCS2Characters", "cdecl"):
    WSGetUCS2Characters = _libs["WSTP64i4"].get("WSGetUCS2Characters", "cdecl")
    WSGetUCS2Characters.argtypes = [WSLINK, POINTER(c_int), POINTER(c_ushort), c_int, POINTER(c_int)]
    WSGetUCS2Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5718
if _libs["WSTP64i4"].has("WSGetUTF8Characters", "cdecl"):
    WSGetUTF8Characters = _libs["WSTP64i4"].get("WSGetUTF8Characters", "cdecl")
    WSGetUTF8Characters.argtypes = [WSLINK, POINTER(c_int), POINTER(c_ubyte), c_int, POINTER(c_int)]
    WSGetUTF8Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5719
if _libs["WSTP64i4"].has("WSGetUTF16Characters", "cdecl"):
    WSGetUTF16Characters = _libs["WSTP64i4"].get("WSGetUTF16Characters", "cdecl")
    WSGetUTF16Characters.argtypes = [WSLINK, POINTER(c_int), POINTER(c_ushort), c_int, POINTER(c_int)]
    WSGetUTF16Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5720
if _libs["WSTP64i4"].has("WSGetUTF32Characters", "cdecl"):
    WSGetUTF32Characters = _libs["WSTP64i4"].get("WSGetUTF32Characters", "cdecl")
    WSGetUTF32Characters.argtypes = [WSLINK, POINTER(c_int), POINTER(c_uint), c_int, POINTER(c_int)]
    WSGetUTF32Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5729
if _libs["WSTP64i4"].has("WSGetByteString", "cdecl"):
    WSGetByteString = _libs["WSTP64i4"].get("WSGetByteString", "cdecl")
    WSGetByteString.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), c_long]
    WSGetByteString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5730
if _libs["WSTP64i4"].has("WSGetString", "cdecl"):
    WSGetString = _libs["WSTP64i4"].get("WSGetString", "cdecl")
    WSGetString.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSGetString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5732
if _libs["WSTP64i4"].has("WSGetUCS2String", "cdecl"):
    WSGetUCS2String = _libs["WSTP64i4"].get("WSGetUCS2String", "cdecl")
    WSGetUCS2String.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int)]
    WSGetUCS2String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5733
if _libs["WSTP64i4"].has("WSGetUTF8String", "cdecl"):
    WSGetUTF8String = _libs["WSTP64i4"].get("WSGetUTF8String", "cdecl")
    WSGetUTF8String.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF8String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5734
if _libs["WSTP64i4"].has("WSGetUTF16String", "cdecl"):
    WSGetUTF16String = _libs["WSTP64i4"].get("WSGetUTF16String", "cdecl")
    WSGetUTF16String.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF16String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5735
if _libs["WSTP64i4"].has("WSGetUTF32String", "cdecl"):
    WSGetUTF32String = _libs["WSTP64i4"].get("WSGetUTF32String", "cdecl")
    WSGetUTF32String.argtypes = [WSLINK, POINTER(POINTER(c_uint)), POINTER(c_int)]
    WSGetUTF32String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5749
if _libs["WSTP64i4"].has("WSGetNumberAsByteString", "cdecl"):
    WSGetNumberAsByteString = _libs["WSTP64i4"].get("WSGetNumberAsByteString", "cdecl")
    WSGetNumberAsByteString.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_long), c_long]
    WSGetNumberAsByteString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5750
if _libs["WSTP64i4"].has("WSGetNumberAsString", "cdecl"):
    WSGetNumberAsString = _libs["WSTP64i4"].get("WSGetNumberAsString", "cdecl")
    WSGetNumberAsString.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSGetNumberAsString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5752
if _libs["WSTP64i4"].has("WSGetNumberAsUCS2String", "cdecl"):
    WSGetNumberAsUCS2String = _libs["WSTP64i4"].get("WSGetNumberAsUCS2String", "cdecl")
    WSGetNumberAsUCS2String.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int)]
    WSGetNumberAsUCS2String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5753
if _libs["WSTP64i4"].has("WSGetNumberAsUTF8String", "cdecl"):
    WSGetNumberAsUTF8String = _libs["WSTP64i4"].get("WSGetNumberAsUTF8String", "cdecl")
    WSGetNumberAsUTF8String.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), POINTER(c_int)]
    WSGetNumberAsUTF8String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5754
if _libs["WSTP64i4"].has("WSGetNumberAsUTF16String", "cdecl"):
    WSGetNumberAsUTF16String = _libs["WSTP64i4"].get("WSGetNumberAsUTF16String", "cdecl")
    WSGetNumberAsUTF16String.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int), POINTER(c_int)]
    WSGetNumberAsUTF16String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5755
if _libs["WSTP64i4"].has("WSGetNumberAsUTF32String", "cdecl"):
    WSGetNumberAsUTF32String = _libs["WSTP64i4"].get("WSGetNumberAsUTF32String", "cdecl")
    WSGetNumberAsUTF32String.argtypes = [WSLINK, POINTER(POINTER(c_uint)), POINTER(c_int)]
    WSGetNumberAsUTF32String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5767
if _libs["WSTP64i4"].has("WSReleaseUCS2String", "cdecl"):
    WSReleaseUCS2String = _libs["WSTP64i4"].get("WSReleaseUCS2String", "cdecl")
    WSReleaseUCS2String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUCS2String.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5768
if _libs["WSTP64i4"].has("WSReleaseUTF8String", "cdecl"):
    WSReleaseUTF8String = _libs["WSTP64i4"].get("WSReleaseUTF8String", "cdecl")
    WSReleaseUTF8String.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseUTF8String.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5769
if _libs["WSTP64i4"].has("WSReleaseUTF16String", "cdecl"):
    WSReleaseUTF16String = _libs["WSTP64i4"].get("WSReleaseUTF16String", "cdecl")
    WSReleaseUTF16String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUTF16String.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5770
if _libs["WSTP64i4"].has("WSReleaseUTF32String", "cdecl"):
    WSReleaseUTF32String = _libs["WSTP64i4"].get("WSReleaseUTF32String", "cdecl")
    WSReleaseUTF32String.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSReleaseUTF32String.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5771
if _libs["WSTP64i4"].has("WSReleaseByteString", "cdecl"):
    WSReleaseByteString = _libs["WSTP64i4"].get("WSReleaseByteString", "cdecl")
    WSReleaseByteString.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseByteString.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5772
if _libs["WSTP64i4"].has("WSReleaseString", "cdecl"):
    WSReleaseString = _libs["WSTP64i4"].get("WSReleaseString", "cdecl")
    WSReleaseString.argtypes = [WSLINK, String]
    WSReleaseString.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5775
if _libs["WSTP64i4"].has("WSTestString", "cdecl"):
    WSTestString = _libs["WSTP64i4"].get("WSTestString", "cdecl")
    WSTestString.argtypes = [WSLINK, String]
    WSTestString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5776
for _lib in _libs.values():
    if not _lib.has("WSTestUCS2String", "cdecl"):
        continue
    WSTestUCS2String = _lib.get("WSTestUCS2String", "cdecl")
    WSTestUCS2String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSTestUCS2String.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5777
for _lib in _libs.values():
    if not _lib.has("WSTestUTF8String", "cdecl"):
        continue
    WSTestUTF8String = _lib.get("WSTestUTF8String", "cdecl")
    WSTestUTF8String.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSTestUTF8String.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5778
for _lib in _libs.values():
    if not _lib.has("WSTestUTF16String", "cdecl"):
        continue
    WSTestUTF16String = _lib.get("WSTestUTF16String", "cdecl")
    WSTestUTF16String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSTestUTF16String.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5779
for _lib in _libs.values():
    if not _lib.has("WSTestUTF32String", "cdecl"):
        continue
    WSTestUTF32String = _lib.get("WSTestUTF32String", "cdecl")
    WSTestUTF32String.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSTestUTF32String.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5807
if _libs["WSTP64i4"].has("WSGetByteSymbol", "cdecl"):
    WSGetByteSymbol = _libs["WSTP64i4"].get("WSGetByteSymbol", "cdecl")
    WSGetByteSymbol.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), c_long]
    WSGetByteSymbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5808
if _libs["WSTP64i4"].has("WSGetSymbol", "cdecl"):
    WSGetSymbol = _libs["WSTP64i4"].get("WSGetSymbol", "cdecl")
    WSGetSymbol.argtypes = [WSLINK, POINTER(POINTER(c_char))]
    WSGetSymbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5810
if _libs["WSTP64i4"].has("WSGetUCS2Symbol", "cdecl"):
    WSGetUCS2Symbol = _libs["WSTP64i4"].get("WSGetUCS2Symbol", "cdecl")
    WSGetUCS2Symbol.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int)]
    WSGetUCS2Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5811
if _libs["WSTP64i4"].has("WSGetUTF8Symbol", "cdecl"):
    WSGetUTF8Symbol = _libs["WSTP64i4"].get("WSGetUTF8Symbol", "cdecl")
    WSGetUTF8Symbol.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF8Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5812
if _libs["WSTP64i4"].has("WSGetUTF16Symbol", "cdecl"):
    WSGetUTF16Symbol = _libs["WSTP64i4"].get("WSGetUTF16Symbol", "cdecl")
    WSGetUTF16Symbol.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF16Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5813
if _libs["WSTP64i4"].has("WSGetUTF32Symbol", "cdecl"):
    WSGetUTF32Symbol = _libs["WSTP64i4"].get("WSGetUTF32Symbol", "cdecl")
    WSGetUTF32Symbol.argtypes = [WSLINK, POINTER(POINTER(c_uint)), POINTER(c_int)]
    WSGetUTF32Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5823
if _libs["WSTP64i4"].has("WSReleaseUCS2Symbol", "cdecl"):
    WSReleaseUCS2Symbol = _libs["WSTP64i4"].get("WSReleaseUCS2Symbol", "cdecl")
    WSReleaseUCS2Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUCS2Symbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5824
if _libs["WSTP64i4"].has("WSReleaseUTF8Symbol", "cdecl"):
    WSReleaseUTF8Symbol = _libs["WSTP64i4"].get("WSReleaseUTF8Symbol", "cdecl")
    WSReleaseUTF8Symbol.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseUTF8Symbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5825
if _libs["WSTP64i4"].has("WSReleaseUTF16Symbol", "cdecl"):
    WSReleaseUTF16Symbol = _libs["WSTP64i4"].get("WSReleaseUTF16Symbol", "cdecl")
    WSReleaseUTF16Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSReleaseUTF16Symbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5826
if _libs["WSTP64i4"].has("WSReleaseUTF32Symbol", "cdecl"):
    WSReleaseUTF32Symbol = _libs["WSTP64i4"].get("WSReleaseUTF32Symbol", "cdecl")
    WSReleaseUTF32Symbol.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSReleaseUTF32Symbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5827
if _libs["WSTP64i4"].has("WSReleaseByteSymbol", "cdecl"):
    WSReleaseByteSymbol = _libs["WSTP64i4"].get("WSReleaseByteSymbol", "cdecl")
    WSReleaseByteSymbol.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseByteSymbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5828
if _libs["WSTP64i4"].has("WSReleaseSymbol", "cdecl"):
    WSReleaseSymbol = _libs["WSTP64i4"].get("WSReleaseSymbol", "cdecl")
    WSReleaseSymbol.argtypes = [WSLINK, String]
    WSReleaseSymbol.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5830
if _libs["WSTP64i4"].has("WSTestSymbol", "cdecl"):
    WSTestSymbol = _libs["WSTP64i4"].get("WSTestSymbol", "cdecl")
    WSTestSymbol.argtypes = [WSLINK, String]
    WSTestSymbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5831
if _libs["WSTP64i4"].has("WSTestUCS2Symbol", "cdecl"):
    WSTestUCS2Symbol = _libs["WSTP64i4"].get("WSTestUCS2Symbol", "cdecl")
    WSTestUCS2Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSTestUCS2Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5832
if _libs["WSTP64i4"].has("WSTestUTF8Symbol", "cdecl"):
    WSTestUTF8Symbol = _libs["WSTP64i4"].get("WSTestUTF8Symbol", "cdecl")
    WSTestUTF8Symbol.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSTestUTF8Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5833
if _libs["WSTP64i4"].has("WSTestUTF16Symbol", "cdecl"):
    WSTestUTF16Symbol = _libs["WSTP64i4"].get("WSTestUTF16Symbol", "cdecl")
    WSTestUTF16Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSTestUTF16Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5834
if _libs["WSTP64i4"].has("WSTestUTF32Symbol", "cdecl"):
    WSTestUTF32Symbol = _libs["WSTP64i4"].get("WSTestUTF32Symbol", "cdecl")
    WSTestUTF32Symbol.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSTestUTF32Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5836
if _libs["WSTP64i4"].has("WSGetFunction", "cdecl"):
    WSGetFunction = _libs["WSTP64i4"].get("WSGetFunction", "cdecl")
    WSGetFunction.argtypes = [WSLINK, POINTER(POINTER(c_char)), POINTER(c_int)]
    WSGetFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5838
if _libs["WSTP64i4"].has("WSGetUCS2Function", "cdecl"):
    WSGetUCS2Function = _libs["WSTP64i4"].get("WSGetUCS2Function", "cdecl")
    WSGetUCS2Function.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int), POINTER(c_int)]
    WSGetUCS2Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5839
if _libs["WSTP64i4"].has("WSGetUTF8Function", "cdecl"):
    WSGetUTF8Function = _libs["WSTP64i4"].get("WSGetUTF8Function", "cdecl")
    WSGetUTF8Function.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF8Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5840
if _libs["WSTP64i4"].has("WSGetUTF16Function", "cdecl"):
    WSGetUTF16Function = _libs["WSTP64i4"].get("WSGetUTF16Function", "cdecl")
    WSGetUTF16Function.argtypes = [WSLINK, POINTER(POINTER(c_ushort)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF16Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5841
if _libs["WSTP64i4"].has("WSGetUTF32Function", "cdecl"):
    WSGetUTF32Function = _libs["WSTP64i4"].get("WSGetUTF32Function", "cdecl")
    WSGetUTF32Function.argtypes = [WSLINK, POINTER(POINTER(c_uint)), POINTER(c_int), POINTER(c_int)]
    WSGetUTF32Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5851
if _libs["WSTP64i4"].has("WSCheckFunction", "cdecl"):
    WSCheckFunction = _libs["WSTP64i4"].get("WSCheckFunction", "cdecl")
    WSCheckFunction.argtypes = [WSLINK, String, POINTER(c_long)]
    WSCheckFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5852
if _libs["WSTP64i4"].has("WSCheckFunctionWithArgCount", "cdecl"):
    WSCheckFunctionWithArgCount = _libs["WSTP64i4"].get("WSCheckFunctionWithArgCount", "cdecl")
    WSCheckFunctionWithArgCount.argtypes = [WSLINK, String, POINTER(c_long)]
    WSCheckFunctionWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5854
if _libs["WSTP64i4"].has("WSTestHead", "cdecl"):
    WSTestHead = _libs["WSTP64i4"].get("WSTestHead", "cdecl")
    WSTestHead.argtypes = [WSLINK, String, POINTER(c_int)]
    WSTestHead.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5861
if _libs["WSTP64i4"].has("WSTestHeadWithArgCount", "cdecl"):
    WSTestHeadWithArgCount = _libs["WSTP64i4"].get("WSTestHeadWithArgCount", "cdecl")
    WSTestHeadWithArgCount.argtypes = [WSLINK, String, POINTER(c_int)]
    WSTestHeadWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5862
if _libs["WSTP64i4"].has("WSTestUCS2HeadWithArgCount", "cdecl"):
    WSTestUCS2HeadWithArgCount = _libs["WSTP64i4"].get("WSTestUCS2HeadWithArgCount", "cdecl")
    WSTestUCS2HeadWithArgCount.argtypes = [WSLINK, POINTER(c_ushort), c_int, POINTER(c_int)]
    WSTestUCS2HeadWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5863
if _libs["WSTP64i4"].has("WSTestUTF16HeadWithArgCount", "cdecl"):
    WSTestUTF16HeadWithArgCount = _libs["WSTP64i4"].get("WSTestUTF16HeadWithArgCount", "cdecl")
    WSTestUTF16HeadWithArgCount.argtypes = [WSLINK, POINTER(c_ushort), c_int, POINTER(c_int)]
    WSTestUTF16HeadWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5864
if _libs["WSTP64i4"].has("WSTestUTF32HeadWithArgCount", "cdecl"):
    WSTestUTF32HeadWithArgCount = _libs["WSTP64i4"].get("WSTestUTF32HeadWithArgCount", "cdecl")
    WSTestUTF32HeadWithArgCount.argtypes = [WSLINK, POINTER(c_uint), c_int, POINTER(c_int)]
    WSTestUTF32HeadWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5865
if _libs["WSTP64i4"].has("WSTestUTF8HeadWithArgCount", "cdecl"):
    WSTestUTF8HeadWithArgCount = _libs["WSTP64i4"].get("WSTestUTF8HeadWithArgCount", "cdecl")
    WSTestUTF8HeadWithArgCount.argtypes = [WSLINK, POINTER(c_ubyte), c_int, POINTER(c_int)]
    WSTestUTF8HeadWithArgCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5867
if _libs["WSTP64i4"].has("WSTestUCS2Head", "cdecl"):
    WSTestUCS2Head = _libs["WSTP64i4"].get("WSTestUCS2Head", "cdecl")
    WSTestUCS2Head.argtypes = [WSLINK, POINTER(c_ushort), c_int, POINTER(c_int)]
    WSTestUCS2Head.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5868
if _libs["WSTP64i4"].has("WSTestUTF8Head", "cdecl"):
    WSTestUTF8Head = _libs["WSTP64i4"].get("WSTestUTF8Head", "cdecl")
    WSTestUTF8Head.argtypes = [WSLINK, POINTER(c_ubyte), c_int, POINTER(c_int)]
    WSTestUTF8Head.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5869
if _libs["WSTP64i4"].has("WSTestUTF16Head", "cdecl"):
    WSTestUTF16Head = _libs["WSTP64i4"].get("WSTestUTF16Head", "cdecl")
    WSTestUTF16Head.argtypes = [WSLINK, POINTER(c_ushort), c_int, POINTER(c_int)]
    WSTestUTF16Head.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5870
if _libs["WSTP64i4"].has("WSTestUTF32Head", "cdecl"):
    WSTestUTF32Head = _libs["WSTP64i4"].get("WSTestUTF32Head", "cdecl")
    WSTestUTF32Head.argtypes = [WSLINK, POINTER(c_uint), c_int, POINTER(c_int)]
    WSTestUTF32Head.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5894
if _libs["WSTP64i4"].has("WSPutBinaryNumber", "cdecl"):
    WSPutBinaryNumber = _libs["WSTP64i4"].get("WSPutBinaryNumber", "cdecl")
    WSPutBinaryNumber.argtypes = [WSLINK, POINTER(None), c_long]
    WSPutBinaryNumber.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5903
if _libs["WSTP64i4"].has("WSPutShortInteger", "cdecl"):
    WSPutShortInteger = _libs["WSTP64i4"].get("WSPutShortInteger", "cdecl")
    WSPutShortInteger.argtypes = [WSLINK, c_int]
    WSPutShortInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5904
if _libs["WSTP64i4"].has("WSPutInteger", "cdecl"):
    WSPutInteger = _libs["WSTP64i4"].get("WSPutInteger", "cdecl")
    WSPutInteger.argtypes = [WSLINK, c_int]
    WSPutInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5905
if _libs["WSTP64i4"].has("WSPutLongInteger", "cdecl"):
    WSPutLongInteger = _libs["WSTP64i4"].get("WSPutLongInteger", "cdecl")
    WSPutLongInteger.argtypes = [WSLINK, c_long]
    WSPutLongInteger.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5907
if _libs["WSTP64i4"].has("WSPutInteger16", "cdecl"):
    WSPutInteger16 = _libs["WSTP64i4"].get("WSPutInteger16", "cdecl")
    WSPutInteger16.argtypes = [WSLINK, c_int]
    WSPutInteger16.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5908
if _libs["WSTP64i4"].has("WSPutInteger32", "cdecl"):
    WSPutInteger32 = _libs["WSTP64i4"].get("WSPutInteger32", "cdecl")
    WSPutInteger32.argtypes = [WSLINK, c_int]
    WSPutInteger32.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5909
if _libs["WSTP64i4"].has("WSPutInteger64", "cdecl"):
    WSPutInteger64 = _libs["WSTP64i4"].get("WSPutInteger64", "cdecl")
    WSPutInteger64.argtypes = [WSLINK, wsint64]
    WSPutInteger64.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5911
if _libs["WSTP64i4"].has("WSPutInteger8", "cdecl"):
    WSPutInteger8 = _libs["WSTP64i4"].get("WSPutInteger8", "cdecl")
    WSPutInteger8.argtypes = [WSLINK, c_ubyte]
    WSPutInteger8.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5921
if _libs["WSTP64i4"].has("WSPutFloat", "cdecl"):
    WSPutFloat = _libs["WSTP64i4"].get("WSPutFloat", "cdecl")
    WSPutFloat.argtypes = [WSLINK, c_double]
    WSPutFloat.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5922
if _libs["WSTP64i4"].has("WSPutDouble", "cdecl"):
    WSPutDouble = _libs["WSTP64i4"].get("WSPutDouble", "cdecl")
    WSPutDouble.argtypes = [WSLINK, c_double]
    WSPutDouble.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5923
if _libs["WSTP64i4"].has("WSPutReal", "cdecl"):
    WSPutReal = _libs["WSTP64i4"].get("WSPutReal", "cdecl")
    WSPutReal.argtypes = [WSLINK, c_double]
    WSPutReal.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5924
if _libs["WSTP64i4"].has("WSPutLongDouble", "cdecl"):
    WSPutLongDouble = _libs["WSTP64i4"].get("WSPutLongDouble", "cdecl")
    WSPutLongDouble.argtypes = [WSLINK, wsextended_double]
    WSPutLongDouble.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5926
if _libs["WSTP64i4"].has("WSPutReal32", "cdecl"):
    WSPutReal32 = _libs["WSTP64i4"].get("WSPutReal32", "cdecl")
    WSPutReal32.argtypes = [WSLINK, c_double]
    WSPutReal32.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5927
if _libs["WSTP64i4"].has("WSPutReal64", "cdecl"):
    WSPutReal64 = _libs["WSTP64i4"].get("WSPutReal64", "cdecl")
    WSPutReal64.argtypes = [WSLINK, c_double]
    WSPutReal64.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5928
if _libs["WSTP64i4"].has("WSPutReal128", "cdecl"):
    WSPutReal128 = _libs["WSTP64i4"].get("WSPutReal128", "cdecl")
    WSPutReal128.argtypes = [WSLINK, wsextended_double]
    WSPutReal128.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5957
if _libs["WSTP64i4"].has("WSPut8BitCharacters", "cdecl"):
    WSPut8BitCharacters = _libs["WSTP64i4"].get("WSPut8BitCharacters", "cdecl")
    WSPut8BitCharacters.argtypes = [WSLINK, c_long, POINTER(c_ubyte), c_long]
    WSPut8BitCharacters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5958
if _libs["WSTP64i4"].has("WSPut7BitCount", "cdecl"):
    WSPut7BitCount = _libs["WSTP64i4"].get("WSPut7BitCount", "cdecl")
    WSPut7BitCount.argtypes = [WSLINK, c_long, c_long]
    WSPut7BitCount.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5959
if _libs["WSTP64i4"].has("WSPut7BitCharacters", "cdecl"):
    WSPut7BitCharacters = _libs["WSTP64i4"].get("WSPut7BitCharacters", "cdecl")
    WSPut7BitCharacters.argtypes = [WSLINK, c_long, String, c_long, c_long]
    WSPut7BitCharacters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5961
if _libs["WSTP64i4"].has("WSPutUCS2Characters", "cdecl"):
    WSPutUCS2Characters = _libs["WSTP64i4"].get("WSPutUCS2Characters", "cdecl")
    WSPutUCS2Characters.argtypes = [WSLINK, c_int, POINTER(c_ushort), c_int]
    WSPutUCS2Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5962
if _libs["WSTP64i4"].has("WSPutUTF8Characters", "cdecl"):
    WSPutUTF8Characters = _libs["WSTP64i4"].get("WSPutUTF8Characters", "cdecl")
    WSPutUTF8Characters.argtypes = [WSLINK, c_int, POINTER(c_ubyte), c_int]
    WSPutUTF8Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5963
if _libs["WSTP64i4"].has("WSPutUTF16Characters", "cdecl"):
    WSPutUTF16Characters = _libs["WSTP64i4"].get("WSPutUTF16Characters", "cdecl")
    WSPutUTF16Characters.argtypes = [WSLINK, c_int, POINTER(c_ushort), c_int]
    WSPutUTF16Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5964
if _libs["WSTP64i4"].has("WSPutUTF32Characters", "cdecl"):
    WSPutUTF32Characters = _libs["WSTP64i4"].get("WSPutUTF32Characters", "cdecl")
    WSPutUTF32Characters.argtypes = [WSLINK, c_int, POINTER(c_uint), c_int]
    WSPutUTF32Characters.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5973
if _libs["WSTP64i4"].has("WSPutByteString", "cdecl"):
    WSPutByteString = _libs["WSTP64i4"].get("WSPutByteString", "cdecl")
    WSPutByteString.argtypes = [WSLINK, POINTER(c_ubyte), c_long]
    WSPutByteString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5974
if _libs["WSTP64i4"].has("WSPutString", "cdecl"):
    WSPutString = _libs["WSTP64i4"].get("WSPutString", "cdecl")
    WSPutString.argtypes = [WSLINK, String]
    WSPutString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5976
if _libs["WSTP64i4"].has("WSPutUCS2String", "cdecl"):
    WSPutUCS2String = _libs["WSTP64i4"].get("WSPutUCS2String", "cdecl")
    WSPutUCS2String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSPutUCS2String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5977
if _libs["WSTP64i4"].has("WSPutUTF8String", "cdecl"):
    WSPutUTF8String = _libs["WSTP64i4"].get("WSPutUTF8String", "cdecl")
    WSPutUTF8String.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSPutUTF8String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5978
if _libs["WSTP64i4"].has("WSPutUTF16String", "cdecl"):
    WSPutUTF16String = _libs["WSTP64i4"].get("WSPutUTF16String", "cdecl")
    WSPutUTF16String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSPutUTF16String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5979
if _libs["WSTP64i4"].has("WSPutUTF32String", "cdecl"):
    WSPutUTF32String = _libs["WSTP64i4"].get("WSPutUTF32String", "cdecl")
    WSPutUTF32String.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSPutUTF32String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5992
if _libs["WSTP64i4"].has("WSPutRealNumberAsString", "cdecl"):
    WSPutRealNumberAsString = _libs["WSTP64i4"].get("WSPutRealNumberAsString", "cdecl")
    WSPutRealNumberAsString.argtypes = [WSLINK, String]
    WSPutRealNumberAsString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5993
if _libs["WSTP64i4"].has("WSPutRealNumberAsByteString", "cdecl"):
    WSPutRealNumberAsByteString = _libs["WSTP64i4"].get("WSPutRealNumberAsByteString", "cdecl")
    WSPutRealNumberAsByteString.argtypes = [WSLINK, POINTER(c_ubyte)]
    WSPutRealNumberAsByteString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5994
if _libs["WSTP64i4"].has("WSPutRealNumberAsUCS2String", "cdecl"):
    WSPutRealNumberAsUCS2String = _libs["WSTP64i4"].get("WSPutRealNumberAsUCS2String", "cdecl")
    WSPutRealNumberAsUCS2String.argtypes = [WSLINK, POINTER(c_ushort)]
    WSPutRealNumberAsUCS2String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5995
if _libs["WSTP64i4"].has("WSPutRealNumberAsUTF8String", "cdecl"):
    WSPutRealNumberAsUTF8String = _libs["WSTP64i4"].get("WSPutRealNumberAsUTF8String", "cdecl")
    WSPutRealNumberAsUTF8String.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSPutRealNumberAsUTF8String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5996
if _libs["WSTP64i4"].has("WSPutRealNumberAsUTF16String", "cdecl"):
    WSPutRealNumberAsUTF16String = _libs["WSTP64i4"].get("WSPutRealNumberAsUTF16String", "cdecl")
    WSPutRealNumberAsUTF16String.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSPutRealNumberAsUTF16String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 5997
if _libs["WSTP64i4"].has("WSPutRealNumberAsUTF32String", "cdecl"):
    WSPutRealNumberAsUTF32String = _libs["WSTP64i4"].get("WSPutRealNumberAsUTF32String", "cdecl")
    WSPutRealNumberAsUTF32String.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSPutRealNumberAsUTF32String.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6000
if _libs["WSTP64i4"].has("WSPutSize", "cdecl"):
    WSPutSize = _libs["WSTP64i4"].get("WSPutSize", "cdecl")
    WSPutSize.argtypes = [WSLINK, c_int]
    WSPutSize.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6001
if _libs["WSTP64i4"].has("WSPutData", "cdecl"):
    WSPutData = _libs["WSTP64i4"].get("WSPutData", "cdecl")
    WSPutData.argtypes = [WSLINK, String, c_int]
    WSPutData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6031
if _libs["WSTP64i4"].has("WSPutByteSymbol", "cdecl"):
    WSPutByteSymbol = _libs["WSTP64i4"].get("WSPutByteSymbol", "cdecl")
    WSPutByteSymbol.argtypes = [WSLINK, POINTER(c_ubyte), c_long]
    WSPutByteSymbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6032
if _libs["WSTP64i4"].has("WSPutSymbol", "cdecl"):
    WSPutSymbol = _libs["WSTP64i4"].get("WSPutSymbol", "cdecl")
    WSPutSymbol.argtypes = [WSLINK, String]
    WSPutSymbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6034
if _libs["WSTP64i4"].has("WSPutUCS2Symbol", "cdecl"):
    WSPutUCS2Symbol = _libs["WSTP64i4"].get("WSPutUCS2Symbol", "cdecl")
    WSPutUCS2Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSPutUCS2Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6036
if _libs["WSTP64i4"].has("WSPutUTF8Symbol", "cdecl"):
    WSPutUTF8Symbol = _libs["WSTP64i4"].get("WSPutUTF8Symbol", "cdecl")
    WSPutUTF8Symbol.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSPutUTF8Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6037
if _libs["WSTP64i4"].has("WSPutUTF16Symbol", "cdecl"):
    WSPutUTF16Symbol = _libs["WSTP64i4"].get("WSPutUTF16Symbol", "cdecl")
    WSPutUTF16Symbol.argtypes = [WSLINK, POINTER(c_ushort), c_int]
    WSPutUTF16Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6038
if _libs["WSTP64i4"].has("WSPutUTF32Symbol", "cdecl"):
    WSPutUTF32Symbol = _libs["WSTP64i4"].get("WSPutUTF32Symbol", "cdecl")
    WSPutUTF32Symbol.argtypes = [WSLINK, POINTER(c_uint), c_int]
    WSPutUTF32Symbol.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6041
if _libs["WSTP64i4"].has("WSPutFunction", "cdecl"):
    WSPutFunction = _libs["WSTP64i4"].get("WSPutFunction", "cdecl")
    WSPutFunction.argtypes = [WSLINK, String, c_int]
    WSPutFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6043
if _libs["WSTP64i4"].has("WSPutUCS2Function", "cdecl"):
    WSPutUCS2Function = _libs["WSTP64i4"].get("WSPutUCS2Function", "cdecl")
    WSPutUCS2Function.argtypes = [WSLINK, POINTER(c_ushort), c_int, c_int]
    WSPutUCS2Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6044
if _libs["WSTP64i4"].has("WSPutUTF8Function", "cdecl"):
    WSPutUTF8Function = _libs["WSTP64i4"].get("WSPutUTF8Function", "cdecl")
    WSPutUTF8Function.argtypes = [WSLINK, POINTER(c_ubyte), c_int, c_int]
    WSPutUTF8Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6045
if _libs["WSTP64i4"].has("WSPutUTF16Function", "cdecl"):
    WSPutUTF16Function = _libs["WSTP64i4"].get("WSPutUTF16Function", "cdecl")
    WSPutUTF16Function.argtypes = [WSLINK, POINTER(c_ushort), c_int, c_int]
    WSPutUTF16Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6046
if _libs["WSTP64i4"].has("WSPutUTF32Function", "cdecl"):
    WSPutUTF32Function = _libs["WSTP64i4"].get("WSPutUTF32Function", "cdecl")
    WSPutUTF32Function.argtypes = [WSLINK, POINTER(c_uint), c_int, c_int]
    WSPutUTF32Function.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6083
class struct_anon_4(Structure):
    pass

struct_anon_4.__slots__ = [
    'str',
    'end',
]
struct_anon_4._fields_ = [
    ('str', String),
    ('end', String),
]

WSStringPosition = struct_anon_4# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6083

WSStringPositionPointer = POINTER(WSStringPosition)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6085

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6111
class struct_anon_5(Structure):
    pass

struct_anon_5.__slots__ = [
    'cc',
    'mode',
    'more',
    'head',
]
struct_anon_5._fields_ = [
    ('cc', POINTER(c_ubyte)),
    ('mode', c_int),
    ('more', c_int),
    ('head', POINTER(c_ubyte)),
]

WSOldStringPosition = struct_anon_5# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6111

WSOldStringPositionPointer = POINTER(WSOldStringPosition)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6113

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6141
if _libs["WSTP64i4"].has("WSCharacterOffset", "cdecl"):
    WSCharacterOffset = _libs["WSTP64i4"].get("WSCharacterOffset", "cdecl")
    WSCharacterOffset.argtypes = [POINTER(POINTER(c_char)), String, c_long]
    WSCharacterOffset.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6142
if _libs["WSTP64i4"].has("WSStringCharacter", "cdecl"):
    WSStringCharacter = _libs["WSTP64i4"].get("WSStringCharacter", "cdecl")
    WSStringCharacter.argtypes = [String, String]
    WSStringCharacter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6143
if _libs["WSTP64i4"].has("WSNextCharacter", "cdecl"):
    WSNextCharacter = _libs["WSTP64i4"].get("WSNextCharacter", "cdecl")
    WSNextCharacter.argtypes = [POINTER(POINTER(c_char)), String]
    WSNextCharacter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6156
if _libs["WSTP64i4"].has("WSNextCharacterFromStringWithLength", "cdecl"):
    WSNextCharacterFromStringWithLength = _libs["WSTP64i4"].get("WSNextCharacterFromStringWithLength", "cdecl")
    WSNextCharacterFromStringWithLength.argtypes = [String, POINTER(c_long), c_long]
    WSNextCharacterFromStringWithLength.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6167
if _libs["WSTP64i4"].has("WSConvertNewLine", "cdecl"):
    WSConvertNewLine = _libs["WSTP64i4"].get("WSConvertNewLine", "cdecl")
    WSConvertNewLine.argtypes = [POINTER(POINTER(c_char))]
    WSConvertNewLine.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6168
if _libs["WSTP64i4"].has("WSConvertCharacter", "cdecl"):
    WSConvertCharacter = _libs["WSTP64i4"].get("WSConvertCharacter", "cdecl")
    WSConvertCharacter.argtypes = [c_ulong, POINTER(POINTER(c_char))]
    WSConvertCharacter.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6169
if _libs["WSTP64i4"].has("WSConvertByteString", "cdecl"):
    WSConvertByteString = _libs["WSTP64i4"].get("WSConvertByteString", "cdecl")
    WSConvertByteString.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String]
    WSConvertByteString.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6170
if _libs["WSTP64i4"].has("WSConvertByteStringNL", "cdecl"):
    WSConvertByteStringNL = _libs["WSTP64i4"].get("WSConvertByteStringNL", "cdecl")
    WSConvertByteStringNL.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertByteStringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6175
if _libs["WSTP64i4"].has("WSConvertDoubleByteString", "cdecl"):
    WSConvertDoubleByteString = _libs["WSTP64i4"].get("WSConvertDoubleByteString", "cdecl")
    WSConvertDoubleByteString.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String]
    WSConvertDoubleByteString.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6176
if _libs["WSTP64i4"].has("WSConvertDoubleByteStringNL", "cdecl"):
    WSConvertDoubleByteStringNL = _libs["WSTP64i4"].get("WSConvertDoubleByteStringNL", "cdecl")
    WSConvertDoubleByteStringNL.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertDoubleByteStringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6178
if _libs["WSTP64i4"].has("WSConvertUCS2String", "cdecl"):
    WSConvertUCS2String = _libs["WSTP64i4"].get("WSConvertUCS2String", "cdecl")
    WSConvertUCS2String.argtypes = [POINTER(c_ushort), c_long, POINTER(POINTER(c_char)), String]
    WSConvertUCS2String.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6179
if _libs["WSTP64i4"].has("WSConvertUCS2StringNL", "cdecl"):
    WSConvertUCS2StringNL = _libs["WSTP64i4"].get("WSConvertUCS2StringNL", "cdecl")
    WSConvertUCS2StringNL.argtypes = [POINTER(c_ushort), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertUCS2StringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6180
if _libs["WSTP64i4"].has("WSConvertUTF8String", "cdecl"):
    WSConvertUTF8String = _libs["WSTP64i4"].get("WSConvertUTF8String", "cdecl")
    WSConvertUTF8String.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String]
    WSConvertUTF8String.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6181
if _libs["WSTP64i4"].has("WSConvertUTF8StringNL", "cdecl"):
    WSConvertUTF8StringNL = _libs["WSTP64i4"].get("WSConvertUTF8StringNL", "cdecl")
    WSConvertUTF8StringNL.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertUTF8StringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6182
if _libs["WSTP64i4"].has("WSConvertUTF16String", "cdecl"):
    WSConvertUTF16String = _libs["WSTP64i4"].get("WSConvertUTF16String", "cdecl")
    WSConvertUTF16String.argtypes = [POINTER(c_ushort), c_long, POINTER(POINTER(c_char)), String]
    WSConvertUTF16String.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6183
if _libs["WSTP64i4"].has("WSConvertUTF16StringNL", "cdecl"):
    WSConvertUTF16StringNL = _libs["WSTP64i4"].get("WSConvertUTF16StringNL", "cdecl")
    WSConvertUTF16StringNL.argtypes = [POINTER(c_ushort), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertUTF16StringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6184
if _libs["WSTP64i4"].has("WSConvertUTF32String", "cdecl"):
    WSConvertUTF32String = _libs["WSTP64i4"].get("WSConvertUTF32String", "cdecl")
    WSConvertUTF32String.argtypes = [POINTER(c_uint), c_long, POINTER(POINTER(c_char)), String]
    WSConvertUTF32String.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6185
if _libs["WSTP64i4"].has("WSConvertUTF32StringNL", "cdecl"):
    WSConvertUTF32StringNL = _libs["WSTP64i4"].get("WSConvertUTF32StringNL", "cdecl")
    WSConvertUTF32StringNL.argtypes = [POINTER(c_uint), c_long, POINTER(POINTER(c_char)), String, c_ulong]
    WSConvertUTF32StringNL.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6208
if _libs["WSTP64i4"].has("WSStringFirstPosFun", "cdecl"):
    WSStringFirstPosFun = _libs["WSTP64i4"].get("WSStringFirstPosFun", "cdecl")
    WSStringFirstPosFun.argtypes = [String, WSStringPositionPointer]
    WSStringFirstPosFun.restype = c_char_p

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6215
if _libs["WSTP64i4"].has("WSOldPutCharToString", "cdecl"):
    WSOldPutCharToString = _libs["WSTP64i4"].get("WSOldPutCharToString", "cdecl")
    WSOldPutCharToString.argtypes = [c_uint, POINTER(POINTER(c_char))]
    WSOldPutCharToString.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6216
if _libs["WSTP64i4"].has("WSOldStringNextPosFun", "cdecl"):
    WSOldStringNextPosFun = _libs["WSTP64i4"].get("WSOldStringNextPosFun", "cdecl")
    WSOldStringNextPosFun.argtypes = [WSOldStringPositionPointer]
    WSOldStringNextPosFun.restype = POINTER(c_ubyte)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6217
if _libs["WSTP64i4"].has("WSOldStringFirstPosFun", "cdecl"):
    WSOldStringFirstPosFun = _libs["WSTP64i4"].get("WSOldStringFirstPosFun", "cdecl")
    WSOldStringFirstPosFun.argtypes = [String, WSOldStringPositionPointer]
    WSOldStringFirstPosFun.restype = POINTER(c_ubyte)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6218
if _libs["WSTP64i4"].has("WSOldStringCharFun", "cdecl"):
    WSOldStringCharFun = _libs["WSTP64i4"].get("WSOldStringCharFun", "cdecl")
    WSOldStringCharFun.argtypes = [WSOldStringPositionPointer]
    WSOldStringCharFun.restype = c_uint

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6219
if _libs["WSTP64i4"].has("WSOldConvertByteString", "cdecl"):
    WSOldConvertByteString = _libs["WSTP64i4"].get("WSOldConvertByteString", "cdecl")
    WSOldConvertByteString.argtypes = [POINTER(c_ubyte), c_long, POINTER(POINTER(c_char)), String]
    WSOldConvertByteString.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6225
if _libs["WSTP64i4"].has("WSOldConvertUCS2String", "cdecl"):
    WSOldConvertUCS2String = _libs["WSTP64i4"].get("WSOldConvertUCS2String", "cdecl")
    WSOldConvertUCS2String.argtypes = [POINTER(c_ushort), c_long, POINTER(POINTER(c_char)), String]
    WSOldConvertUCS2String.restype = c_long

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6236
for _lib in _libs.values():
    if not _lib.has("WSCharOffset", "cdecl"):
        continue
    WSCharOffset = _lib.get("WSCharOffset", "cdecl")
    WSCharOffset.argtypes = [POINTER(POINTER(c_char)), String, c_long, c_int]
    WSCharOffset.restype = c_long
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6237
for _lib in _libs.values():
    if not _lib.has("WSNextChar", "cdecl"):
        continue
    WSNextChar = _lib.get("WSNextChar", "cdecl")
    WSNextChar.argtypes = [POINTER(POINTER(c_char)), String, c_int, c_int, POINTER(c_int)]
    WSNextChar.restype = c_long
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6257
class struct_array_meter(Structure):
    pass

array_meterp = POINTER(struct_array_meter)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6257

array_meterpp = POINTER(array_meterp)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6258

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6268
if _libs["WSTP64i4"].has("WSPutArray", "cdecl"):
    WSPutArray = _libs["WSTP64i4"].get("WSPutArray", "cdecl")
    WSPutArray.argtypes = [WSLINK, array_meterp]
    WSPutArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6270
if _libs["WSTP64i4"].has("WSPutBinaryNumberArrayData", "cdecl"):
    WSPutBinaryNumberArrayData = _libs["WSTP64i4"].get("WSPutBinaryNumberArrayData", "cdecl")
    WSPutBinaryNumberArrayData.argtypes = [WSLINK, array_meterp, POINTER(None), c_long, c_long]
    WSPutBinaryNumberArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6271
if _libs["WSTP64i4"].has("WSPutByteArrayData", "cdecl"):
    WSPutByteArrayData = _libs["WSTP64i4"].get("WSPutByteArrayData", "cdecl")
    WSPutByteArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_ubyte), c_long]
    WSPutByteArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6272
if _libs["WSTP64i4"].has("WSPutShortIntegerArrayData", "cdecl"):
    WSPutShortIntegerArrayData = _libs["WSTP64i4"].get("WSPutShortIntegerArrayData", "cdecl")
    WSPutShortIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_short), c_long]
    WSPutShortIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6273
if _libs["WSTP64i4"].has("WSPutIntegerArrayData", "cdecl"):
    WSPutIntegerArrayData = _libs["WSTP64i4"].get("WSPutIntegerArrayData", "cdecl")
    WSPutIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_int), c_long]
    WSPutIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6274
if _libs["WSTP64i4"].has("WSPutLongIntegerArrayData", "cdecl"):
    WSPutLongIntegerArrayData = _libs["WSTP64i4"].get("WSPutLongIntegerArrayData", "cdecl")
    WSPutLongIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_long), c_long]
    WSPutLongIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6276
if _libs["WSTP64i4"].has("WSPutInteger8ArrayData", "cdecl"):
    WSPutInteger8ArrayData = _libs["WSTP64i4"].get("WSPutInteger8ArrayData", "cdecl")
    WSPutInteger8ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_ubyte), c_int]
    WSPutInteger8ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6277
if _libs["WSTP64i4"].has("WSPutInteger16ArrayData", "cdecl"):
    WSPutInteger16ArrayData = _libs["WSTP64i4"].get("WSPutInteger16ArrayData", "cdecl")
    WSPutInteger16ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_short), c_int]
    WSPutInteger16ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6278
if _libs["WSTP64i4"].has("WSPutInteger32ArrayData", "cdecl"):
    WSPutInteger32ArrayData = _libs["WSTP64i4"].get("WSPutInteger32ArrayData", "cdecl")
    WSPutInteger32ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_int), c_int]
    WSPutInteger32ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6279
if _libs["WSTP64i4"].has("WSPutInteger64ArrayData", "cdecl"):
    WSPutInteger64ArrayData = _libs["WSTP64i4"].get("WSPutInteger64ArrayData", "cdecl")
    WSPutInteger64ArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsint64), c_int]
    WSPutInteger64ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6284
if _libs["WSTP64i4"].has("WSPutFloatArrayData", "cdecl"):
    WSPutFloatArrayData = _libs["WSTP64i4"].get("WSPutFloatArrayData", "cdecl")
    WSPutFloatArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_float), c_long]
    WSPutFloatArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6285
if _libs["WSTP64i4"].has("WSPutDoubleArrayData", "cdecl"):
    WSPutDoubleArrayData = _libs["WSTP64i4"].get("WSPutDoubleArrayData", "cdecl")
    WSPutDoubleArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_double), c_long]
    WSPutDoubleArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6286
if _libs["WSTP64i4"].has("WSPutLongDoubleArrayData", "cdecl"):
    WSPutLongDoubleArrayData = _libs["WSTP64i4"].get("WSPutLongDoubleArrayData", "cdecl")
    WSPutLongDoubleArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsextended_double), c_long]
    WSPutLongDoubleArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6288
if _libs["WSTP64i4"].has("WSPutReal32ArrayData", "cdecl"):
    WSPutReal32ArrayData = _libs["WSTP64i4"].get("WSPutReal32ArrayData", "cdecl")
    WSPutReal32ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_float), c_int]
    WSPutReal32ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6289
if _libs["WSTP64i4"].has("WSPutReal64ArrayData", "cdecl"):
    WSPutReal64ArrayData = _libs["WSTP64i4"].get("WSPutReal64ArrayData", "cdecl")
    WSPutReal64ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_double), c_int]
    WSPutReal64ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6290
if _libs["WSTP64i4"].has("WSPutReal128ArrayData", "cdecl"):
    WSPutReal128ArrayData = _libs["WSTP64i4"].get("WSPutReal128ArrayData", "cdecl")
    WSPutReal128ArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsextended_double), c_int]
    WSPutReal128ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6297
if _libs["WSTP64i4"].has("WSPutBinaryNumberArray", "cdecl"):
    WSPutBinaryNumberArray = _libs["WSTP64i4"].get("WSPutBinaryNumberArray", "cdecl")
    WSPutBinaryNumberArray.argtypes = [WSLINK, POINTER(None), POINTER(c_long), POINTER(POINTER(c_char)), c_long, c_long]
    WSPutBinaryNumberArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6298
if _libs["WSTP64i4"].has("WSPutByteArray", "cdecl"):
    WSPutByteArray = _libs["WSTP64i4"].get("WSPutByteArray", "cdecl")
    WSPutByteArray.argtypes = [WSLINK, POINTER(c_ubyte), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutByteArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6299
if _libs["WSTP64i4"].has("WSPutShortIntegerArray", "cdecl"):
    WSPutShortIntegerArray = _libs["WSTP64i4"].get("WSPutShortIntegerArray", "cdecl")
    WSPutShortIntegerArray.argtypes = [WSLINK, POINTER(c_short), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutShortIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6300
if _libs["WSTP64i4"].has("WSPutIntegerArray", "cdecl"):
    WSPutIntegerArray = _libs["WSTP64i4"].get("WSPutIntegerArray", "cdecl")
    WSPutIntegerArray.argtypes = [WSLINK, POINTER(c_int), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6301
if _libs["WSTP64i4"].has("WSPutLongIntegerArray", "cdecl"):
    WSPutLongIntegerArray = _libs["WSTP64i4"].get("WSPutLongIntegerArray", "cdecl")
    WSPutLongIntegerArray.argtypes = [WSLINK, POINTER(c_long), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutLongIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6303
if _libs["WSTP64i4"].has("WSPutInteger8Array", "cdecl"):
    WSPutInteger8Array = _libs["WSTP64i4"].get("WSPutInteger8Array", "cdecl")
    WSPutInteger8Array.argtypes = [WSLINK, POINTER(c_ubyte), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutInteger8Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6304
if _libs["WSTP64i4"].has("WSPutInteger16Array", "cdecl"):
    WSPutInteger16Array = _libs["WSTP64i4"].get("WSPutInteger16Array", "cdecl")
    WSPutInteger16Array.argtypes = [WSLINK, POINTER(c_short), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutInteger16Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6305
if _libs["WSTP64i4"].has("WSPutInteger32Array", "cdecl"):
    WSPutInteger32Array = _libs["WSTP64i4"].get("WSPutInteger32Array", "cdecl")
    WSPutInteger32Array.argtypes = [WSLINK, POINTER(c_int), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutInteger32Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6306
if _libs["WSTP64i4"].has("WSPutInteger64Array", "cdecl"):
    WSPutInteger64Array = _libs["WSTP64i4"].get("WSPutInteger64Array", "cdecl")
    WSPutInteger64Array.argtypes = [WSLINK, POINTER(wsint64), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutInteger64Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6309
if _libs["WSTP64i4"].has("WSPutFloatArray", "cdecl"):
    WSPutFloatArray = _libs["WSTP64i4"].get("WSPutFloatArray", "cdecl")
    WSPutFloatArray.argtypes = [WSLINK, POINTER(c_float), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutFloatArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6310
if _libs["WSTP64i4"].has("WSPutDoubleArray", "cdecl"):
    WSPutDoubleArray = _libs["WSTP64i4"].get("WSPutDoubleArray", "cdecl")
    WSPutDoubleArray.argtypes = [WSLINK, POINTER(c_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutDoubleArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6311
if _libs["WSTP64i4"].has("WSPutRealArray", "cdecl"):
    WSPutRealArray = _libs["WSTP64i4"].get("WSPutRealArray", "cdecl")
    WSPutRealArray.argtypes = [WSLINK, POINTER(c_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutRealArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6312
if _libs["WSTP64i4"].has("WSPutLongDoubleArray", "cdecl"):
    WSPutLongDoubleArray = _libs["WSTP64i4"].get("WSPutLongDoubleArray", "cdecl")
    WSPutLongDoubleArray.argtypes = [WSLINK, POINTER(wsextended_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSPutLongDoubleArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6314
if _libs["WSTP64i4"].has("WSPutReal32Array", "cdecl"):
    WSPutReal32Array = _libs["WSTP64i4"].get("WSPutReal32Array", "cdecl")
    WSPutReal32Array.argtypes = [WSLINK, POINTER(c_float), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutReal32Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6315
if _libs["WSTP64i4"].has("WSPutReal64Array", "cdecl"):
    WSPutReal64Array = _libs["WSTP64i4"].get("WSPutReal64Array", "cdecl")
    WSPutReal64Array.argtypes = [WSLINK, POINTER(c_double), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutReal64Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6316
if _libs["WSTP64i4"].has("WSPutReal128Array", "cdecl"):
    WSPutReal128Array = _libs["WSTP64i4"].get("WSPutReal128Array", "cdecl")
    WSPutReal128Array.argtypes = [WSLINK, POINTER(wsextended_double), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSPutReal128Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6319
if _libs["WSTP64i4"].has("WSPutBinaryNumberList", "cdecl"):
    WSPutBinaryNumberList = _libs["WSTP64i4"].get("WSPutBinaryNumberList", "cdecl")
    WSPutBinaryNumberList.argtypes = [WSLINK, POINTER(None), c_long, c_long]
    WSPutBinaryNumberList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6320
if _libs["WSTP64i4"].has("WSPutIntegerList", "cdecl"):
    WSPutIntegerList = _libs["WSTP64i4"].get("WSPutIntegerList", "cdecl")
    WSPutIntegerList.argtypes = [WSLINK, POINTER(c_int), c_long]
    WSPutIntegerList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6321
if _libs["WSTP64i4"].has("WSPutRealList", "cdecl"):
    WSPutRealList = _libs["WSTP64i4"].get("WSPutRealList", "cdecl")
    WSPutRealList.argtypes = [WSLINK, POINTER(c_double), c_long]
    WSPutRealList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6323
if _libs["WSTP64i4"].has("WSPutInteger8List", "cdecl"):
    WSPutInteger8List = _libs["WSTP64i4"].get("WSPutInteger8List", "cdecl")
    WSPutInteger8List.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSPutInteger8List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6324
if _libs["WSTP64i4"].has("WSPutInteger16List", "cdecl"):
    WSPutInteger16List = _libs["WSTP64i4"].get("WSPutInteger16List", "cdecl")
    WSPutInteger16List.argtypes = [WSLINK, POINTER(c_short), c_int]
    WSPutInteger16List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6325
if _libs["WSTP64i4"].has("WSPutInteger32List", "cdecl"):
    WSPutInteger32List = _libs["WSTP64i4"].get("WSPutInteger32List", "cdecl")
    WSPutInteger32List.argtypes = [WSLINK, POINTER(c_int), c_int]
    WSPutInteger32List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6326
if _libs["WSTP64i4"].has("WSPutInteger64List", "cdecl"):
    WSPutInteger64List = _libs["WSTP64i4"].get("WSPutInteger64List", "cdecl")
    WSPutInteger64List.argtypes = [WSLINK, POINTER(wsint64), c_int]
    WSPutInteger64List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6328
if _libs["WSTP64i4"].has("WSPutReal32List", "cdecl"):
    WSPutReal32List = _libs["WSTP64i4"].get("WSPutReal32List", "cdecl")
    WSPutReal32List.argtypes = [WSLINK, POINTER(c_float), c_int]
    WSPutReal32List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6329
if _libs["WSTP64i4"].has("WSPutReal64List", "cdecl"):
    WSPutReal64List = _libs["WSTP64i4"].get("WSPutReal64List", "cdecl")
    WSPutReal64List.argtypes = [WSLINK, POINTER(c_double), c_int]
    WSPutReal64List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6330
if _libs["WSTP64i4"].has("WSPutReal128List", "cdecl"):
    WSPutReal128List = _libs["WSTP64i4"].get("WSPutReal128List", "cdecl")
    WSPutReal128List.argtypes = [WSLINK, POINTER(wsextended_double), c_int]
    WSPutReal128List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6333
if _libs["WSTP64i4"].has("WSPutArrayType", "cdecl"):
    WSPutArrayType = _libs["WSTP64i4"].get("WSPutArrayType", "cdecl")
    WSPutArrayType.argtypes = [WSLINK, WSLINK, c_long, array_meterpp]
    WSPutArrayType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6334
if _libs["WSTP64i4"].has("WSReleasePutArrayState", "cdecl"):
    WSReleasePutArrayState = _libs["WSTP64i4"].get("WSReleasePutArrayState", "cdecl")
    WSReleasePutArrayState.argtypes = [WSLINK, WSLINK, array_meterp]
    WSReleasePutArrayState.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6336
if _libs["WSTP64i4"].has("WSPutArrayLeaves", "cdecl"):
    WSPutArrayLeaves = _libs["WSTP64i4"].get("WSPutArrayLeaves", "cdecl")
    WSPutArrayLeaves.argtypes = [WSLINK, WSLINK, array_meterp, WSLINK, c_long]
    WSPutArrayLeaves.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6337
if _libs["WSTP64i4"].has("WSPutBinaryNumberArrayDataWithHeads", "cdecl"):
    WSPutBinaryNumberArrayDataWithHeads = _libs["WSTP64i4"].get("WSPutBinaryNumberArrayDataWithHeads", "cdecl")
    WSPutBinaryNumberArrayDataWithHeads.argtypes = [WSLINK, WSLINK, array_meterp, POINTER(None), c_long, c_long]
    WSPutBinaryNumberArrayDataWithHeads.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6369
if _libs["WSTP64i4"].has("WSGetArrayDimensions", "cdecl"):
    WSGetArrayDimensions = _libs["WSTP64i4"].get("WSGetArrayDimensions", "cdecl")
    WSGetArrayDimensions.argtypes = [WSLINK, array_meterp]
    WSGetArrayDimensions.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6370
if _libs["WSTP64i4"].has("WSGetArrayType", "cdecl"):
    WSGetArrayType = _libs["WSTP64i4"].get("WSGetArrayType", "cdecl")
    WSGetArrayType.argtypes = [WSLINK, array_meterp]
    WSGetArrayType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6372
if _libs["WSTP64i4"].has("WSGetBinaryNumberList", "cdecl"):
    WSGetBinaryNumberList = _libs["WSTP64i4"].get("WSGetBinaryNumberList", "cdecl")
    WSGetBinaryNumberList.argtypes = [WSLINK, POINTER(POINTER(None)), POINTER(c_long), c_long]
    WSGetBinaryNumberList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6381
if _libs["WSTP64i4"].has("WSGetIntegerList", "cdecl"):
    WSGetIntegerList = _libs["WSTP64i4"].get("WSGetIntegerList", "cdecl")
    WSGetIntegerList.argtypes = [WSLINK, POINTER(POINTER(c_int)), POINTER(c_long)]
    WSGetIntegerList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6382
if _libs["WSTP64i4"].has("WSGetRealList", "cdecl"):
    WSGetRealList = _libs["WSTP64i4"].get("WSGetRealList", "cdecl")
    WSGetRealList.argtypes = [WSLINK, POINTER(POINTER(c_double)), POINTER(c_long)]
    WSGetRealList.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6384
if _libs["WSTP64i4"].has("WSGetInteger16List", "cdecl"):
    WSGetInteger16List = _libs["WSTP64i4"].get("WSGetInteger16List", "cdecl")
    WSGetInteger16List.argtypes = [WSLINK, POINTER(POINTER(c_short)), POINTER(c_int)]
    WSGetInteger16List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6385
if _libs["WSTP64i4"].has("WSGetInteger32List", "cdecl"):
    WSGetInteger32List = _libs["WSTP64i4"].get("WSGetInteger32List", "cdecl")
    WSGetInteger32List.argtypes = [WSLINK, POINTER(POINTER(c_int)), POINTER(c_int)]
    WSGetInteger32List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6386
if _libs["WSTP64i4"].has("WSGetInteger64List", "cdecl"):
    WSGetInteger64List = _libs["WSTP64i4"].get("WSGetInteger64List", "cdecl")
    WSGetInteger64List.argtypes = [WSLINK, POINTER(POINTER(wsint64)), POINTER(c_int)]
    WSGetInteger64List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6388
if _libs["WSTP64i4"].has("WSGetReal32List", "cdecl"):
    WSGetReal32List = _libs["WSTP64i4"].get("WSGetReal32List", "cdecl")
    WSGetReal32List.argtypes = [WSLINK, POINTER(POINTER(c_float)), POINTER(c_int)]
    WSGetReal32List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6389
if _libs["WSTP64i4"].has("WSGetReal64List", "cdecl"):
    WSGetReal64List = _libs["WSTP64i4"].get("WSGetReal64List", "cdecl")
    WSGetReal64List.argtypes = [WSLINK, POINTER(POINTER(c_double)), POINTER(c_int)]
    WSGetReal64List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6390
if _libs["WSTP64i4"].has("WSGetReal128List", "cdecl"):
    WSGetReal128List = _libs["WSTP64i4"].get("WSGetReal128List", "cdecl")
    WSGetReal128List.argtypes = [WSLINK, POINTER(POINTER(wsextended_double)), POINTER(c_int)]
    WSGetReal128List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6401
if _libs["WSTP64i4"].has("WSReleaseIntegerList", "cdecl"):
    WSReleaseIntegerList = _libs["WSTP64i4"].get("WSReleaseIntegerList", "cdecl")
    WSReleaseIntegerList.argtypes = [WSLINK, POINTER(c_int), c_long]
    WSReleaseIntegerList.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6402
if _libs["WSTP64i4"].has("WSReleaseRealList", "cdecl"):
    WSReleaseRealList = _libs["WSTP64i4"].get("WSReleaseRealList", "cdecl")
    WSReleaseRealList.argtypes = [WSLINK, POINTER(c_double), c_long]
    WSReleaseRealList.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6404
if _libs["WSTP64i4"].has("WSReleaseBinaryNumberList", "cdecl"):
    WSReleaseBinaryNumberList = _libs["WSTP64i4"].get("WSReleaseBinaryNumberList", "cdecl")
    WSReleaseBinaryNumberList.argtypes = [WSLINK, POINTER(None), c_int, c_long]
    WSReleaseBinaryNumberList.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6405
if _libs["WSTP64i4"].has("WSReleaseInteger16List", "cdecl"):
    WSReleaseInteger16List = _libs["WSTP64i4"].get("WSReleaseInteger16List", "cdecl")
    WSReleaseInteger16List.argtypes = [WSLINK, POINTER(c_short), c_int]
    WSReleaseInteger16List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6406
if _libs["WSTP64i4"].has("WSReleaseInteger32List", "cdecl"):
    WSReleaseInteger32List = _libs["WSTP64i4"].get("WSReleaseInteger32List", "cdecl")
    WSReleaseInteger32List.argtypes = [WSLINK, POINTER(c_int), c_int]
    WSReleaseInteger32List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6407
if _libs["WSTP64i4"].has("WSReleaseInteger64List", "cdecl"):
    WSReleaseInteger64List = _libs["WSTP64i4"].get("WSReleaseInteger64List", "cdecl")
    WSReleaseInteger64List.argtypes = [WSLINK, POINTER(wsint64), c_int]
    WSReleaseInteger64List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6409
if _libs["WSTP64i4"].has("WSReleaseReal32List", "cdecl"):
    WSReleaseReal32List = _libs["WSTP64i4"].get("WSReleaseReal32List", "cdecl")
    WSReleaseReal32List.argtypes = [WSLINK, POINTER(c_float), c_int]
    WSReleaseReal32List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6410
if _libs["WSTP64i4"].has("WSReleaseReal64List", "cdecl"):
    WSReleaseReal64List = _libs["WSTP64i4"].get("WSReleaseReal64List", "cdecl")
    WSReleaseReal64List.argtypes = [WSLINK, POINTER(c_double), c_int]
    WSReleaseReal64List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6411
if _libs["WSTP64i4"].has("WSReleaseReal128List", "cdecl"):
    WSReleaseReal128List = _libs["WSTP64i4"].get("WSReleaseReal128List", "cdecl")
    WSReleaseReal128List.argtypes = [WSLINK, POINTER(wsextended_double), c_int]
    WSReleaseReal128List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6413
if _libs["WSTP64i4"].has("WSGetBinaryNumberArrayData", "cdecl"):
    WSGetBinaryNumberArrayData = _libs["WSTP64i4"].get("WSGetBinaryNumberArrayData", "cdecl")
    WSGetBinaryNumberArrayData.argtypes = [WSLINK, array_meterp, POINTER(None), c_long, c_long]
    WSGetBinaryNumberArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6414
if _libs["WSTP64i4"].has("WSGetByteArrayData", "cdecl"):
    WSGetByteArrayData = _libs["WSTP64i4"].get("WSGetByteArrayData", "cdecl")
    WSGetByteArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_ubyte), c_long]
    WSGetByteArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6425
if _libs["WSTP64i4"].has("WSGetShortIntegerArrayData", "cdecl"):
    WSGetShortIntegerArrayData = _libs["WSTP64i4"].get("WSGetShortIntegerArrayData", "cdecl")
    WSGetShortIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_short), c_long]
    WSGetShortIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6426
if _libs["WSTP64i4"].has("WSGetIntegerArrayData", "cdecl"):
    WSGetIntegerArrayData = _libs["WSTP64i4"].get("WSGetIntegerArrayData", "cdecl")
    WSGetIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_int), c_long]
    WSGetIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6427
if _libs["WSTP64i4"].has("WSGetLongIntegerArrayData", "cdecl"):
    WSGetLongIntegerArrayData = _libs["WSTP64i4"].get("WSGetLongIntegerArrayData", "cdecl")
    WSGetLongIntegerArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_long), c_long]
    WSGetLongIntegerArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6429
if _libs["WSTP64i4"].has("WSGetInteger16ArrayData", "cdecl"):
    WSGetInteger16ArrayData = _libs["WSTP64i4"].get("WSGetInteger16ArrayData", "cdecl")
    WSGetInteger16ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_short), c_int]
    WSGetInteger16ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6430
if _libs["WSTP64i4"].has("WSGetInteger32ArrayData", "cdecl"):
    WSGetInteger32ArrayData = _libs["WSTP64i4"].get("WSGetInteger32ArrayData", "cdecl")
    WSGetInteger32ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_int), c_int]
    WSGetInteger32ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6431
if _libs["WSTP64i4"].has("WSGetInteger64ArrayData", "cdecl"):
    WSGetInteger64ArrayData = _libs["WSTP64i4"].get("WSGetInteger64ArrayData", "cdecl")
    WSGetInteger64ArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsint64), c_int]
    WSGetInteger64ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6442
if _libs["WSTP64i4"].has("WSGetFloatArrayData", "cdecl"):
    WSGetFloatArrayData = _libs["WSTP64i4"].get("WSGetFloatArrayData", "cdecl")
    WSGetFloatArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_float), c_long]
    WSGetFloatArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6443
if _libs["WSTP64i4"].has("WSGetDoubleArrayData", "cdecl"):
    WSGetDoubleArrayData = _libs["WSTP64i4"].get("WSGetDoubleArrayData", "cdecl")
    WSGetDoubleArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_double), c_long]
    WSGetDoubleArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6444
if _libs["WSTP64i4"].has("WSGetLongDoubleArrayData", "cdecl"):
    WSGetLongDoubleArrayData = _libs["WSTP64i4"].get("WSGetLongDoubleArrayData", "cdecl")
    WSGetLongDoubleArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsextended_double), c_long]
    WSGetLongDoubleArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6446
if _libs["WSTP64i4"].has("WSGetReal32ArrayData", "cdecl"):
    WSGetReal32ArrayData = _libs["WSTP64i4"].get("WSGetReal32ArrayData", "cdecl")
    WSGetReal32ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_float), c_int]
    WSGetReal32ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6447
if _libs["WSTP64i4"].has("WSGetReal64ArrayData", "cdecl"):
    WSGetReal64ArrayData = _libs["WSTP64i4"].get("WSGetReal64ArrayData", "cdecl")
    WSGetReal64ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_double), c_int]
    WSGetReal64ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6448
if _libs["WSTP64i4"].has("WSGetReal128ArrayData", "cdecl"):
    WSGetReal128ArrayData = _libs["WSTP64i4"].get("WSGetReal128ArrayData", "cdecl")
    WSGetReal128ArrayData.argtypes = [WSLINK, array_meterp, POINTER(wsextended_double), c_int]
    WSGetReal128ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6451
if _libs["WSTP64i4"].has("WSGetInteger8List", "cdecl"):
    WSGetInteger8List = _libs["WSTP64i4"].get("WSGetInteger8List", "cdecl")
    WSGetInteger8List.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(c_int)]
    WSGetInteger8List.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6452
if _libs["WSTP64i4"].has("WSGetInteger8ArrayData", "cdecl"):
    WSGetInteger8ArrayData = _libs["WSTP64i4"].get("WSGetInteger8ArrayData", "cdecl")
    WSGetInteger8ArrayData.argtypes = [WSLINK, array_meterp, POINTER(c_ubyte), c_int]
    WSGetInteger8ArrayData.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6453
if _libs["WSTP64i4"].has("WSReleaseInteger8List", "cdecl"):
    WSReleaseInteger8List = _libs["WSTP64i4"].get("WSReleaseInteger8List", "cdecl")
    WSReleaseInteger8List.argtypes = [WSLINK, POINTER(c_ubyte), c_int]
    WSReleaseInteger8List.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6455
if _libs["WSTP64i4"].has("WSGetArrayTypeWithDepthAndLeafType", "cdecl"):
    WSGetArrayTypeWithDepthAndLeafType = _libs["WSTP64i4"].get("WSGetArrayTypeWithDepthAndLeafType", "cdecl")
    WSGetArrayTypeWithDepthAndLeafType.argtypes = [WSLINK, WSLINK, array_meterpp, POINTER(c_long), POINTER(mlapi__token)]
    WSGetArrayTypeWithDepthAndLeafType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6458
if _libs["WSTP64i4"].has("WSGetBinaryNumberArrayDataWithHeads", "cdecl"):
    WSGetBinaryNumberArrayDataWithHeads = _libs["WSTP64i4"].get("WSGetBinaryNumberArrayDataWithHeads", "cdecl")
    WSGetBinaryNumberArrayDataWithHeads.argtypes = [WSLINK, WSLINK, array_meterp, POINTER(None), POINTER(c_long), c_long]
    WSGetBinaryNumberArrayDataWithHeads.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6460
if _libs["WSTP64i4"].has("WSReleaseGetArrayState", "cdecl"):
    WSReleaseGetArrayState = _libs["WSTP64i4"].get("WSReleaseGetArrayState", "cdecl")
    WSReleaseGetArrayState.argtypes = [WSLINK, WSLINK, array_meterp]
    WSReleaseGetArrayState.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6462
if _libs["WSTP64i4"].has("WSGetBinaryNumberArrayWithLeafType", "cdecl"):
    WSGetBinaryNumberArrayWithLeafType = _libs["WSTP64i4"].get("WSGetBinaryNumberArrayWithLeafType", "cdecl")
    WSGetBinaryNumberArrayWithLeafType.argtypes = [WSLINK, POINTER(POINTER(None)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long), c_long, POINTER(mlapi__token)]
    WSGetBinaryNumberArrayWithLeafType.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6466
if _libs["WSTP64i4"].has("WSGetBinaryNumberArray", "cdecl"):
    WSGetBinaryNumberArray = _libs["WSTP64i4"].get("WSGetBinaryNumberArray", "cdecl")
    WSGetBinaryNumberArray.argtypes = [WSLINK, POINTER(POINTER(None)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long), c_long]
    WSGetBinaryNumberArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6467
if _libs["WSTP64i4"].has("WSGetByteArray", "cdecl"):
    WSGetByteArray = _libs["WSTP64i4"].get("WSGetByteArray", "cdecl")
    WSGetByteArray.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetByteArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6478
if _libs["WSTP64i4"].has("WSGetShortIntegerArray", "cdecl"):
    WSGetShortIntegerArray = _libs["WSTP64i4"].get("WSGetShortIntegerArray", "cdecl")
    WSGetShortIntegerArray.argtypes = [WSLINK, POINTER(POINTER(c_short)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetShortIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6479
if _libs["WSTP64i4"].has("WSGetIntegerArray", "cdecl"):
    WSGetIntegerArray = _libs["WSTP64i4"].get("WSGetIntegerArray", "cdecl")
    WSGetIntegerArray.argtypes = [WSLINK, POINTER(POINTER(c_int)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6480
if _libs["WSTP64i4"].has("WSGetLongIntegerArray", "cdecl"):
    WSGetLongIntegerArray = _libs["WSTP64i4"].get("WSGetLongIntegerArray", "cdecl")
    WSGetLongIntegerArray.argtypes = [WSLINK, POINTER(POINTER(c_long)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetLongIntegerArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6482
if _libs["WSTP64i4"].has("WSGetInteger16Array", "cdecl"):
    WSGetInteger16Array = _libs["WSTP64i4"].get("WSGetInteger16Array", "cdecl")
    WSGetInteger16Array.argtypes = [WSLINK, POINTER(POINTER(c_short)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetInteger16Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6483
if _libs["WSTP64i4"].has("WSGetInteger32Array", "cdecl"):
    WSGetInteger32Array = _libs["WSTP64i4"].get("WSGetInteger32Array", "cdecl")
    WSGetInteger32Array.argtypes = [WSLINK, POINTER(POINTER(c_int)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetInteger32Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6484
if _libs["WSTP64i4"].has("WSGetInteger64Array", "cdecl"):
    WSGetInteger64Array = _libs["WSTP64i4"].get("WSGetInteger64Array", "cdecl")
    WSGetInteger64Array.argtypes = [WSLINK, POINTER(POINTER(wsint64)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetInteger64Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6487
if _libs["WSTP64i4"].has("WSGetInteger8Array", "cdecl"):
    WSGetInteger8Array = _libs["WSTP64i4"].get("WSGetInteger8Array", "cdecl")
    WSGetInteger8Array.argtypes = [WSLINK, POINTER(POINTER(c_ubyte)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetInteger8Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6499
if _libs["WSTP64i4"].has("WSGetFloatArray", "cdecl"):
    WSGetFloatArray = _libs["WSTP64i4"].get("WSGetFloatArray", "cdecl")
    WSGetFloatArray.argtypes = [WSLINK, POINTER(POINTER(c_float)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetFloatArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6500
if _libs["WSTP64i4"].has("WSGetDoubleArray", "cdecl"):
    WSGetDoubleArray = _libs["WSTP64i4"].get("WSGetDoubleArray", "cdecl")
    WSGetDoubleArray.argtypes = [WSLINK, POINTER(POINTER(c_double)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetDoubleArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6501
if _libs["WSTP64i4"].has("WSGetRealArray", "cdecl"):
    WSGetRealArray = _libs["WSTP64i4"].get("WSGetRealArray", "cdecl")
    WSGetRealArray.argtypes = [WSLINK, POINTER(POINTER(c_double)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetRealArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6502
if _libs["WSTP64i4"].has("WSGetLongDoubleArray", "cdecl"):
    WSGetLongDoubleArray = _libs["WSTP64i4"].get("WSGetLongDoubleArray", "cdecl")
    WSGetLongDoubleArray.argtypes = [WSLINK, POINTER(POINTER(wsextended_double)), POINTER(POINTER(c_long)), POINTER(POINTER(POINTER(c_char))), POINTER(c_long)]
    WSGetLongDoubleArray.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6504
if _libs["WSTP64i4"].has("WSGetReal32Array", "cdecl"):
    WSGetReal32Array = _libs["WSTP64i4"].get("WSGetReal32Array", "cdecl")
    WSGetReal32Array.argtypes = [WSLINK, POINTER(POINTER(c_float)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetReal32Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6505
if _libs["WSTP64i4"].has("WSGetReal64Array", "cdecl"):
    WSGetReal64Array = _libs["WSTP64i4"].get("WSGetReal64Array", "cdecl")
    WSGetReal64Array.argtypes = [WSLINK, POINTER(POINTER(c_double)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetReal64Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6506
if _libs["WSTP64i4"].has("WSGetReal128Array", "cdecl"):
    WSGetReal128Array = _libs["WSTP64i4"].get("WSGetReal128Array", "cdecl")
    WSGetReal128Array.argtypes = [WSLINK, POINTER(POINTER(wsextended_double)), POINTER(POINTER(c_int)), POINTER(POINTER(POINTER(c_char))), POINTER(c_int)]
    WSGetReal128Array.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6519
if _libs["WSTP64i4"].has("WSReleaseShortIntegerArray", "cdecl"):
    WSReleaseShortIntegerArray = _libs["WSTP64i4"].get("WSReleaseShortIntegerArray", "cdecl")
    WSReleaseShortIntegerArray.argtypes = [WSLINK, POINTER(c_short), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseShortIntegerArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6520
if _libs["WSTP64i4"].has("WSReleaseIntegerArray", "cdecl"):
    WSReleaseIntegerArray = _libs["WSTP64i4"].get("WSReleaseIntegerArray", "cdecl")
    WSReleaseIntegerArray.argtypes = [WSLINK, POINTER(c_int), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseIntegerArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6521
if _libs["WSTP64i4"].has("WSReleaseLongIntegerArray", "cdecl"):
    WSReleaseLongIntegerArray = _libs["WSTP64i4"].get("WSReleaseLongIntegerArray", "cdecl")
    WSReleaseLongIntegerArray.argtypes = [WSLINK, POINTER(c_long), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseLongIntegerArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6523
if _libs["WSTP64i4"].has("WSReleaseBinaryNumberArray", "cdecl"):
    WSReleaseBinaryNumberArray = _libs["WSTP64i4"].get("WSReleaseBinaryNumberArray", "cdecl")
    WSReleaseBinaryNumberArray.argtypes = [WSLINK, POINTER(None), POINTER(c_int), POINTER(POINTER(c_char)), c_int, c_long]
    WSReleaseBinaryNumberArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6524
if _libs["WSTP64i4"].has("WSReleaseByteArray", "cdecl"):
    WSReleaseByteArray = _libs["WSTP64i4"].get("WSReleaseByteArray", "cdecl")
    WSReleaseByteArray.argtypes = [WSLINK, POINTER(c_ubyte), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseByteArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6525
if _libs["WSTP64i4"].has("WSReleaseInteger16Array", "cdecl"):
    WSReleaseInteger16Array = _libs["WSTP64i4"].get("WSReleaseInteger16Array", "cdecl")
    WSReleaseInteger16Array.argtypes = [WSLINK, POINTER(c_short), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseInteger16Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6526
if _libs["WSTP64i4"].has("WSReleaseInteger32Array", "cdecl"):
    WSReleaseInteger32Array = _libs["WSTP64i4"].get("WSReleaseInteger32Array", "cdecl")
    WSReleaseInteger32Array.argtypes = [WSLINK, POINTER(c_int), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseInteger32Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6527
if _libs["WSTP64i4"].has("WSReleaseInteger64Array", "cdecl"):
    WSReleaseInteger64Array = _libs["WSTP64i4"].get("WSReleaseInteger64Array", "cdecl")
    WSReleaseInteger64Array.argtypes = [WSLINK, POINTER(wsint64), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseInteger64Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6529
if _libs["WSTP64i4"].has("WSReleaseInteger8Array", "cdecl"):
    WSReleaseInteger8Array = _libs["WSTP64i4"].get("WSReleaseInteger8Array", "cdecl")
    WSReleaseInteger8Array.argtypes = [WSLINK, POINTER(c_ubyte), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseInteger8Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6539
if _libs["WSTP64i4"].has("WSReleaseFloatArray", "cdecl"):
    WSReleaseFloatArray = _libs["WSTP64i4"].get("WSReleaseFloatArray", "cdecl")
    WSReleaseFloatArray.argtypes = [WSLINK, POINTER(c_float), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseFloatArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6540
if _libs["WSTP64i4"].has("WSReleaseDoubleArray", "cdecl"):
    WSReleaseDoubleArray = _libs["WSTP64i4"].get("WSReleaseDoubleArray", "cdecl")
    WSReleaseDoubleArray.argtypes = [WSLINK, POINTER(c_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseDoubleArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6541
if _libs["WSTP64i4"].has("WSReleaseRealArray", "cdecl"):
    WSReleaseRealArray = _libs["WSTP64i4"].get("WSReleaseRealArray", "cdecl")
    WSReleaseRealArray.argtypes = [WSLINK, POINTER(c_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseRealArray.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6543
if _libs["WSTP64i4"].has("WSReleaseReal32Array", "cdecl"):
    WSReleaseReal32Array = _libs["WSTP64i4"].get("WSReleaseReal32Array", "cdecl")
    WSReleaseReal32Array.argtypes = [WSLINK, POINTER(c_float), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseReal32Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6544
if _libs["WSTP64i4"].has("WSReleaseReal64Array", "cdecl"):
    WSReleaseReal64Array = _libs["WSTP64i4"].get("WSReleaseReal64Array", "cdecl")
    WSReleaseReal64Array.argtypes = [WSLINK, POINTER(c_double), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseReal64Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6553
if _libs["WSTP64i4"].has("WSReleaseReal128Array", "cdecl"):
    WSReleaseReal128Array = _libs["WSTP64i4"].get("WSReleaseReal128Array", "cdecl")
    WSReleaseReal128Array.argtypes = [WSLINK, POINTER(wsextended_double), POINTER(c_int), POINTER(POINTER(c_char)), c_int]
    WSReleaseReal128Array.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6555
if _libs["WSTP64i4"].has("WSReleaseLongDoubleArray", "cdecl"):
    WSReleaseLongDoubleArray = _libs["WSTP64i4"].get("WSReleaseLongDoubleArray", "cdecl")
    WSReleaseLongDoubleArray.argtypes = [WSLINK, POINTER(wsextended_double), POINTER(c_long), POINTER(POINTER(c_char)), c_long]
    WSReleaseLongDoubleArray.restype = None

enum_WSUnicodeContainerType = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6575

UCS2ContainerType = 0# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6575

UTF8ContainerType = (UCS2ContainerType + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6575

UTF16ContainerType = (UTF8ContainerType + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6575

UTF32ContainerType = (UTF16ContainerType + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6575

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6585
class union__pointer(Union):
    pass

union__pointer.__slots__ = [
    'ucs2',
    'utf8',
    'utf16',
    'utf32',
]
union__pointer._fields_ = [
    ('ucs2', POINTER(c_ushort)),
    ('utf8', POINTER(c_ubyte)),
    ('utf16', POINTER(c_ushort)),
    ('utf32', POINTER(c_uint)),
]

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6595
class struct__MLUnicodeContainer(Structure):
    pass

struct__MLUnicodeContainer.__slots__ = [
    'pointer',
    'length',
    'type',
]
struct__MLUnicodeContainer._fields_ = [
    ('pointer', union__pointer),
    ('length', c_int),
    ('type', enum_WSUnicodeContainerType),
]

WSUnicodeContainer = struct__MLUnicodeContainer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6595

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6604
if _libs["WSTP64i4"].has("WSNewUnicodeContainer", "cdecl"):
    WSNewUnicodeContainer = _libs["WSTP64i4"].get("WSNewUnicodeContainer", "cdecl")
    WSNewUnicodeContainer.argtypes = [POINTER(None), c_int, enum_WSUnicodeContainerType]
    WSNewUnicodeContainer.restype = POINTER(WSUnicodeContainer)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6605
for _lib in _libs.values():
    if not _lib.has("WSReleaseUnicodeContainer", "cdecl"):
        continue
    WSReleaseUnicodeContainer = _lib.get("WSReleaseUnicodeContainer", "cdecl")
    WSReleaseUnicodeContainer.argtypes = [POINTER(WSUnicodeContainer)]
    WSReleaseUnicodeContainer.restype = None
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6627
if _libs["WSTP64i4"].has("WSCreateMark", "cdecl"):
    WSCreateMark = _libs["WSTP64i4"].get("WSCreateMark", "cdecl")
    WSCreateMark.argtypes = [WSLINK]
    WSCreateMark.restype = MLINKMark

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6628
if _libs["WSTP64i4"].has("WSSeekToMark", "cdecl"):
    WSSeekToMark = _libs["WSTP64i4"].get("WSSeekToMark", "cdecl")
    WSSeekToMark.argtypes = [WSLINK, MLINKMark, c_int]
    WSSeekToMark.restype = MLINKMark

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6629
if _libs["WSTP64i4"].has("WSSeekMark", "cdecl"):
    WSSeekMark = _libs["WSTP64i4"].get("WSSeekMark", "cdecl")
    WSSeekMark.argtypes = [WSLINK, MLINKMark, c_int]
    WSSeekMark.restype = MLINKMark

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6630
if _libs["WSTP64i4"].has("WSDestroyMark", "cdecl"):
    WSDestroyMark = _libs["WSTP64i4"].get("WSDestroyMark", "cdecl")
    WSDestroyMark.argtypes = [WSLINK, MLINKMark]
    WSDestroyMark.restype = None

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6652
if _libs["WSTP64i4"].has("WSTransferExpression", "cdecl"):
    WSTransferExpression = _libs["WSTP64i4"].get("WSTransferExpression", "cdecl")
    WSTransferExpression.argtypes = [WSLINK, WSLINK]
    WSTransferExpression.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6653
if _libs["WSTP64i4"].has("WSTransferToEndOfLoopbackLink", "cdecl"):
    WSTransferToEndOfLoopbackLink = _libs["WSTP64i4"].get("WSTransferToEndOfLoopbackLink", "cdecl")
    WSTransferToEndOfLoopbackLink.argtypes = [WSLINK, WSLINK]
    WSTransferToEndOfLoopbackLink.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6674
if _libs["WSTP64i4"].has("WSForwardReset", "cdecl"):
    WSForwardReset = _libs["WSTP64i4"].get("WSForwardReset", "cdecl")
    WSForwardReset.argtypes = [WSLINK, c_ulong]
    WSForwardReset.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6677
if _libs["WSTP64i4"].has("WSAlign", "cdecl"):
    WSAlign = _libs["WSTP64i4"].get("WSAlign", "cdecl")
    WSAlign.argtypes = [WSLINK, WSLINK]
    WSAlign.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6740
if _libs["WSTP64i4"].has("WSNextPacket", "cdecl"):
    WSNextPacket = _libs["WSTP64i4"].get("WSNextPacket", "cdecl")
    WSNextPacket.argtypes = [WSLINK]
    WSNextPacket.restype = c_int

mldlg_result = c_long# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6759

WSAlertProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment, String)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6762

WSRequestProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment, String, String, c_long)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6763

WSConfirmProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment, String, mldlg_result)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6764

WSRequestArgvProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment, POINTER(POINTER(c_char)), c_long, String, c_long)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6765

WSRequestToInteractProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment, mldlg_result)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6766

WSDialogProcPtr = CFUNCTYPE(UNCHECKED(mldlg_result), WSEnvironment)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6767

MLDialogUPP = WSDialogProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6770

MLAlertUPP = WSAlertProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6771

MLRequestUPP = WSRequestProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6772

MLConfirmUPP = WSConfirmProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6773

MLRequestArgvUPP = WSRequestArgvProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6774

MLRequestToInteractUPP = WSRequestToInteractProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6775

MLAlertFunctionType = MLAlertUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6782

MLRequestFunctionType = MLRequestUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6783

MLConfirmFunctionType = MLConfirmUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6784

MLRequestArgvFunctionType = MLRequestArgvUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6785

MLRequestToInteractFunctionType = MLRequestToInteractUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6786

WSDialogFunctionType = MLDialogUPP# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6787

enum_anon_6 = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

WSAlertFunction = 1# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

WSRequestFunction = (WSAlertFunction + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

WSConfirmFunction = (WSRequestFunction + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

WSRequestArgvFunction = (WSConfirmFunction + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

WSRequestToInteractFunction = (WSRequestArgvFunction + 1)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6811

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6888
for _lib in _libs.values():
    if not _lib.has("MLAlert_unix", "cdecl"):
        continue
    MLAlert_unix = _lib.get("MLAlert_unix", "cdecl")
    MLAlert_unix.argtypes = [WSEnvironment, String]
    MLAlert_unix.restype = mldlg_result
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6889
for _lib in _libs.values():
    if not _lib.has("MLRequest_unix", "cdecl"):
        continue
    MLRequest_unix = _lib.get("MLRequest_unix", "cdecl")
    MLRequest_unix.argtypes = [WSEnvironment, String, String, c_long]
    MLRequest_unix.restype = mldlg_result
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6890
for _lib in _libs.values():
    if not _lib.has("MLConfirm_unix", "cdecl"):
        continue
    MLConfirm_unix = _lib.get("MLConfirm_unix", "cdecl")
    MLConfirm_unix.argtypes = [WSEnvironment, String, mldlg_result]
    MLConfirm_unix.restype = mldlg_result
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6891
for _lib in _libs.values():
    if not _lib.has("MLPermit_unix", "cdecl"):
        continue
    MLPermit_unix = _lib.get("MLPermit_unix", "cdecl")
    MLPermit_unix.argtypes = [WSEnvironment, mldlg_result]
    MLPermit_unix.restype = mldlg_result
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6906
for _lib in _libs.values():
    if not _lib.has("default_request_argv", "cdecl"):
        continue
    default_request_argv = _lib.get("default_request_argv", "cdecl")
    default_request_argv.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), c_long, String, c_long]
    default_request_argv.restype = mldlg_result
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6915
if _libs["WSTP64i4"].has("WSAlert", "cdecl"):
    WSAlert = _libs["WSTP64i4"].get("WSAlert", "cdecl")
    WSAlert.argtypes = [WSEnvironment, String]
    WSAlert.restype = mldlg_result

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6916
if _libs["WSTP64i4"].has("WSRequest", "cdecl"):
    WSRequest = _libs["WSTP64i4"].get("WSRequest", "cdecl")
    WSRequest.argtypes = [WSEnvironment, String, String, c_long]
    WSRequest.restype = mldlg_result

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6917
if _libs["WSTP64i4"].has("WSConfirm", "cdecl"):
    WSConfirm = _libs["WSTP64i4"].get("WSConfirm", "cdecl")
    WSConfirm.argtypes = [WSEnvironment, String, mldlg_result]
    WSConfirm.restype = mldlg_result

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6918
if _libs["WSTP64i4"].has("WSRequestArgv", "cdecl"):
    WSRequestArgv = _libs["WSTP64i4"].get("WSRequestArgv", "cdecl")
    WSRequestArgv.argtypes = [WSEnvironment, POINTER(POINTER(c_char)), c_long, String, c_long]
    WSRequestArgv.restype = mldlg_result

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6920
if _libs["WSTP64i4"].has("WSRequestToInteract", "cdecl"):
    WSRequestToInteract = _libs["WSTP64i4"].get("WSRequestToInteract", "cdecl")
    WSRequestToInteract.argtypes = [WSEnvironment, mldlg_result]
    WSRequestToInteract.restype = mldlg_result

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6921
if _libs["WSTP64i4"].has("WSSetDialogFunction", "cdecl"):
    WSSetDialogFunction = _libs["WSTP64i4"].get("WSSetDialogFunction", "cdecl")
    WSSetDialogFunction.argtypes = [WSEnvironment, c_long, WSDialogFunctionType]
    WSSetDialogFunction.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6924
if _libs["WSTP64i4"].has("WSAlertCast", "cdecl"):
    WSAlertCast = _libs["WSTP64i4"].get("WSAlertCast", "cdecl")
    WSAlertCast.argtypes = [WSAlertProcPtr]
    WSAlertCast.restype = WSDialogProcPtr

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6925
if _libs["WSTP64i4"].has("WSRequestCast", "cdecl"):
    WSRequestCast = _libs["WSTP64i4"].get("WSRequestCast", "cdecl")
    WSRequestCast.argtypes = [WSRequestProcPtr]
    WSRequestCast.restype = WSDialogProcPtr

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6926
if _libs["WSTP64i4"].has("WSConfirmCast", "cdecl"):
    WSConfirmCast = _libs["WSTP64i4"].get("WSConfirmCast", "cdecl")
    WSConfirmCast.argtypes = [WSConfirmProcPtr]
    WSConfirmCast.restype = WSDialogProcPtr

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6927
if _libs["WSTP64i4"].has("WSRequestArgvCast", "cdecl"):
    WSRequestArgvCast = _libs["WSTP64i4"].get("WSRequestArgvCast", "cdecl")
    WSRequestArgvCast.argtypes = [WSRequestArgvProcPtr]
    WSRequestArgvCast.restype = WSDialogProcPtr

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6928
if _libs["WSTP64i4"].has("WSRequestToInteractCast", "cdecl"):
    WSRequestToInteractCast = _libs["WSTP64i4"].get("WSRequestToInteractCast", "cdecl")
    WSRequestToInteractCast.argtypes = [WSRequestToInteractProcPtr]
    WSRequestToInteractCast.restype = WSDialogProcPtr

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6949
class struct__mltimeval(Structure):
    pass

struct__mltimeval.__slots__ = [
    'tv_sec',
    'tv_usec',
]
struct__mltimeval._fields_ = [
    ('tv_sec', c_ulong),
    ('tv_usec', c_ulong),
]

wstimeval = struct__mltimeval# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6949

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6957
if _libs["WSTP64i4"].has("WSReady", "cdecl"):
    WSReady = _libs["WSTP64i4"].get("WSReady", "cdecl")
    WSReady.argtypes = [WSLINK]
    WSReady.restype = c_int

WSREADYPARALLELENV = POINTER(None)# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6972

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6975
if _libs["WSTP64i4"].has("WSReadyParallel", "cdecl"):
    WSReadyParallel = _libs["WSTP64i4"].get("WSReadyParallel", "cdecl")
    WSReadyParallel.argtypes = [WSENV, POINTER(WSLINK), c_int, wstimeval]
    WSReadyParallel.restype = c_int

WSLinkWaitCallBackObject = CFUNCTYPE(UNCHECKED(c_int), WSLINK, POINTER(None))# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6980

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6982
if _libs["WSTP64i4"].has("WSWaitForLinkActivity", "cdecl"):
    WSWaitForLinkActivity = _libs["WSTP64i4"].get("WSWaitForLinkActivity", "cdecl")
    WSWaitForLinkActivity.argtypes = [WSLINK]
    WSWaitForLinkActivity.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6983
if _libs["WSTP64i4"].has("WSWaitForLinkActivityWithCallback", "cdecl"):
    WSWaitForLinkActivityWithCallback = _libs["WSTP64i4"].get("WSWaitForLinkActivityWithCallback", "cdecl")
    WSWaitForLinkActivityWithCallback.argtypes = [WSLINK, WSLinkWaitCallBackObject]
    WSWaitForLinkActivityWithCallback.restype = c_int

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7011
for _lib in _libs.values():
    try:
        stdlink = (WSLINK).in_dll(_lib, "stdlink")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7012
for _lib in _libs.values():
    try:
        stdenv = (WSEnvironment).in_dll(_lib, "stdenv")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7014
for _lib in _libs.values():
    try:
        stdyielder = (WSYieldFunctionObject).in_dll(_lib, "stdyielder")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7015
for _lib in _libs.values():
    try:
        stdhandler = (WSMessageHandlerObject).in_dll(_lib, "stdhandler")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7017
for _lib in _libs.values():
    if not _lib.has("WSMain", "cdecl"):
        continue
    WSMain = _lib.get("WSMain", "cdecl")
    WSMain.argtypes = [c_int, POINTER(POINTER(c_char))]
    WSMain.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7018
for _lib in _libs.values():
    if not _lib.has("WSMainString", "cdecl"):
        continue
    WSMainString = _lib.get("WSMainString", "cdecl")
    WSMainString.argtypes = [String]
    WSMainString.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7019
for _lib in _libs.values():
    if not _lib.has("WSMainArgv", "cdecl"):
        continue
    WSMainArgv = _lib.get("WSMainArgv", "cdecl")
    WSMainArgv.argtypes = [POINTER(POINTER(c_char)), POINTER(POINTER(c_char))]
    WSMainArgv.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7021
for _lib in _libs.values():
    if not _lib.has("WSInstall", "cdecl"):
        continue
    WSInstall = _lib.get("WSInstall", "cdecl")
    WSInstall.argtypes = [WSLINK]
    WSInstall.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7022
for _lib in _libs.values():
    if not _lib.has("WSAnswer", "cdecl"):
        continue
    WSAnswer = _lib.get("WSAnswer", "cdecl")
    WSAnswer.argtypes = [WSLINK]
    WSAnswer.restype = wsapi_packet
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7023
for _lib in _libs.values():
    if not _lib.has("WSDoCallPacket", "cdecl"):
        continue
    WSDoCallPacket = _lib.get("WSDoCallPacket", "cdecl")
    WSDoCallPacket.argtypes = [WSLINK]
    WSDoCallPacket.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7024
for _lib in _libs.values():
    if not _lib.has("WSEvaluate", "cdecl"):
        continue
    WSEvaluate = _lib.get("WSEvaluate", "cdecl")
    WSEvaluate.argtypes = [WSLINK, String]
    WSEvaluate.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7025
for _lib in _libs.values():
    if not _lib.has("WSEvaluateString", "cdecl"):
        continue
    WSEvaluateString = _lib.get("WSEvaluateString", "cdecl")
    WSEvaluateString.argtypes = [WSLINK, String]
    WSEvaluateString.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7028
for _lib in _libs.values():
    if not _lib.has("WSDefaultHandler", "cdecl"):
        continue
    WSDefaultHandler = _lib.get("WSDefaultHandler", "cdecl")
    WSDefaultHandler.argtypes = [WSLINK, c_int, c_int]
    WSDefaultHandler.restype = None
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7029
for _lib in _libs.values():
    if not _lib.has("WSDefaultYielder", "cdecl"):
        continue
    WSDefaultYielder = _lib.get("WSDefaultYielder", "cdecl")
    WSDefaultYielder.argtypes = [WSLINK, WSYieldParameters]
    WSDefaultYielder.restype = c_int
    break

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7032
for _lib in _libs.values():
    try:
        WSAbort = (c_int).in_dll(_lib, "WSAbort")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7032
for _lib in _libs.values():
    try:
        WSDone = (c_int).in_dll(_lib, "WSDone")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7033
for _lib in _libs.values():
    try:
        WSSpecialCharacter = (c_long).in_dll(_lib, "WSSpecialCharacter")
        break
    except:
        pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 19
try:
    UNIX_MATHLINK = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 20
try:
    UNIX_WSTP = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 77
try:
    LITTLEENDIAN_NUMERIC_TYPES = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 78
try:
    LINUX_MATHLINK = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 80
try:
    X86_64_LINUX_MATHLINK = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 111
try:
    WSVERSION = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 114
try:
    WSINTERFACE = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 121
try:
    WSREVISION = 48
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 208
try:
    WSCREATIONID = 114411
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 211
try:
    WSAPI4REVISION = 25
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 214
try:
    WSAPIREVISION = WSAPI4REVISION
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 225
try:
    WSOLDDEFINITION = WSAPI4REVISION
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 242
try:
    MLActivate = WSActivate
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 243
try:
    MLAlert = WSAlert
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 244
try:
    MLAlertCast = WSAlertCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 245
try:
    MLAlign = WSAlign
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 246
try:
    MLAllocParameter = WSAllocParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 247
try:
    MLAllocatorCast = WSAllocatorCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 248
try:
    MLBegin = WSBegin
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 249
try:
    MLBrowseForLinkServices = WSBrowseForLinkServices
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 250
try:
    MLBytesToGet = WSBytesToGet
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 251
try:
    MLBytesToPut = WSBytesToPut
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 252
try:
    MLCallMessageHandler = WSCallMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 253
try:
    MLCallYieldFunction = WSCallYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 254
try:
    MLCharacterOffset = WSCharacterOffset
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 255
try:
    MLCheckFunction = WSCheckFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 256
try:
    MLCheckFunctionWithArgCount = WSCheckFunctionWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 257
try:
    MLClearAllSymbolReplacements = WSClearAllSymbolReplacements
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 258
try:
    MLClearError = WSClearError
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 259
try:
    MLClearSymbolReplacement = WSClearSymbolReplacement
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 260
try:
    MLClose = WSClose
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 261
try:
    MLCompilerID = WSCompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 262
try:
    MLConfirm = WSConfirm
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 263
try:
    MLConfirmCast = WSConfirmCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 264
try:
    MLConnect = WSConnect
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 265
try:
    MLContextFromLinkServer = WSContextFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 266
try:
    MLConvertByteString = WSConvertByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 267
try:
    MLConvertByteStringNL = WSConvertByteStringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 268
try:
    MLConvertCharacter = WSConvertCharacter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 269
try:
    MLConvertDoubleByteString = WSConvertDoubleByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 270
try:
    MLConvertDoubleByteStringNL = WSConvertDoubleByteStringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 271
try:
    MLConvertNewLine = WSConvertNewLine
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 272
try:
    MLConvertUCS2String = WSConvertUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 273
try:
    MLConvertUCS2StringNL = WSConvertUCS2StringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 274
try:
    MLConvertUTF16String = WSConvertUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 275
try:
    MLConvertUTF16StringNL = WSConvertUTF16StringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 276
try:
    MLConvertUTF32String = WSConvertUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 277
try:
    MLConvertUTF32StringNL = WSConvertUTF32StringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 278
try:
    MLConvertUTF8String = WSConvertUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 279
try:
    MLConvertUTF8StringNL = WSConvertUTF8StringNL
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 280
try:
    MLCountYP = WSCountYP
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 281
try:
    MLCreateMark = WSCreateMark
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 282
try:
    MLCreateMessageHandler = WSCreateMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 283
try:
    MLCreateYieldFunction = WSCreateYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 284
try:
    MLDeallocatorCast = WSDeallocatorCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 285
try:
    MLDefaultYieldFunction = WSDefaultYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 286
try:
    MLDeinit = WSDeinit
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 287
try:
    MLDeinitialize = WSDeinitialize
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 288
try:
    MLDestroyMark = WSDestroyMark
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 289
try:
    MLDestroyMessageHandler = WSDestroyMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 290
try:
    MLDestroyYieldFunction = WSDestroyYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 291
try:
    MLDeviceInformation = WSDeviceInformation
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 292
try:
    MLDisableLinkLock = WSDisableLinkLock
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 293
try:
    MLDisableLoggingStream = WSDisableLoggingStream
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 294
try:
    MLDoNotHandleSignalParameter = WSDoNotHandleSignalParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 295
try:
    MLDuplicateLink = WSDuplicateLink
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 296
try:
    MLEnableLinkLock = WSEnableLinkLock
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 297
try:
    MLEnableLoggingStream = WSEnableLoggingStream
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 298
try:
    MLEnclosingEnvironment = WSEnclosingEnvironment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 299
try:
    MLEnd = WSEnd
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 300
try:
    MLEndPacket = WSEndPacket
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 301
try:
    MLEnvironmentData = WSEnvironmentData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 302
try:
    MLError = WSError
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 303
try:
    MLErrorMessage = WSErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 304
try:
    MLErrorParameter = WSErrorParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 305
try:
    MLErrorString = WSErrorString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 306
try:
    MLEstablish = WSEstablish
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 307
try:
    MLEstablishString = WSEstablishString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 308
try:
    MLExpressionsToGet = WSExpressionsToGet
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 309
try:
    MLFeatureString = WSFeatureString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 310
try:
    MLFill = WSFill
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 311
try:
    MLFilterArgv = WSFilterArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 312
try:
    MLFlush = WSFlush
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 313
try:
    MLForwardReset = WSForwardReset
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 314
try:
    MLFromLinkID = WSFromLinkID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 315
try:
    MLGet7BitCharacters = WSGet7BitCharacters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 316
try:
    MLGet8BitCharacters = WSGet8BitCharacters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 317
try:
    MLGetAvailableLinkProtocolNames = WSGetAvailableLinkProtocolNames
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 318
try:
    MLGetArgCount = WSGetArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 319
try:
    MLGetArrayDimensions = WSGetArrayDimensions
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 320
try:
    MLGetArrayType = WSGetArrayType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 321
try:
    MLGetArrayTypeWithDepthAndLeafType = WSGetArrayTypeWithDepthAndLeafType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 322
try:
    MLGetBinaryNumber = WSGetBinaryNumber
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 323
try:
    MLGetBinaryNumberArray = WSGetBinaryNumberArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 324
try:
    MLGetBinaryNumberArrayData = WSGetBinaryNumberArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 325
try:
    MLGetBinaryNumberArrayDataWithHeads = WSGetBinaryNumberArrayDataWithHeads
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 326
try:
    MLGetBinaryNumberArrayWithLeafType = WSGetBinaryNumberArrayWithLeafType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 327
try:
    MLGetBinaryNumberList = WSGetBinaryNumberList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 328
try:
    MLGetByteArray = WSGetByteArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 329
try:
    MLGetByteArrayData = WSGetByteArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 330
try:
    MLGetByteString = WSGetByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 331
try:
    MLGetByteSymbol = WSGetByteSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 332
try:
    MLGetData = WSGetData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 333
try:
    MLGetDomainNameList = WSGetDomainNameList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 334
try:
    MLGetDouble = WSGetDouble
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 335
try:
    MLGetDoubleArray = WSGetDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 336
try:
    MLGetDoubleArrayData = WSGetDoubleArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 337
try:
    MLGetFloat = WSGetFloat
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 338
try:
    MLGetFloatArray = WSGetFloatArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 339
try:
    MLGetFloatArrayData = WSGetFloatArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 340
try:
    MLGetFunction = WSGetFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 341
try:
    MLGetInteger = WSGetInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 342
try:
    MLGetInteger16 = WSGetInteger16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 343
try:
    MLGetInteger16Array = WSGetInteger16Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 344
try:
    MLGetInteger16ArrayData = WSGetInteger16ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 345
try:
    MLGetInteger16List = WSGetInteger16List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 346
try:
    MLGetInteger32 = WSGetInteger32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 347
try:
    MLGetInteger32Array = WSGetInteger32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 348
try:
    MLGetInteger32ArrayData = WSGetInteger32ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 349
try:
    MLGetInteger32List = WSGetInteger32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 350
try:
    MLGetInteger64 = WSGetInteger64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 351
try:
    MLGetInteger64Array = WSGetInteger64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 352
try:
    MLGetInteger64ArrayData = WSGetInteger64ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 353
try:
    MLGetInteger64List = WSGetInteger64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 354
try:
    MLGetInteger8 = WSGetInteger8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 355
try:
    MLGetInteger8Array = WSGetInteger8Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 356
try:
    MLGetInteger8ArrayData = WSGetInteger8ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 357
try:
    MLGetInteger8List = WSGetInteger8List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 358
try:
    MLGetIntegerArray = WSGetIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 359
try:
    MLGetIntegerArrayData = WSGetIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 360
try:
    MLGetIntegerList = WSGetIntegerList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 361
try:
    MLGetLinkedEnvIDString = WSGetLinkedEnvIDString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 362
try:
    MLGetLinksFromEnvironment = WSGetLinksFromEnvironment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 363
try:
    MLGetLongDouble = WSGetLongDouble
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 364
try:
    MLGetLongDoubleArray = WSGetLongDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 365
try:
    MLGetLongDoubleArrayData = WSGetLongDoubleArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 366
try:
    MLGetLongInteger = WSGetLongInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 367
try:
    MLGetLongIntegerArray = WSGetLongIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 368
try:
    MLGetLongIntegerArrayData = WSGetLongIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 369
try:
    MLGetMessage = WSGetMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 370
try:
    MLGetMessageHandler = WSGetMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 371
try:
    MLGetNetworkAddressList = WSGetNetworkAddressList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 372
try:
    MLGetNext = WSGetNext
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 373
try:
    MLGetNextRaw = WSGetNextRaw
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 374
try:
    MLGetNumberAsByteString = WSGetNumberAsByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 375
try:
    MLGetNumberAsString = WSGetNumberAsString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 376
try:
    MLGetNumberAsUCS2String = WSGetNumberAsUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 377
try:
    MLGetNumberAsUTF16String = WSGetNumberAsUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 378
try:
    MLGetNumberAsUTF32String = WSGetNumberAsUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 379
try:
    MLGetNumberAsUTF8String = WSGetNumberAsUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 380
try:
    MLGetRawArgCount = WSGetRawArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 381
try:
    MLGetRawData = WSGetRawData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 382
try:
    MLGetRawType = WSGetRawType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 383
try:
    MLGetReal = WSGetReal
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 384
try:
    MLGetReal128 = WSGetReal128
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 385
try:
    MLGetReal128Array = WSGetReal128Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 386
try:
    MLGetReal128ArrayData = WSGetReal128ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 387
try:
    MLGetReal128List = WSGetReal128List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 388
try:
    MLGetReal32 = WSGetReal32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 389
try:
    MLGetReal32Array = WSGetReal32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 390
try:
    MLGetReal32ArrayData = WSGetReal32ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 391
try:
    MLGetReal32List = WSGetReal32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 392
try:
    MLGetReal64 = WSGetReal64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 393
try:
    MLGetReal64Array = WSGetReal64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 394
try:
    MLGetReal64ArrayData = WSGetReal64ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 395
try:
    MLGetReal64List = WSGetReal64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 396
try:
    MLGetRealArray = WSGetRealArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 397
try:
    MLGetRealList = WSGetRealList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 398
try:
    MLGetShortInteger = WSGetShortInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 399
try:
    MLGetShortIntegerArray = WSGetShortIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 400
try:
    MLGetShortIntegerArrayData = WSGetShortIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 401
try:
    MLGetString = WSGetString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 402
try:
    MLGetSymbol = WSGetSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 403
try:
    MLGetType = WSGetType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 404
try:
    MLGetUCS2Characters = WSGetUCS2Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 405
try:
    MLGetUCS2Function = WSGetUCS2Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 406
try:
    MLGetUCS2String = WSGetUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 407
try:
    MLGetUCS2Symbol = WSGetUCS2Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 408
try:
    MLGetUTF16Characters = WSGetUTF16Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 409
try:
    MLGetUTF16Function = WSGetUTF16Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 410
try:
    MLGetUTF16String = WSGetUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 411
try:
    MLGetUTF16Symbol = WSGetUTF16Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 412
try:
    MLGetUTF32Characters = WSGetUTF32Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 413
try:
    MLGetUTF32Function = WSGetUTF32Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 414
try:
    MLGetUTF32String = WSGetUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 415
try:
    MLGetUTF32Symbol = WSGetUTF32Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 416
try:
    MLGetUTF8Characters = WSGetUTF8Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 417
try:
    MLGetUTF8Function = WSGetUTF8Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 418
try:
    MLGetUTF8String = WSGetUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 419
try:
    MLGetUTF8Symbol = WSGetUTF8Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 420
try:
    MLGetYieldFunction = WSGetYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 421
try:
    MLHandlerCast = WSHandlerCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 422
try:
    MLHandleSignal = WSHandleSignal
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 423
try:
    MLInit = WSInit
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 424
try:
    MLInitialize = WSInitialize
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 425
try:
    MLInterfaceFromLinkServer = WSInterfaceFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 426
try:
    MLIsLinkLoopback = WSIsLinkLoopback
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 427
try:
    MLinkEnvironment = WLinkEnvironment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 428
try:
    MLLinkEnvironment = WSLinkEnvironment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 429
try:
    MLLinkName = WSLinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 430
try:
    MLLogFileNameForLink = WSLogFileNameForLink
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 431
try:
    MLLogStreamToFile = WSLogStreamToFile
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 432
try:
    MLLoopbackOpen = WSLoopbackOpen
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 433
try:
    MLLowLevelDeviceName = WSLowLevelDeviceName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 434
try:
    MLMessageHandler = WSMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 435
try:
    MLMessageReady = WSMessageReady
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 436
try:
    MLName = WSName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 437
try:
    MLNewLinkServer = WSNewLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 438
try:
    MLNewLinkServerWithPort = WSNewLinkServerWithPort
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 439
try:
    MLNewLinkServerWithPortAndInterface = WSNewLinkServerWithPortAndInterface
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 440
try:
    MLNewPacket = WSNewPacket
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 441
try:
    MLNewParameters = WSNewParameters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 442
try:
    MLNewUnicodeContainer = WSNewUnicodeContainer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 443
try:
    MLNextCharacter = WSNextCharacter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 444
try:
    MLNextCharacterFromStringWithLength = WSNextCharacterFromStringWithLength
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 445
try:
    MLNextPacket = WSNextPacket
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 446
try:
    MLNumber = WSNumber
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 447
try:
    MLNumericsQuery = WSNumericsQuery
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 448
try:
    MLOldConvertByteString = WSOldConvertByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 449
try:
    MLOldConvertUCS2String = WSOldConvertUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 450
try:
    MLOldPutCharToString = WSOldPutCharToString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 451
try:
    MLOldStringCharFun = WSOldStringCharFun
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 452
try:
    MLOldStringFirstPosFun = WSOldStringFirstPosFun
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 453
try:
    MLOldStringNextPosFun = WSOldStringNextPosFun
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 454
try:
    MLOpen = WSOpen
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 455
try:
    MLOpenArgcArgv = WSOpenArgcArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 456
try:
    MLOpenArgv = WSOpenArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 457
try:
    MLOpenInEnv = WSOpenInEnv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 458
try:
    MLOpenString = WSOpenString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 459
try:
    MLPortFromLinkServer = WSPortFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 460
try:
    MLPrintArgv = WSPrintArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 461
try:
    MLPut7BitCharacters = WSPut7BitCharacters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 462
try:
    MLPut7BitCount = WSPut7BitCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 463
try:
    MLPut8BitCharacters = WSPut8BitCharacters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 464
try:
    MLPutArgCount = WSPutArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 465
try:
    MLPutArray = WSPutArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 466
try:
    MLPutArrayLeaves = WSPutArrayLeaves
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 467
try:
    MLPutArrayType = WSPutArrayType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 468
try:
    MLPutBinaryNumber = WSPutBinaryNumber
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 469
try:
    MLPutBinaryNumberArray = WSPutBinaryNumberArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 470
try:
    MLPutBinaryNumberArrayData = WSPutBinaryNumberArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 471
try:
    MLPutBinaryNumberArrayDataWithHeads = WSPutBinaryNumberArrayDataWithHeads
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 472
try:
    MLPutBinaryNumberList = WSPutBinaryNumberList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 473
try:
    MLPutByteArray = WSPutByteArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 474
try:
    MLPutByteArrayData = WSPutByteArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 475
try:
    MLPutByteString = WSPutByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 476
try:
    MLPutByteSymbol = WSPutByteSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 477
try:
    MLPutComposite = WSPutComposite
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 478
try:
    MLPutData = WSPutData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 479
try:
    MLPutDouble = WSPutDouble
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 480
try:
    MLPutDoubleArray = WSPutDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 481
try:
    MLPutDoubleArrayData = WSPutDoubleArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 482
try:
    MLPutFloat = WSPutFloat
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 483
try:
    MLPutFloatArray = WSPutFloatArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 484
try:
    MLPutFloatArrayData = WSPutFloatArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 485
try:
    MLPutFunction = WSPutFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 486
try:
    MLPutInteger = WSPutInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 487
try:
    MLPutInteger16 = WSPutInteger16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 488
try:
    MLPutInteger16Array = WSPutInteger16Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 489
try:
    MLPutInteger16ArrayData = WSPutInteger16ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 490
try:
    MLPutInteger16List = WSPutInteger16List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 491
try:
    MLPutInteger32 = WSPutInteger32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 492
try:
    MLPutInteger32Array = WSPutInteger32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 493
try:
    MLPutInteger32ArrayData = WSPutInteger32ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 494
try:
    MLPutInteger32List = WSPutInteger32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 495
try:
    MLPutInteger64 = WSPutInteger64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 496
try:
    MLPutInteger64Array = WSPutInteger64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 497
try:
    MLPutInteger64ArrayData = WSPutInteger64ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 498
try:
    MLPutInteger64List = WSPutInteger64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 499
try:
    MLPutInteger8 = WSPutInteger8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 500
try:
    MLPutInteger8Array = WSPutInteger8Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 501
try:
    MLPutInteger8ArrayData = WSPutInteger8ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 502
try:
    MLPutInteger8List = WSPutInteger8List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 503
try:
    MLPutIntegerArray = WSPutIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 504
try:
    MLPutIntegerArrayData = WSPutIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 505
try:
    MLPutIntegerList = WSPutIntegerList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 506
try:
    MLPutLongDouble = WSPutLongDouble
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 507
try:
    MLPutLongDoubleArray = WSPutLongDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 508
try:
    MLPutLongDoubleArrayData = WSPutLongDoubleArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 509
try:
    MLPutLongInteger = WSPutLongInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 510
try:
    MLPutLongIntegerArray = WSPutLongIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 511
try:
    MLPutLongIntegerArrayData = WSPutLongIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 512
try:
    MLPutMessage = WSPutMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 513
try:
    MLPutMessageWithArg = WSPutMessageWithArg
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 514
try:
    MLPutNext = WSPutNext
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 515
try:
    MLPutRawData = WSPutRawData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 516
try:
    MLPutRawSize = WSPutRawSize
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 517
try:
    MLPutReal = WSPutReal
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 518
try:
    MLPutReal128 = WSPutReal128
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 519
try:
    MLPutReal128Array = WSPutReal128Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 520
try:
    MLPutReal128ArrayData = WSPutReal128ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 521
try:
    MLPutReal128List = WSPutReal128List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 522
try:
    MLPutReal32 = WSPutReal32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 523
try:
    MLPutReal32Array = WSPutReal32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 524
try:
    MLPutReal32ArrayData = WSPutReal32ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 525
try:
    MLPutReal32List = WSPutReal32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 526
try:
    MLPutReal64 = WSPutReal64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 527
try:
    MLPutReal64Array = WSPutReal64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 528
try:
    MLPutReal64ArrayData = WSPutReal64ArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 529
try:
    MLPutReal64List = WSPutReal64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 530
try:
    MLPutRealArray = WSPutRealArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 531
try:
    MLPutRealList = WSPutRealList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 532
try:
    MLPutRealNumberAsByteString = WSPutRealNumberAsByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 533
try:
    MLPutRealNumberAsString = WSPutRealNumberAsString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 534
try:
    MLPutRealNumberAsUCS2String = WSPutRealNumberAsUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 535
try:
    MLPutRealNumberAsUTF16String = WSPutRealNumberAsUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 536
try:
    MLPutRealNumberAsUTF32String = WSPutRealNumberAsUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 537
try:
    MLPutRealNumberAsUTF8String = WSPutRealNumberAsUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 538
try:
    MLPutShortInteger = WSPutShortInteger
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 539
try:
    MLPutShortIntegerArray = WSPutShortIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 540
try:
    MLPutShortIntegerArrayData = WSPutShortIntegerArrayData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 541
try:
    MLPutSize = WSPutSize
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 542
try:
    MLPutString = WSPutString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 543
try:
    MLPutSymbol = WSPutSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 544
try:
    MLPutType = WSPutType
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 545
try:
    MLPutUCS2Characters = WSPutUCS2Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 546
try:
    MLPutUCS2Function = WSPutUCS2Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 547
try:
    MLPutUCS2String = WSPutUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 548
try:
    MLPutUCS2Symbol = WSPutUCS2Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 549
try:
    MLPutUTF16Characters = WSPutUTF16Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 550
try:
    MLPutUTF16Function = WSPutUTF16Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 551
try:
    MLPutUTF16String = WSPutUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 552
try:
    MLPutUTF16Symbol = WSPutUTF16Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 553
try:
    MLPutUTF32Characters = WSPutUTF32Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 554
try:
    MLPutUTF32Function = WSPutUTF32Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 555
try:
    MLPutUTF32String = WSPutUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 556
try:
    MLPutUTF32Symbol = WSPutUTF32Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 557
try:
    MLPutUTF8Characters = WSPutUTF8Characters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 558
try:
    MLPutUTF8Function = WSPutUTF8Function
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 559
try:
    MLPutUTF8String = WSPutUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 560
try:
    MLPutUTF8Symbol = WSPutUTF8Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 561
try:
    MLRawBytesToGet = WSRawBytesToGet
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 562
try:
    MLReady = WSReady
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 563
try:
    MLReadyParallel = WSReadyParallel
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 564
try:
    MLRegisterCallbackFunctionWithLinkServer = WSRegisterCallbackFunctionWithLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 565
try:
    MLRegisterLinkService = WSRegisterLinkService
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 566
try:
    MLRegisterLinkServiceFromLinkServer = WSRegisterLinkServiceFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 567
try:
    MLRegisterLinkServiceWithHostname = WSRegisterLinkServiceWithHostname
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 568
try:
    MLRegisterLinkServiceWithPortAndHostname = WSRegisterLinkServiceWithPortAndHostname
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 569
try:
    MLReleaseBinaryNumberArray = WSReleaseBinaryNumberArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 570
try:
    MLReleaseBinaryNumberList = WSReleaseBinaryNumberList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 571
try:
    MLReleaseByteArray = WSReleaseByteArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 572
try:
    MLReleaseByteString = WSReleaseByteString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 573
try:
    MLReleaseByteSymbol = WSReleaseByteSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 574
try:
    MLReleaseCompilerID = WSReleaseCompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 575
try:
    MLReleaseDomainNameList = WSReleaseDomainNameList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 576
try:
    MLReleaseDoubleArray = WSReleaseDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 577
try:
    MLReleaseEnvIDString = WSReleaseEnvIDString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 578
try:
    MLReleaseErrorMessage = WSReleaseErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 579
try:
    MLReleaseFloatArray = WSReleaseFloatArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 580
try:
    MLReleaseGetArrayState = WSReleaseGetArrayState
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 581
try:
    MLReleaseInteger16Array = WSReleaseInteger16Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 582
try:
    MLReleaseInteger16List = WSReleaseInteger16List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 583
try:
    MLReleaseInteger32Array = WSReleaseInteger32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 584
try:
    MLReleaseInteger32List = WSReleaseInteger32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 585
try:
    MLReleaseInteger64Array = WSReleaseInteger64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 586
try:
    MLReleaseInteger64List = WSReleaseInteger64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 587
try:
    MLReleaseInteger8Array = WSReleaseInteger8Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 588
try:
    MLReleaseInteger8List = WSReleaseInteger8List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 589
try:
    MLReleaseIntegerArray = WSReleaseIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 590
try:
    MLReleaseIntegerList = WSReleaseIntegerList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 591
try:
    MLReleaseInterfaceFromLinkServer = WSReleaseInterfaceFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 592
try:
    MLReleaseLinkName = WSReleaseLinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 593
try:
    MLReleaseLinkProtocolNames = WSReleaseLinkProtocolNames
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 594
try:
    MLReleaseLinksFromEnvironment = WSReleaseLinksFromEnvironment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 595
try:
    MLReleaseLogFileNameForLink = WSReleaseLogFileNameForLink
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 596
try:
    MLReleaseLongDoubleArray = WSReleaseLongDoubleArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 597
try:
    MLReleaseLongIntegerArray = WSReleaseLongIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 598
try:
    MLReleaseLowLevelDeviceName = WSReleaseLowLevelDeviceName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 599
try:
    MLReleaseNetworkAddressList = WSReleaseNetworkAddressList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 600
try:
    MLReleaseParameters = WSReleaseParameters
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 601
try:
    MLReleasePutArrayState = WSReleasePutArrayState
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 602
try:
    MLReleaseReal128Array = WSReleaseReal128Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 603
try:
    MLReleaseReal128List = WSReleaseReal128List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 604
try:
    MLReleaseReal32Array = WSReleaseReal32Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 605
try:
    MLReleaseReal32List = WSReleaseReal32List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 606
try:
    MLReleaseReal64Array = WSReleaseReal64Array
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 607
try:
    MLReleaseReal64List = WSReleaseReal64List
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 608
try:
    MLReleaseRealArray = WSReleaseRealArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 609
try:
    MLReleaseRealList = WSReleaseRealList
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 610
try:
    MLReleaseShortIntegerArray = WSReleaseShortIntegerArray
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 611
try:
    MLReleaseString = WSReleaseString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 612
try:
    MLReleaseSymbol = WSReleaseSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 613
try:
    MLReleaseUCS2CompilerID = WSReleaseUCS2CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 614
try:
    MLReleaseUCS2ErrorMessage = WSReleaseUCS2ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 615
try:
    MLReleaseUCS2LinkName = WSReleaseUCS2LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 616
try:
    MLReleaseUCS2String = WSReleaseUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 617
try:
    MLReleaseUCS2Symbol = WSReleaseUCS2Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 618
try:
    MLReleaseUnicodeContainer = WSReleaseUnicodeContainer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 619
try:
    MLReleaseUTF16CompilerID = WSReleaseUTF16CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 620
try:
    MLReleaseUTF16ErrorMessage = WSReleaseUTF16ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 621
try:
    MLReleaseUTF16LinkName = WSReleaseUTF16LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 622
try:
    MLReleaseUTF16String = WSReleaseUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 623
try:
    MLReleaseUTF16Symbol = WSReleaseUTF16Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 624
try:
    MLReleaseUTF32CompilerID = WSReleaseUTF32CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 625
try:
    MLReleaseUTF32ErrorMessage = WSReleaseUTF32ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 626
try:
    MLReleaseUTF32LinkName = WSReleaseUTF32LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 627
try:
    MLReleaseUTF32String = WSReleaseUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 628
try:
    MLReleaseUTF32Symbol = WSReleaseUTF32Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 629
try:
    MLReleaseUTF8CompilerID = WSReleaseUTF8CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 630
try:
    MLReleaseUTF8ErrorMessage = WSReleaseUTF8ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 631
try:
    MLReleaseUTF8LinkName = WSReleaseUTF8LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 632
try:
    MLReleaseUTF8String = WSReleaseUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 633
try:
    MLReleaseUTF8Symbol = WSReleaseUTF8Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 634
try:
    MLRequest = WSRequest
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 635
try:
    MLRequestArgv = WSRequestArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 636
try:
    MLRequestArgvCast = WSRequestArgvCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 637
try:
    MLRequestCast = WSRequestCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 638
try:
    MLRequestToInteract = WSRequestToInteract
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 639
try:
    MLRequestToInteractCast = WSRequestToInteractCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 640
try:
    MLResolveLinkService = WSResolveLinkService
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 641
try:
    MLScanString = WSScanString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 642
try:
    MLSeekMark = WSSeekMark
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 643
try:
    MLSeekToMark = WSSeekToMark
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 644
try:
    MLServiceProtocolFromReference = WSServiceProtocolFromReference
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 645
try:
    MLSetAllocParameter = WSSetAllocParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 646
try:
    MLSetDefaultYieldFunction = WSSetDefaultYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 647
try:
    MLSetDeviceParameter = WSSetDeviceParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 648
try:
    MLSetDialogFunction = WSSetDialogFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 649
try:
    MLSetEncodingParameter = WSSetEncodingParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 650
try:
    MLSetEnvIDString = WSSetEnvIDString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 651
try:
    MLSetEnvironmentData = WSSetEnvironmentData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 652
try:
    MLSetError = WSSetError
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 653
try:
    MLSetMessageHandler = WSSetMessageHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 654
try:
    MLSetName = WSSetName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 655
try:
    MLSetResourceParameter = WSSetResourceParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 656
try:
    MLSetSignalHandler = WSSetSignalHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 657
try:
    MLSetSignalHandlerFromFunction = WSSetSignalHandlerFromFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 658
try:
    MLSetSymbolReplacement = WSSetSymbolReplacement
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 659
try:
    MLSetThreadSafeLinksParameter = WSSetThreadSafeLinksParameter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 660
try:
    MLSetUserBlock = WSSetUserBlock
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 661
try:
    MLSetUserData = WSSetUserData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 662
try:
    MLSetYieldFunction = WSSetYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 663
try:
    MLShutdownLinkServer = WSShutdownLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 664
try:
    MLSleepYP = WSSleepYP
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 665
try:
    MLStopBrowsingForLinkServices = WSStopBrowsingForLinkServices
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 666
try:
    MLStopHandlingSignal = WSStopHandlingSignal
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 667
try:
    MLStopLoggingStream = WSStopLoggingStream
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 668
try:
    MLStopLoggingStreamToFile = WSStopLoggingStreamToFile
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 669
try:
    MLStopRegisteringLinkService = WSStopRegisteringLinkService
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 670
try:
    MLStopResolvingLinkService = WSStopResolvingLinkService
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 671
try:
    MLStopRegisteringLinkServiceForLink = WSStopRegisteringLinkServiceForLink
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 672
try:
    MLStringCharacter = WSStringCharacter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 673
try:
    MLStringFirstPosFun = WSStringFirstPosFun
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 674
try:
    MLStringToArgv = WSStringToArgv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 675
try:
    MLTakeLast = WSTakeLast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 676
try:
    MLTestHead = WSTestHead
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 677
try:
    MLTestHeadWithArgCount = WSTestHeadWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 678
try:
    MLTestString = WSTestString
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 679
try:
    MLTestSymbol = WSTestSymbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 680
try:
    MLTestUCS2Head = WSTestUCS2Head
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 681
try:
    MLTestUCS2HeadWithArgCount = WSTestUCS2HeadWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 682
try:
    MLTestUCS2String = WSTestUCS2String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 683
try:
    MLTestUCS2Symbol = WSTestUCS2Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 684
try:
    MLTestUTF16Head = WSTestUTF16Head
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 685
try:
    MLTestUTF16HeadWithArgCount = WSTestUTF16HeadWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 686
try:
    MLTestUTF16String = WSTestUTF16String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 687
try:
    MLTestUTF16Symbol = WSTestUTF16Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 688
try:
    MLTestUTF32Head = WSTestUTF32Head
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 689
try:
    MLTestUTF32HeadWithArgCount = WSTestUTF32HeadWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 690
try:
    MLTestUTF32String = WSTestUTF32String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 691
try:
    MLTestUTF32Symbol = WSTestUTF32Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 692
try:
    MLTestUTF8Head = WSTestUTF8Head
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 693
try:
    MLTestUTF8HeadWithArgCount = WSTestUTF8HeadWithArgCount
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 694
try:
    MLTestUTF8String = WSTestUTF8String
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 695
try:
    MLTestUTF8Symbol = WSTestUTF8Symbol
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 696
try:
    MLToLinkID = WSToLinkID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 697
try:
    MLTransferExpression = WSTransferExpression
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 698
try:
    MLTransferToEndOfLoopbackLink = WSTransferToEndOfLoopbackLink
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 699
try:
    MLUCS2CompilerID = WSUCS2CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 700
try:
    MLUCS2ErrorMessage = WSUCS2ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 701
try:
    MLUCS2LinkName = WSUCS2LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 702
try:
    MLUTF16CompilerID = WSUTF16CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 703
try:
    MLUTF16ErrorMessage = WSUTF16ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 704
try:
    MLUTF16LinkName = WSUTF16LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 705
try:
    MLUTF32CompilerID = WSUTF32CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 706
try:
    MLUTF32ErrorMessage = WSUTF32ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 707
try:
    MLUTF32LinkName = WSUTF32LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 708
try:
    MLUTF8CompilerID = WSUTF8CompilerID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 709
try:
    MLUTF8ErrorMessage = WSUTF8ErrorMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 710
try:
    MLUTF8LinkName = WSUTF8LinkName
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 711
try:
    MLUnsetSignalHandler = WSUnsetSignalHandler
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 712
try:
    MLUserBlock = WSUserBlock
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 713
try:
    MLUserCast = WSUserCast
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 714
try:
    MLUserData = WSUserData
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 715
try:
    MLValid = WSValid
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 716
try:
    MLVersionNumbers = WSVersionNumbers
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 717
try:
    MLWaitForLinkActivity = WSWaitForLinkActivity
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 718
try:
    MLWaitForLinkActivityWithCallback = WSWaitForLinkActivityWithCallback
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 719
try:
    MLWaitForNewLinkFromLinkServer = WSWaitForNewLinkFromLinkServer
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 720
try:
    MLYieldFunction = WSYieldFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 721
try:
    MLYielderCast = WSYielderCast
except:
    pass

MLPointer = WSPointer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 728

MLENV = WSENV# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 729

MLEnvironment = WSEnvironment# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 730

MLEnvironmentParameter = WSEnvironmentParameter# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 731

MLINK = WSLINK# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 732

MLMARK = WSMARK# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 733

MLMessageHandlerObject = WSMessageHandlerObject# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 740

mlint64 = wsint64# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 741

mlextended_double = wsextended_double# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 742

MLMessageHandlerType = WSMessageHandlerType# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 757

MLLinkWaitCallBackObject = WSLinkWaitCallBackObject# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 764

MLServiceRef = WSServiceRef# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 765

MLSignalHandlerType = WSSignalHandlerType# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 766

MLUserFunction = WSUserFunction# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 767

MLUserFunctionType = WSUserFunctionType# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 768

MLUserFunctionTypePointer = WSUserFunctionTypePointer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 769

MLYieldFunctionObject = WSYieldFunctionObject# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 770

MLYieldParameters = WSYieldParameters# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 771

MLAllocator = WSAllocator# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 774

MLAllocatorp = WSAllocatorp# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 775

MLDeallocator = WSDeallocator# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 776

MLDeallocatorp = WSDeallocatorp# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 777

MLLinkServer = WSLinkServer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 778

MLNewLinkCallbackFunction = WSNewLinkCallbackFunction# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 779

MLOldStringPositionPointer = WSOldStringPositionPointer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 780

MLUnicodeContainer = WSUnicodeContainer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 781

MLAllocatorProcPtr = WSAllocatorProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 782

MLDeallocatorProcPtr = WSDeallocatorProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 783

MLHandlerProcPtr = WSHandlerProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 784

MLUserProcPtr = WSUserProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 785

MLYielderProcPtr = WSYielderProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 786

MLYieldFunctionType = WSYieldFunctionType# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 787

MLDialogProcPtr = WSDialogProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 789

MLAlertProcPtr = WSAlertProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 790

MLRequestProcPtr = WSRequestProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 791

MLConfirmProcPtr = WSConfirmProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 792

MLRequestArgvProcPtr = WSRequestArgvProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 793

MLRequestToInteractProcPtr = WSRequestToInteractProcPtr# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 794

mltimeval = wstimeval# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 800

MLParametersPointer = WSParametersPointer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 801

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 808
try:
    MLTerminateMessage = WSTerminateMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 809
try:
    MLInterruptMessage = WSInterruptMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 810
try:
    MLAbortMessage = WSAbortMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 811
try:
    MLEndPacketMessage = WSEndPacketMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 812
try:
    MLSynchronizeMessage = WSSynchronizeMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 813
try:
    MLImDyingMessage = WSImDyingMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 814
try:
    MLWaitingAcknowledgment = WSWaitingAcknowledgment
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 815
try:
    MLMarkTopLevelMessage = WSMarkTopLevelMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 816
try:
    MLLinkClosingMessage = WSLinkClosingMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 817
try:
    MLAuthenticateFailure = WSAuthenticateFailure
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 818
try:
    MLSuspendActivitiesMessage = WSSuspendActivitiesMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 819
try:
    MLResumeActivitiesMessage = WSResumeActivitiesMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 820
try:
    MLFirstUserMessage = WSFirstUserMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 821
try:
    MLLastUserMessage = WSLastUserMessage
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 824
try:
    MLAlertFunction = WSAlertFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 825
try:
    MLRequestFunction = WSRequestFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 826
try:
    MLConfirmFunction = WSConfirmFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 827
try:
    MLRequestArgvFunction = WSRequestArgvFunction
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 828
try:
    MLRequestToInteractFunction = WSRequestToInteractFunction
except:
    pass

MLDialogFunctionType = WSDialogFunctionType# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 829

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 841
try:
    WSREADYPARALLELERROR = (-1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 842
try:
    WSREADYPARALLELTIMEDOUT = (-2)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 843
try:
    WSREADYPARALLELINVALIDARGUMENT = (-3)
except:
    pass

MLREADYPARALLELENV = WSREADYPARALLELENV# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 845

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 847
try:
    WSWAITSUCCESS = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 848
try:
    WSWAITERROR = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 849
try:
    WSWAITTIMEOUT = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 850
try:
    WSWAITCALLBACKABORTED = 4
except:
    pass

mlapi_packet = wsapi_packet# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 858

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 861
try:
    MLINTERFACE = WSINTERFACE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 920
try:
    ML64BIT_MATHLINK = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 928
try:
    ML_SMALLEST_SIGNED_64BIT = ((-9223372036854775807) - 1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 929
try:
    ML_LARGEST_SIGNED_64BIT = 9223372036854775807
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 930
try:
    ML_LARGEST_UNSIGNED_64BIT = 18446744073709551615
except:
    pass

__uint_ct__ = c_uint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1274

__int_ct__ = c_int# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1277

__mlapi_token__ = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1344

__mlapi_packet__ = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1353

__mlapi_result__ = int_ct# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1369

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1374
try:
    MLSUCCESS = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1375
try:
    MLFAILURE = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1402
try:
    MLDEV_WRITE_WINDOW = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1403
try:
    MLDEV_WRITE = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1404
try:
    MLDEV_HAS_DATA = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1405
try:
    MLDEV_READ = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1406
try:
    MLDEV_READ_COMPLETE = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1407
try:
    MLDEV_ACKNOWLEDGE = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1409
try:
    T_DEV_WRITE_WINDOW = MLDEV_WRITE_WINDOW
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1410
try:
    T_DEV_WRITE = MLDEV_WRITE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1411
try:
    T_DEV_HAS_DATA = MLDEV_HAS_DATA
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1412
try:
    T_DEV_READ = MLDEV_READ
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1413
try:
    T_DEV_READ_COMPLETE = MLDEV_READ_COMPLETE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1428
def CallMLDeviceProc(userRoutine, thing, selector, p1, p2):
    return ((userRoutine[0]) (thing, selector, (dev_voidp (ord_if_char(p1))).value, (dev_voidp (ord_if_char(p2))).value))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1429
def NewMLDeviceProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1470
def CallMLAllocatorProc(userRoutine, size):
    return ((userRoutine[0]) (size))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1471
def NewMLAllocatorProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1479
def CallMLDeallocatorProc(userRoutine, p):
    return ((userRoutine[0]) (p))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1480
def NewMLDeallocatorProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1499
try:
    MLCallAllocator = CallMLAllocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1500
try:
    MLNewAllocator = NewMLAllocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1504
try:
    MLCallDeallocator = CallMLDeallocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1505
try:
    MLNewDeallocator = NewMLDeallocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1534
try:
    REALBIT = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1535
try:
    REAL_MASK = (1 << REALBIT)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1536
try:
    XDRBIT = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1537
try:
    XDR_MASK = (1 << XDRBIT)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1538
try:
    BINARYBIT = 7
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1539
try:
    BINARY_MASK = (1 << BINARYBIT)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1540
try:
    SIZEVARIANTBIT = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1541
try:
    SIZEVARIANT_MASK = (1 << SIZEVARIANTBIT)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1544
try:
    WSTK_INVALID = 155
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1553
try:
    WSTK_8BIT_SIGNED_2sCOMPLEMENT_INTEGER = 160
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1556
try:
    WSTK_8BIT_UNSIGNED_2sCOMPLEMENT_INTEGER = 161
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1557
try:
    WSTK_8BIT_UNSIGNED_INTEGER = WSTK_8BIT_UNSIGNED_2sCOMPLEMENT_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1561
try:
    WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 162
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1564
try:
    WSTK_16BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 163
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1565
try:
    WSTK_16BIT_UNSIGNED_BIGENDIAN_INTEGER = WSTK_16BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1568
try:
    WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 164
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1571
try:
    WSTK_32BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 165
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1572
try:
    WSTK_32BIT_UNSIGNED_BIGENDIAN_INTEGER = WSTK_32BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1575
try:
    WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 166
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1578
try:
    WSTK_64BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER = 167
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1579
try:
    WSTK_64BIT_UNSIGNED_BIGENDIAN_INTEGER = WSTK_64BIT_UNSIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1584
try:
    WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 226
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1587
try:
    WSTK_16BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 227
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1588
try:
    WSTK_16BIT_UNSIGNED_LITTLEENDIAN_INTEGER = WSTK_16BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1591
try:
    WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 228
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1594
try:
    WSTK_32BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 229
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1595
try:
    WSTK_32BIT_UNSIGNED_LITTLEENDIAN_INTEGER = WSTK_32BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1598
try:
    WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 230
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1601
try:
    WSTK_64BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER = 231
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1602
try:
    WSTK_64BIT_UNSIGNED_LITTLEENDIAN_INTEGER = WSTK_64BIT_UNSIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1606
try:
    WSTK_BIGENDIAN_IEEE754_SINGLE = 180
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1609
try:
    WSTK_BIGENDIAN_IEEE754_DOUBLE = 182
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1612
try:
    WSTK_BIGENDIAN_128BIT_DOUBLE = 184
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1616
try:
    WSTK_LITTLEENDIAN_IEEE754_SINGLE = 244
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1619
try:
    WSTK_LITTLEENDIAN_IEEE754_DOUBLE = 246
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1622
try:
    WSTK_LITTLEENDIAN_128BIT_DOUBLE = 248
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1636
def WSNE__SELECTOR(dtok, stok):
    return ((dtok << 8) | stok)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1639
def WSNE__SIZESELECTOR(tok):
    return (WSNE__SELECTOR (0, tok))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1640
try:
    WSNE__INITSELECTOR = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1641
def WSNE__TOSTRINGSELECTOR(tok):
    return (WSNE__SELECTOR ((WSNE__IS_REAL (tok)) and WSTKREAL or WSTKINT, tok))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1642
def WSNE__FROMSTRINGSELECTOR(dtok, stok):
    return (WSNE__SELECTOR (dtok, stok))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1644
def WSNE__STOK(selector):
    return (selector & 0x000000FF)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1645
def WSNE__DTOK(selector):
    return ((selector & 0x0000FF00) >> 8)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1647
def WSNE__IS_BINARY(tok):
    return (tok & BINARY_MASK)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1648
def WSNE__IS_REAL(tok):
    return (tok & REAL_MASK)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1649
def WSNE__TEXT_TOKEN(tok):
    return (WSNE__IS_REAL (tok)) and WSTKREAL or WSTKINT

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1675
try:
    WSTK_CSHORT_P = ((BINARY_MASK | SIZEVARIANT_MASK) | 1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1676
try:
    WSTK_CINT_P = ((BINARY_MASK | SIZEVARIANT_MASK) | 2)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1677
try:
    WSTK_CLONG_P = ((BINARY_MASK | SIZEVARIANT_MASK) | 3)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1678
try:
    WSTK_CFLOAT_P = (((BINARY_MASK | SIZEVARIANT_MASK) | REAL_MASK) | 1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1679
try:
    WSTK_CDOUBLE_P = (((BINARY_MASK | SIZEVARIANT_MASK) | REAL_MASK) | 2)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1680
try:
    WSTK_CLONGDOUBLE_P = (((BINARY_MASK | SIZEVARIANT_MASK) | REAL_MASK) | 3)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1683
try:
    WSTK_64BIT_LITTLEENDIAN_STRUCTURE = 196
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1684
try:
    WSTK_64BIT_BIGENDIAN_STRUCTURE = 197
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1687
try:
    WSTK_128BIT_EXTENDED = 158
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1688
try:
    WSTK_128BIT_LONGDOUBLE = 158
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1693
try:
    WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED = 218
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1697
try:
    WSTK_INTEL_80BIT_EXTENDED = 216
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1700
try:
    WSMASTIFF_NUMERICS_ID = 'mastiff'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1701
try:
    WSMASTIFF_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1702
try:
    WSMASTIFF_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1703
try:
    WSMASTIFF_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1704
try:
    WSMASTIFF_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1705
try:
    WSMASTIFF_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1706
try:
    WSMASTIFF_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1707
try:
    WSMASTIFF_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1708
try:
    WSMASTIFF_CLONGDOUBLE = WSTK_128BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1709
try:
    WSMASTIFF_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1710
try:
    WSMASTIFF_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1711
try:
    WSMASTIFF_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1712
try:
    WSMASTIFF_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1713
try:
    WSMASTIFF_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1714
try:
    WSMASTIFF_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1715
try:
    WSMASTIFF_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1716
try:
    WSMASTIFF_WSLONGDOUBLE = WSTK_128BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1719
try:
    WSJAPANESECHIN_NUMERICS_ID = 'japanesechin'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1720
try:
    WSJAPANESECHIN_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1721
try:
    WSJAPANESECHIN_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1722
try:
    WSJAPANESECHIN_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1723
try:
    WSJAPANESECHIN_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1724
try:
    WSJAPANESECHIN_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1725
try:
    WSJAPANESECHIN_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1726
try:
    WSJAPANESECHIN_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1727
try:
    WSJAPANESECHIN_CLONGDOUBLE = WSTK_128BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1728
try:
    WSJAPANESECHIN_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1729
try:
    WSJAPANESECHIN_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1730
try:
    WSJAPANESECHIN_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1731
try:
    WSJAPANESECHIN_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1732
try:
    WSJAPANESECHIN_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1733
try:
    WSJAPANESECHIN_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1734
try:
    WSJAPANESECHIN_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1735
try:
    WSJAPANESECHIN_WSLONGDOUBLE = WSTK_128BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1741
try:
    WSBORZOI_NUMERICS_ID = 'borzoi'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1742
try:
    WSBORZOI_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1743
try:
    WSBORZOI_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1744
try:
    WSBORZOI_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1745
try:
    WSBORZOI_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1746
try:
    WSBORZOI_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1747
try:
    WSBORZOI_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1748
try:
    WSBORZOI_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1749
try:
    WSBORZOI_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1750
try:
    WSBORZOI_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1751
try:
    WSBORZOI_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1752
try:
    WSBORZOI_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1753
try:
    WSBORZOI_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1754
try:
    WSBORZOI_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1755
try:
    WSBORZOI_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1761
try:
    WSBRIARD_NUMERICS_ID = 'briard'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1762
try:
    WSBRIARD_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1763
try:
    WSBRIARD_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1764
try:
    WSBRIARD_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1765
try:
    WSBRIARD_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1766
try:
    WSBRIARD_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1767
try:
    WSBRIARD_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1768
try:
    WSBRIARD_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1769
try:
    WSBRIARD_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1770
try:
    WSBRIARD_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1771
try:
    WSBRIARD_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1772
try:
    WSBRIARD_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1773
try:
    WSBRIARD_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1774
try:
    WSBRIARD_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1775
try:
    WSBRIARD_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1778
try:
    WSKEESHOND_NUMERICS_ID = 'keeshond'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1779
try:
    WSKEESHOND_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1780
try:
    WSKEESHOND_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1781
try:
    WSKEESHOND_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1782
try:
    WSKEESHOND_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1783
try:
    WSKEESHOND_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1784
try:
    WSKEESHOND_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1785
try:
    WSKEESHOND_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1786
try:
    WSKEESHOND_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1787
try:
    WSKEESHOND_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1788
try:
    WSKEESHOND_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1789
try:
    WSKEESHOND_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1790
try:
    WSKEESHOND_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1791
try:
    WSKEESHOND_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1792
try:
    WSKEESHOND_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1793
try:
    WSKEESHOND_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1794
try:
    WSKEESHOND_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1797
try:
    WSKOMONDOR_NUMERICS_ID = 'komondor'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1798
try:
    WSKOMONDOR_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1799
try:
    WSKOMONDOR_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1800
try:
    WSKOMONDOR_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1801
try:
    WSKOMONDOR_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1802
try:
    WSKOMONDOR_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1803
try:
    WSKOMONDOR_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1804
try:
    WSKOMONDOR_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1805
try:
    WSKOMONDOR_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1806
try:
    WSKOMONDOR_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1807
try:
    WSKOMONDOR_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1808
try:
    WSKOMONDOR_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1809
try:
    WSKOMONDOR_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1810
try:
    WSKOMONDOR_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1811
try:
    WSKOMONDOR_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1812
try:
    WSKOMONDOR_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1813
try:
    WSKOMONDOR_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1816
try:
    WSNORWEGIANELKHOUND_NUMERICS_ID = 'norwegianelkhound'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1817
try:
    WSNORWEGIANELKHOUND_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1818
try:
    WSNORWEGIANELKHOUND_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1819
try:
    WSNORWEGIANELKHOUND_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1820
try:
    WSNORWEGIANELKHOUND_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1821
try:
    WSNORWEGIANELKHOUND_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1822
try:
    WSNORWEGIANELKHOUND_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1823
try:
    WSNORWEGIANELKHOUND_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1824
try:
    WSNORWEGIANELKHOUND_CLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1825
try:
    WSNORWEGIANELKHOUND_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1826
try:
    WSNORWEGIANELKHOUND_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1827
try:
    WSNORWEGIANELKHOUND_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1828
try:
    WSNORWEGIANELKHOUND_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1829
try:
    WSNORWEGIANELKHOUND_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1830
try:
    WSNORWEGIANELKHOUND_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1831
try:
    WSNORWEGIANELKHOUND_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1832
try:
    WSNORWEGIANELKHOUND_WSLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1835
try:
    WSNORWICHTERRIOR_NUMERICS_ID = 'norwichterrior'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1836
try:
    WSNORWICHTERRIOR_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1837
try:
    WSNORWICHTERRIOR_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1838
try:
    WSNORWICHTERRIOR_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1839
try:
    WSNORWICHTERRIOR_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1840
try:
    WSNORWICHTERRIOR_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1841
try:
    WSNORWICHTERRIOR_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1842
try:
    WSNORWICHTERRIOR_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1843
try:
    WSNORWICHTERRIOR_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1844
try:
    WSNORWICHTERRIOR_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1845
try:
    WSNORWICHTERRIOR_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1846
try:
    WSNORWICHTERRIOR_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1847
try:
    WSNORWICHTERRIOR_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1848
try:
    WSNORWICHTERRIOR_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1849
try:
    WSNORWICHTERRIOR_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1850
try:
    WSNORWICHTERRIOR_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1851
try:
    WSNORWICHTERRIOR_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1854
try:
    WSSAINTBERNARD_NUMERICS_ID = 'saintbernarnd'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1855
try:
    WSSAINTBERNARD_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1856
try:
    WSSAINTBERNARD_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1857
try:
    WSSAINTBERNARD_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1858
try:
    WSSAINTBERNARD_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1859
try:
    WSSAINTBERNARD_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1860
try:
    WSSAINTBERNARD_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1861
try:
    WSSAINTBERNARD_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1862
try:
    WSSAINTBERNARD_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1863
try:
    WSSAINTBERNARD_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1864
try:
    WSSAINTBERNARD_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1865
try:
    WSSAINTBERNARD_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1866
try:
    WSSAINTBERNARD_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1867
try:
    WSSAINTBERNARD_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1868
try:
    WSSAINTBERNARD_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1869
try:
    WSSAINTBERNARD_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1870
try:
    WSSAINTBERNARD_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1873
try:
    WSBERNESEMOUNTAINDOG_NUMERICS_ID = 'bernesemountaindog'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1874
try:
    WSBERNESEMOUNTAINDOG_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1875
try:
    WSBERNESEMOUNTAINDOG_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1876
try:
    WSBERNESEMOUNTAINDOG_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1877
try:
    WSBERNESEMOUNTAINDOG_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1878
try:
    WSBERNESEMOUNTAINDOG_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1879
try:
    WSBERNESEMOUNTAINDOG_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1880
try:
    WSBERNESEMOUNTAINDOG_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1881
try:
    WSBERNESEMOUNTAINDOG_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1882
try:
    WSBERNESEMOUNTAINDOG_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1883
try:
    WSBERNESEMOUNTAINDOG_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1884
try:
    WSBERNESEMOUNTAINDOG_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1885
try:
    WSBERNESEMOUNTAINDOG_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1886
try:
    WSBERNESEMOUNTAINDOG_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1887
try:
    WSBERNESEMOUNTAINDOG_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1888
try:
    WSBERNESEMOUNTAINDOG_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1889
try:
    WSBERNESEMOUNTAINDOG_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1892
try:
    WSSETTER_NUMERICS_ID = 'setter'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1893
try:
    WSSETTER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1894
try:
    WSSETTER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1895
try:
    WSSETTER_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1896
try:
    WSSETTER_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1897
try:
    WSSETTER_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1898
try:
    WSSETTER_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1899
try:
    WSSETTER_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1900
try:
    WSSETTER_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1901
try:
    WSSETTER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1902
try:
    WSSETTER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1903
try:
    WSSETTER_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1904
try:
    WSSETTER_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1905
try:
    WSSETTER_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1906
try:
    WSSETTER_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1907
try:
    WSSETTER_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1908
try:
    WSSETTER_WSLONGDOUBLE = WSTK_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1911
try:
    WSFRENCH_BULLDOG_NUMERICS_ID = 'french_bulldog'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1912
try:
    WSFRENCH_BULLDOG_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1913
try:
    WSFRENCH_BULLDOG_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1914
try:
    WSFRENCH_BULLDOG_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1915
try:
    WSFRENCH_BULLDOG_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1916
try:
    WSFRENCH_BULLDOG_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1917
try:
    WSFRENCH_BULLDOG_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1918
try:
    WSFRENCH_BULLDOG_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1919
try:
    WSFRENCH_BULLDOG_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1920
try:
    WSFRENCH_BULLDOG_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1921
try:
    WSFRENCH_BULLDOG_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1922
try:
    WSFRENCH_BULLDOG_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1923
try:
    WSFRENCH_BULLDOG_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1924
try:
    WSFRENCH_BULLDOG_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1925
try:
    WSFRENCH_BULLDOG_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1926
try:
    WSFRENCH_BULLDOG_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1927
try:
    WSFRENCH_BULLDOG_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1930
try:
    WSBICHON_FRISE_NUMERICS_ID = 'bichon_frise'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1931
try:
    WSBICHON_FRISE_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1932
try:
    WSBICHON_FRISE_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1933
try:
    WSBICHON_FRISE_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1934
try:
    WSBICHON_FRISE_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1935
try:
    WSBICHON_FRISE_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1936
try:
    WSBICHON_FRISE_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1937
try:
    WSBICHON_FRISE_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1938
try:
    WSBICHON_FRISE_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1939
try:
    WSBICHON_FRISE_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1940
try:
    WSBICHON_FRISE_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1941
try:
    WSBICHON_FRISE_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1942
try:
    WSBICHON_FRISE_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1943
try:
    WSBICHON_FRISE_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1944
try:
    WSBICHON_FRISE_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1945
try:
    WSBICHON_FRISE_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1946
try:
    WSBICHON_FRISE_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1949
try:
    WSHELEN_NUMERICS_ID = 'helen'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1950
try:
    WSHELEN_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1951
try:
    WSHELEN_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1952
try:
    WSHELEN_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1953
try:
    WSHELEN_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1954
try:
    WSHELEN_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1955
try:
    WSHELEN_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1956
try:
    WSHELEN_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1957
try:
    WSHELEN_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1958
try:
    WSHELEN_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1959
try:
    WSHELEN_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1960
try:
    WSHELEN_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1961
try:
    WSHELEN_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1962
try:
    WSHELEN_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1963
try:
    WSHELEN_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1964
try:
    WSHELEN_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1965
try:
    WSHELEN_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1968
try:
    WSBEAGLE_NUMERICS_ID = 'beagle'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1969
try:
    WSBEAGLE_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1970
try:
    WSBEAGLE_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1971
try:
    WSBEAGLE_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1972
try:
    WSBEAGLE_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1973
try:
    WSBEAGLE_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1974
try:
    WSBEAGLE_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1975
try:
    WSBEAGLE_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1976
try:
    WSBEAGLE_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1977
try:
    WSBEAGLE_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1978
try:
    WSBEAGLE_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1979
try:
    WSBEAGLE_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1980
try:
    WSBEAGLE_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1981
try:
    WSBEAGLE_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1982
try:
    WSBEAGLE_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1983
try:
    WSBEAGLE_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1984
try:
    WSBEAGLE_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1987
try:
    WSBULLTERRIER_NUMERICS_ID = 'bullterrier'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1988
try:
    WSBULLTERRIER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1989
try:
    WSBULLTERRIER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1990
try:
    WSBULLTERRIER_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1991
try:
    WSBULLTERRIER_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1992
try:
    WSBULLTERRIER_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1993
try:
    WSBULLTERRIER_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1994
try:
    WSBULLTERRIER_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1995
try:
    WSBULLTERRIER_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1996
try:
    WSBULLTERRIER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1997
try:
    WSBULLTERRIER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1998
try:
    WSBULLTERRIER_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1999
try:
    WSBULLTERRIER_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2000
try:
    WSBULLTERRIER_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2001
try:
    WSBULLTERRIER_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2002
try:
    WSBULLTERRIER_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2003
try:
    WSBULLTERRIER_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2006
try:
    WSBORDERTERRIER_NUMERICS_ID = 'borderterrier'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2007
try:
    WSBORDERTERRIER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2008
try:
    WSBORDERTERRIER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2009
try:
    WSBORDERTERRIER_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2010
try:
    WSBORDERTERRIER_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2011
try:
    WSBORDERTERRIER_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2012
try:
    WSBORDERTERRIER_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2013
try:
    WSBORDERTERRIER_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2014
try:
    WSBORDERTERRIER_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2015
try:
    WSBORDERTERRIER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2016
try:
    WSBORDERTERRIER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2017
try:
    WSBORDERTERRIER_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2018
try:
    WSBORDERTERRIER_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2019
try:
    WSBORDERTERRIER_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2020
try:
    WSBORDERTERRIER_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2021
try:
    WSBORDERTERRIER_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2022
try:
    WSBORDERTERRIER_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2025
try:
    WSBASENJI_NUMERICS_ID = 'basenji'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2026
try:
    WSBASENJI_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2027
try:
    WSBASENJI_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2028
try:
    WSBASENJI_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2029
try:
    WSBASENJI_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2030
try:
    WSBASENJI_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2031
try:
    WSBASENJI_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2032
try:
    WSBASENJI_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2033
try:
    WSBASENJI_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2034
try:
    WSBASENJI_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2035
try:
    WSBASENJI_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2036
try:
    WSBASENJI_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2037
try:
    WSBASENJI_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2038
try:
    WSBASENJI_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2039
try:
    WSBASENJI_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2040
try:
    WSBASENJI_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2041
try:
    WSBASENJI_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2044
try:
    WSSHARPEI_NUMERICS_ID = 'sharpei'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2045
try:
    WSSHARPEI_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2046
try:
    WSSHARPEI_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2047
try:
    WSSHARPEI_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2048
try:
    WSSHARPEI_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2049
try:
    WSSHARPEI_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2050
try:
    WSSHARPEI_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2051
try:
    WSSHARPEI_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2052
try:
    WSSHARPEI_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2053
try:
    WSSHARPEI_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2054
try:
    WSSHARPEI_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2055
try:
    WSSHARPEI_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2056
try:
    WSSHARPEI_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2057
try:
    WSSHARPEI_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2058
try:
    WSSHARPEI_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2059
try:
    WSSHARPEI_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2060
try:
    WSSHARPEI_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2063
try:
    WSTIBETANMASTIFF_NUMERICS_ID = 'tibetanmastiff'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2064
try:
    WSTIBETANMASTIFF_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2065
try:
    WSTIBETANMASTIFF_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2066
try:
    WSTIBETANMASTIFF_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2067
try:
    WSTIBETANMASTIFF_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2068
try:
    WSTIBETANMASTIFF_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2069
try:
    WSTIBETANMASTIFF_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2070
try:
    WSTIBETANMASTIFF_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2071
try:
    WSTIBETANMASTIFF_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2072
try:
    WSTIBETANMASTIFF_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2073
try:
    WSTIBETANMASTIFF_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2074
try:
    WSTIBETANMASTIFF_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2075
try:
    WSTIBETANMASTIFF_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2076
try:
    WSTIBETANMASTIFF_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2077
try:
    WSTIBETANMASTIFF_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2078
try:
    WSTIBETANMASTIFF_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2079
try:
    WSTIBETANMASTIFF_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2082
try:
    WSGREATDANE_NUMERICS_ID = 'greatdane'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2083
try:
    WSGREATDANE_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2084
try:
    WSGREATDANE_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2085
try:
    WSGREATDANE_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2086
try:
    WSGREATDANE_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2087
try:
    WSGREATDANE_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2088
try:
    WSGREATDANE_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2089
try:
    WSGREATDANE_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2090
try:
    WSGREATDANE_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2091
try:
    WSGREATDANE_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2092
try:
    WSGREATDANE_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2093
try:
    WSGREATDANE_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2094
try:
    WSGREATDANE_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2095
try:
    WSGREATDANE_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2096
try:
    WSGREATDANE_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2097
try:
    WSGREATDANE_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2098
try:
    WSGREATDANE_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2101
try:
    WSREDDOG_NUMERICS_ID = 'reddog'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2102
try:
    WSREDDOG_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2103
try:
    WSREDDOG_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2104
try:
    WSREDDOG_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2105
try:
    WSREDDOG_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2106
try:
    WSREDDOG_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2107
try:
    WSREDDOG_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2108
try:
    WSREDDOG_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2109
try:
    WSREDDOG_CLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2110
try:
    WSREDDOG_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2111
try:
    WSREDDOG_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2112
try:
    WSREDDOG_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2113
try:
    WSREDDOG_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2114
try:
    WSREDDOG_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2115
try:
    WSREDDOG_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2116
try:
    WSREDDOG_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2117
try:
    WSREDDOG_WSLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2120
try:
    WSAUSTRALIANCATTLEDOG_NUMERICS_ID = 'australiancattledog'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2121
try:
    WSAUSTRALIANCATTLEDOG_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2122
try:
    WSAUSTRALIANCATTLEDOG_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2123
try:
    WSAUSTRALIANCATTLEDOG_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2124
try:
    WSAUSTRALIANCATTLEDOG_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2125
try:
    WSAUSTRALIANCATTLEDOG_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2126
try:
    WSAUSTRALIANCATTLEDOG_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2127
try:
    WSAUSTRALIANCATTLEDOG_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2128
try:
    WSAUSTRALIANCATTLEDOG_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2129
try:
    WSAUSTRALIANCATTLEDOG_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2130
try:
    WSAUSTRALIANCATTLEDOG_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2131
try:
    WSAUSTRALIANCATTLEDOG_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2132
try:
    WSAUSTRALIANCATTLEDOG_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2133
try:
    WSAUSTRALIANCATTLEDOG_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2134
try:
    WSAUSTRALIANCATTLEDOG_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2135
try:
    WSAUSTRALIANCATTLEDOG_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2136
try:
    WSAUSTRALIANCATTLEDOG_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2139
try:
    WSBOXER_NUMERICS_ID = 'boxer'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2140
try:
    WSBOXER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2141
try:
    WSBOXER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2142
try:
    WSBOXER_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2143
try:
    WSBOXER_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2144
try:
    WSBOXER_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2145
try:
    WSBOXER_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2146
try:
    WSBOXER_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2147
try:
    WSBOXER_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2148
try:
    WSBOXER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2149
try:
    WSBOXER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2150
try:
    WSBOXER_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2151
try:
    WSBOXER_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2152
try:
    WSBOXER_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2153
try:
    WSBOXER_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2154
try:
    WSBOXER_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2155
try:
    WSBOXER_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2158
try:
    WSAKITAINU_NUMERICS_ID = 'akitainu'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2159
try:
    WSAKITAINU_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2160
try:
    WSAKITAINU_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2161
try:
    WSAKITAINU_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2162
try:
    WSAKITAINU_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2163
try:
    WSAKITAINU_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2164
try:
    WSAKITAINU_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2165
try:
    WSAKITAINU_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2166
try:
    WSAKITAINU_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2167
try:
    WSAKITAINU_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2168
try:
    WSAKITAINU_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2169
try:
    WSAKITAINU_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2170
try:
    WSAKITAINU_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2171
try:
    WSAKITAINU_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2172
try:
    WSAKITAINU_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2173
try:
    WSAKITAINU_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2174
try:
    WSAKITAINU_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2177
try:
    WSCHIHUAHUA_NUMERICS_ID = 'chihuahua'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2178
try:
    WSCHIHUAHUA_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2179
try:
    WSCHIHUAHUA_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2180
try:
    WSCHIHUAHUA_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2181
try:
    WSCHIHUAHUA_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2182
try:
    WSCHIHUAHUA_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2183
try:
    WSCHIHUAHUA_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2184
try:
    WSCHIHUAHUA_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2185
try:
    WSCHIHUAHUA_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2186
try:
    WSCHIHUAHUA_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2187
try:
    WSCHIHUAHUA_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2188
try:
    WSCHIHUAHUA_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2189
try:
    WSCHIHUAHUA_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2190
try:
    WSCHIHUAHUA_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2191
try:
    WSCHIHUAHUA_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2192
try:
    WSCHIHUAHUA_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2193
try:
    WSCHIHUAHUA_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2196
try:
    WSROTTWEILER_NUMERICS_ID = 'rottweiler'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2197
try:
    WSROTTWEILER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2198
try:
    WSROTTWEILER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2199
try:
    WSROTTWEILER_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2200
try:
    WSROTTWEILER_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2201
try:
    WSROTTWEILER_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2202
try:
    WSROTTWEILER_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2203
try:
    WSROTTWEILER_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2204
try:
    WSROTTWEILER_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2205
try:
    WSROTTWEILER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2206
try:
    WSROTTWEILER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2207
try:
    WSROTTWEILER_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2208
try:
    WSROTTWEILER_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2209
try:
    WSROTTWEILER_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2210
try:
    WSROTTWEILER_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2211
try:
    WSROTTWEILER_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2212
try:
    WSROTTWEILER_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2215
try:
    WSPHARAOHHOUND_NUMERICS_ID = 'pharaohhound'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2216
try:
    WSPHARAOHHOUND_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2217
try:
    WSPHARAOHHOUND_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2218
try:
    WSPHARAOHHOUND_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2219
try:
    WSPHARAOHHOUND_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2220
try:
    WSPHARAOHHOUND_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2221
try:
    WSPHARAOHHOUND_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2222
try:
    WSPHARAOHHOUND_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2223
try:
    WSPHARAOHHOUND_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2224
try:
    WSPHARAOHHOUND_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2225
try:
    WSPHARAOHHOUND_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2226
try:
    WSPHARAOHHOUND_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2227
try:
    WSPHARAOHHOUND_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2228
try:
    WSPHARAOHHOUND_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2229
try:
    WSPHARAOHHOUND_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2230
try:
    WSPHARAOHHOUND_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2231
try:
    WSPHARAOHHOUND_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2234
try:
    WSTROUT_NUMERICS_ID = 'trout'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2235
try:
    WSTROUT_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2236
try:
    WSTROUT_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2237
try:
    WSTROUT_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2238
try:
    WSTROUT_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2239
try:
    WSTROUT_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2240
try:
    WSTROUT_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2241
try:
    WSTROUT_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2242
try:
    WSTROUT_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2243
try:
    WSTROUT_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2244
try:
    WSTROUT_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2245
try:
    WSTROUT_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2246
try:
    WSTROUT_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2247
try:
    WSTROUT_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2248
try:
    WSTROUT_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2249
try:
    WSTROUT_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2250
try:
    WSTROUT_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2253
try:
    WSPUG_NUMERICS_ID = 'pug'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2254
try:
    WSPUG_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2255
try:
    WSPUG_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2256
try:
    WSPUG_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2257
try:
    WSPUG_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2258
try:
    WSPUG_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2259
try:
    WSPUG_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2260
try:
    WSPUG_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2261
try:
    WSPUG_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2262
try:
    WSPUG_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2263
try:
    WSPUG_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2264
try:
    WSPUG_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2265
try:
    WSPUG_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2266
try:
    WSPUG_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2267
try:
    WSPUG_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2268
try:
    WSPUG_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2269
try:
    WSPUG_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2272
try:
    WSPOINTER_NUMERICS_ID = 'pointer'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2273
try:
    WSPOINTER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2274
try:
    WSPOINTER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2275
try:
    WSPOINTER_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2276
try:
    WSPOINTER_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2277
try:
    WSPOINTER_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2278
try:
    WSPOINTER_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2279
try:
    WSPOINTER_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2280
try:
    WSPOINTER_CLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2281
try:
    WSPOINTER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2282
try:
    WSPOINTER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2283
try:
    WSPOINTER_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2284
try:
    WSPOINTER_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2285
try:
    WSPOINTER_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2286
try:
    WSPOINTER_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2287
try:
    WSPOINTER_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2288
try:
    WSPOINTER_WSLONGDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2291
try:
    WSSAMOYED_NUMERICS_ID = 'samoyed'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2292
try:
    WSSAMOYED_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2293
try:
    WSSAMOYED_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2294
try:
    WSSAMOYED_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2295
try:
    WSSAMOYED_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2296
try:
    WSSAMOYED_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2297
try:
    WSSAMOYED_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2298
try:
    WSSAMOYED_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2299
try:
    WSSAMOYED_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2300
try:
    WSSAMOYED_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2301
try:
    WSSAMOYED_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2302
try:
    WSSAMOYED_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2303
try:
    WSSAMOYED_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2304
try:
    WSSAMOYED_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2305
try:
    WSSAMOYED_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2306
try:
    WSSAMOYED_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2307
try:
    WSSAMOYED_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2310
try:
    WSSIBERIANHUSKY_NUMERICS_ID = 'siberianhusky'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2311
try:
    WSSIBERIANHUSKY_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2312
try:
    WSSIBERIANHUSKY_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2313
try:
    WSSIBERIANHUSKY_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2314
try:
    WSSIBERIANHUSKY_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2315
try:
    WSSIBERIANHUSKY_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2316
try:
    WSSIBERIANHUSKY_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2317
try:
    WSSIBERIANHUSKY_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2318
try:
    WSSIBERIANHUSKY_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2319
try:
    WSSIBERIANHUSKY_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2320
try:
    WSSIBERIANHUSKY_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2321
try:
    WSSIBERIANHUSKY_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2322
try:
    WSSIBERIANHUSKY_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2323
try:
    WSSIBERIANHUSKY_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2324
try:
    WSSIBERIANHUSKY_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2325
try:
    WSSIBERIANHUSKY_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2326
try:
    WSSIBERIANHUSKY_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2329
try:
    WSSHIBAINU_NUMERICS_ID = 'shibainu'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2330
try:
    WSSHIBAINU_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2331
try:
    WSSHIBAINU_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2332
try:
    WSSHIBAINU_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2333
try:
    WSSHIBAINU_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2334
try:
    WSSHIBAINU_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2335
try:
    WSSHIBAINU_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2336
try:
    WSSHIBAINU_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2337
try:
    WSSHIBAINU_CLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2338
try:
    WSSHIBAINU_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2339
try:
    WSSHIBAINU_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2340
try:
    WSSHIBAINU_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2341
try:
    WSSHIBAINU_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2342
try:
    WSSHIBAINU_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2343
try:
    WSSHIBAINU_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2344
try:
    WSSHIBAINU_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2345
try:
    WSSHIBAINU_WSLONGDOUBLE = WSTK_LITTLEENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2348
try:
    WSNEWFOUNDLAND_NUMERICS_ID = 'newfoundland'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2349
try:
    WSNEWFOUNDLAND_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2350
try:
    WSNEWFOUNDLAND_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2351
try:
    WSNEWFOUNDLAND_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2352
try:
    WSNEWFOUNDLAND_CINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2353
try:
    WSNEWFOUNDLAND_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2354
try:
    WSNEWFOUNDLAND_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2355
try:
    WSNEWFOUNDLAND_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2356
try:
    WSNEWFOUNDLAND_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2357
try:
    WSNEWFOUNDLAND_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2358
try:
    WSNEWFOUNDLAND_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2359
try:
    WSNEWFOUNDLAND_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2360
try:
    WSNEWFOUNDLAND_WSINT64 = WSTK_64BIT_BIGENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2361
try:
    WSNEWFOUNDLAND_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2362
try:
    WSNEWFOUNDLAND_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2363
try:
    WSNEWFOUNDLAND_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2364
try:
    WSNEWFOUNDLAND_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2367
try:
    WSAFFENPINSCHER_NUMERICS_ID = 'affenpinscher'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2368
try:
    WSAFFENPINSCHER_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2369
try:
    WSAFFENPINSCHER_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2370
try:
    WSAFFENPINSCHER_CLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2371
try:
    WSAFFENPINSCHER_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2372
try:
    WSAFFENPINSCHER_CSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2373
try:
    WSAFFENPINSCHER_CFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2374
try:
    WSAFFENPINSCHER_CDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2375
try:
    WSAFFENPINSCHER_CLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2376
try:
    WSAFFENPINSCHER_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2377
try:
    WSAFFENPINSCHER_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2378
try:
    WSAFFENPINSCHER_WSLONG = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2379
try:
    WSAFFENPINSCHER_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2380
try:
    WSAFFENPINSCHER_WSSIZE_T = WSTK_64BIT_SIGNED_2sCOMPLEMENT_BIGENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2381
try:
    WSAFFENPINSCHER_WSFLOAT = WSTK_BIGENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2382
try:
    WSAFFENPINSCHER_WSDOUBLE = WSTK_BIGENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2383
try:
    WSAFFENPINSCHER_WSLONGDOUBLE = WSTK_BIGENDIAN_128BIT_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2386
try:
    WSBEAUCERON_NUMERICS_ID = 'beauceron'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2387
try:
    WSBEAUCERON_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2388
try:
    WSBEAUCERON_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2389
try:
    WSBEAUCERON_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2390
try:
    WSBEAUCERON_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2391
try:
    WSBEAUCERON_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2392
try:
    WSBEAUCERON_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2393
try:
    WSBEAUCERON_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2394
try:
    WSBEAUCERON_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2395
try:
    WSBEAUCERON_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2396
try:
    WSBEAUCERON_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2397
try:
    WSBEAUCERON_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2398
try:
    WSBEAUCERON_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2399
try:
    WSBEAUCERON_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2400
try:
    WSBEAUCERON_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2401
try:
    WSBEAUCERON_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2402
try:
    WSBEAUCERON_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2406
try:
    WSBERGAMASCO_NUMERICS_ID = 'bergamasco'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2407
try:
    WSBERGAMASCO_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2408
try:
    WSBERGAMASCO_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2409
try:
    WSBERGAMASCO_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2410
try:
    WSBERGAMASCO_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2411
try:
    WSBERGAMASCO_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2412
try:
    WSBERGAMASCO_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2413
try:
    WSBERGAMASCO_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2414
try:
    WSBERGAMASCO_CLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2415
try:
    WSBERGAMASCO_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2416
try:
    WSBERGAMASCO_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2417
try:
    WSBERGAMASCO_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2418
try:
    WSBERGAMASCO_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2419
try:
    WSBERGAMASCO_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2420
try:
    WSBERGAMASCO_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2421
try:
    WSBERGAMASCO_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2422
try:
    WSBERGAMASCO_WSLONGDOUBLE = WSTK_96BIT_HIGHPADDED_INTEL_80BIT_EXTENDED
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2426
try:
    WSBOERBOEL_NUMERICS_ID = 'boerboel'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2427
try:
    WSBOERBOEL_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2428
try:
    WSBOERBOEL_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2429
try:
    WSBOERBOEL_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2430
try:
    WSBOERBOEL_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2431
try:
    WSBOERBOEL_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2432
try:
    WSBOERBOEL_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2433
try:
    WSBOERBOEL_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2434
try:
    WSBOERBOEL_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2435
try:
    WSBOERBOEL_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2436
try:
    WSBOERBOEL_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2437
try:
    WSBOERBOEL_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2438
try:
    WSBOERBOEL_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2439
try:
    WSBOERBOEL_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2440
try:
    WSBOERBOEL_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2441
try:
    WSBOERBOEL_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2442
try:
    WSBOERBOEL_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2446
try:
    WSCHINOOK_NUMERICS_ID = 'chinook'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2447
try:
    WSCHINOOK_CSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2448
try:
    WSCHINOOK_CINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2449
try:
    WSCHINOOK_CLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2450
try:
    WSCHINOOK_CINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2451
try:
    WSCHINOOK_CSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2452
try:
    WSCHINOOK_CFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2453
try:
    WSCHINOOK_CDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2454
try:
    WSCHINOOK_CLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2455
try:
    WSCHINOOK_WSSHORT = WSTK_16BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2456
try:
    WSCHINOOK_WSINT = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2457
try:
    WSCHINOOK_WSLONG = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2458
try:
    WSCHINOOK_WSINT64 = WSTK_64BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2459
try:
    WSCHINOOK_WSSIZE_T = WSTK_32BIT_SIGNED_2sCOMPLEMENT_LITTLEENDIAN_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2460
try:
    WSCHINOOK_WSFLOAT = WSTK_LITTLEENDIAN_IEEE754_SINGLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2461
try:
    WSCHINOOK_WSDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2462
try:
    WSCHINOOK_WSLONGDOUBLE = WSTK_LITTLEENDIAN_IEEE754_DOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2467
try:
    WSOLD_WIN_ENV_NUMERICS_ID = 'Sep 13 1996, 13:46:34'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2468
try:
    WSOLD_WIN_ENV_CSHORT = WSTK_CSHORT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2469
try:
    WSOLD_WIN_ENV_CINT = WSTK_CINT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2470
try:
    WSOLD_WIN_ENV_CLONG = WSTK_CLONG_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2471
try:
    WSOLD_WIN_ENV_CINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2472
try:
    WSOLD_WIN_ENV_CSIZE_T = WSTK_CLONG_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2473
try:
    WSOLD_WIN_ENV_CFLOAT = WSTK_CFLOAT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2474
try:
    WSOLD_WIN_ENV_CDOUBLE = WSTK_CDOUBLE_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2475
try:
    WSOLD_WIN_ENV_CLONGDOUBLE = WSTK_CLONGDOUBLE_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2476
try:
    WSOLD_WIN_ENV_WSSHORT = WSTK_CSHORT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2477
try:
    WSOLD_WIN_ENV_WSINT = WSTK_CINT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2478
try:
    WSOLD_WIN_ENV_WSLONG = WSTK_CLONG_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2479
try:
    WSOLD_WIN_ENV_WSINT64 = WSTK_64BIT_LITTLEENDIAN_STRUCTURE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2480
try:
    WSOLD_WIN_ENV_WSSIZE_T = WSTK_CLONG_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2481
try:
    WSOLD_WIN_ENV_WSFLOAT = WSTK_CFLOAT_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2482
try:
    WSOLD_WIN_ENV_WSDOUBLE = WSTK_CDOUBLE_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2483
try:
    WSOLD_WIN_ENV_WSLONGDOUBLE = WSTK_CLONGDOUBLE_P
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2486
try:
    WSTK_CUCHAR = WSTK_8BIT_UNSIGNED_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2487
try:
    WSTK_WSUCHAR = WSTK_8BIT_UNSIGNED_INTEGER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2648
try:
    WSTP_NUMERICS_ENVIRONMENT_ID = WSBOXER_NUMERICS_ID
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2650
try:
    WSTK_CSHORT = WSBOXER_CSHORT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2651
try:
    WSTK_CINT = WSBOXER_CINT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2652
try:
    WSTK_CLONG = WSBOXER_CLONG
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2653
try:
    WSTK_CINT64 = WSBOXER_CINT64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2654
try:
    WSTK_CSIZE_T = WSBOXER_CSIZE_T
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2655
try:
    WSTK_CFLOAT = WSBOXER_CFLOAT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2656
try:
    WSTK_CDOUBLE = WSBOXER_CDOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2657
try:
    WSTK_CLONGDOUBLE = WSBOXER_CLONGDOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2659
try:
    WSTK_WSSHORT = WSBOXER_WSSHORT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2660
try:
    WSTK_WSINT = WSBOXER_WSINT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2661
try:
    WSTK_WSLONG = WSBOXER_WSLONG
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2662
try:
    WSTK_WSINT64 = WSBOXER_WSINT64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2663
try:
    WSTK_WSSIZE_T = WSBOXER_WSSIZE_T
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2664
try:
    WSTK_WSFLOAT = WSBOXER_WSFLOAT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2665
try:
    WSTK_WSDOUBLE = WSBOXER_WSDOUBLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2666
try:
    WSTK_WSLONGDOUBLE = WSBOXER_WSLONGDOUBLE
except:
    pass

__extended_nt__ = c_longdouble# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2756

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2798
try:
    MLVERSION = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2808
try:
    MLREVISION = 48
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2895
try:
    MLCREATIONID = 114411
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2898
try:
    MLAPI1REVISION = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2899
try:
    MLAPI2REVISION = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2900
try:
    MLAPI3REVISION = 16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2901
try:
    MLAPI4REVISION = 25
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2904
try:
    MLAPIREVISION = MLAPI4REVISION
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3149
try:
    MLOLDDEFINITION = MLAPI1REVISION
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3166
try:
    call_dev_allocator = CallMLAllocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3167
try:
    new_dev_allocator = NewMLAllocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3170
try:
    call_dev_deallocator = CallMLDeallocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3171
try:
    new_dev_deallocator = NewMLDeallocatorProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3175
try:
    MLSTDWORLD_INIT = 16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3176
try:
    MLSTDWORLD_DEINIT = 17
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3177
try:
    MLSTDWORLD_MAKE = 18
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3180
try:
    MLSTDWORLD_GET_SIGNAL_HANDLERS = 29
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3181
try:
    MLSTDWORLD_RELEASE_SIGNAL_HANDLERS = 30
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3184
try:
    MLSTDWORLD_PROTOCOL = 31
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3185
try:
    MLSTDWORLD_MODES = 32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3186
try:
    MLSTDWORLD_STREAMCAPACITY = 33
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3187
try:
    MLSTDWORLD_ID = 34
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3189
try:
    MLSTDDEV_CONNECT_READY = 19
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3190
try:
    MLSTDDEV_CONNECT = 20
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3191
try:
    MLSTDDEV_DESTROY = 21
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3193
try:
    MLSTDDEV_SET_YIELDER = 22
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3194
try:
    MLSTDDEV_GET_YIELDER = 23
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3196
try:
    MLSTDDEV_WRITE_MSG = 24
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3197
try:
    MLSTDDEV_HAS_MSG = 25
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3198
try:
    MLSTDDEV_READ_MSG = 26
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3199
try:
    MLSTDDEV_SET_HANDLER = 27
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3200
try:
    MLSTDDEV_GET_HANDLER = 28
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3203
try:
    T_WORLD_INIT = MLSTDWORLD_INIT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3204
try:
    T_WORLD_DEINIT = MLSTDWORLD_DEINIT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3205
try:
    T_WORLD_MAKE = MLSTDWORLD_MAKE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3206
try:
    T_DEV_CONNECT_READY = MLSTDDEV_CONNECT_READY
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3207
try:
    T_DEV_CONNECT = MLSTDDEV_CONNECT
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3208
try:
    T_DEV_DESTROY = MLSTDDEV_DESTROY
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3210
try:
    T_DEV_SET_YIELDER = MLSTDDEV_SET_YIELDER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3211
try:
    T_DEV_GET_YIELDER = MLSTDDEV_GET_YIELDER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3213
try:
    T_DEV_WRITE_MSG = MLSTDDEV_WRITE_MSG
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3214
try:
    T_DEV_HAS_MSG = MLSTDDEV_HAS_MSG
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3215
try:
    T_DEV_READ_MSG = MLSTDDEV_READ_MSG
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3216
try:
    T_DEV_SET_HANDLER = MLSTDDEV_SET_HANDLER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3217
try:
    T_DEV_GET_HANDLER = MLSTDDEV_GET_HANDLER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3222
try:
    NOMODE = (dev_mode (ord_if_char(0x0000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3223
try:
    LOOPBACKBIT = (dev_mode (ord_if_char(0x0001))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3224
try:
    LISTENBIT = (dev_mode (ord_if_char(0x0002))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3225
try:
    CONNECTBIT = (dev_mode (ord_if_char(0x0004))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3226
try:
    LAUNCHBIT = (dev_mode (ord_if_char(0x0008))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3227
try:
    PARENTCONNECTBIT = (dev_mode (ord_if_char(0x0010))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3228
try:
    READBIT = (dev_mode (ord_if_char(0x0020))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3229
try:
    WRITEBIT = (dev_mode (ord_if_char(0x0040))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3230
try:
    SERVERBIT = (dev_mode (ord_if_char(0x0080))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3231
try:
    ANYMODE = (~(dev_mode (ord_if_char(0))).value)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3241
try:
    _DefaultOptions = (dev_options (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3243
try:
    _NetworkVisibleMask = (dev_options (ord_if_char(0x00000003))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3244
try:
    _BrowseMask = (dev_options (ord_if_char(0x00000010))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3245
try:
    _NonBlockingMask = (dev_options (ord_if_char(0x00000020))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3246
try:
    _InteractMask = (dev_options (ord_if_char(0x00000100))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3247
try:
    _YieldMask = (dev_options (ord_if_char(0x00000200))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3248
try:
    _UseIPV6Mask = (dev_options (ord_if_char(0x00010000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3249
try:
    _UseIPV4Mask = (dev_options (ord_if_char(0x00020000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3250
try:
    _VersionMask = (dev_options (ord_if_char(0x0F000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3251
try:
    _UseNewTCPIPConnectionMask = (dev_options (ord_if_char(0x00100000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3252
try:
    _UseOldTCPIPConnectionMask = (dev_options (ord_if_char(0x00200000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3253
try:
    _UseUUIDTCPIPConnectionMask = (dev_options (ord_if_char(0x00000004))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3254
try:
    _UseAnyNetworkAddressMask = (dev_options (ord_if_char(0x00000008))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3256
try:
    _NetworkVisible = (dev_options (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3257
try:
    _LocallyVisible = (dev_options (ord_if_char(0x00000001))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3258
try:
    _InternetVisible = (dev_options (ord_if_char(0x00000002))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3260
try:
    _Browse = (dev_options (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3261
try:
    _DontBrowse = (dev_options (ord_if_char(0x00000010))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3263
try:
    _NonBlocking = (dev_options (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3264
try:
    _Blocking = (dev_options (ord_if_char(0x00000020))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3266
try:
    _Interact = (dev_options (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3267
try:
    _DontInteract = (dev_options (ord_if_char(0x00000100))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3269
try:
    _ForceYield = (dev_options (ord_if_char(0x00000200))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3270
try:
    _UseIPV6 = (dev_options (ord_if_char(0x00010000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3271
try:
    _UseIPV4 = (dev_options (ord_if_char(0x00020000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3272
try:
    _UseNewTCPIPConnection = (dev_options (ord_if_char(0x00100000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3273
try:
    _UseOldTCPIPConnection = (dev_options (ord_if_char(0x00200000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3274
try:
    _UseUUIDTCPIPConnection = (dev_options (ord_if_char(0x00000004))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3275
try:
    _UseAnyNetworkAddress = (dev_options (ord_if_char(0x00000008))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3279
try:
    INFO_MASK = (1 << 31)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3280
try:
    INFO_TYPE_MASK = ((1 << 31) - 1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3281
try:
    INFO_SWITCH_MASK = (1 << 30)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3282
try:
    MLDEVICE_MASK = INFO_MASK
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3283
try:
    WORLD_MASK = (INFO_MASK | (1 << 30))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3286
try:
    UNREGISTERED_TYPE = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3287
try:
    UNIXPIPE_TYPE = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3288
try:
    UNIXSOCKET_TYPE = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3289
try:
    LOOPBACK_TYPE = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3290
try:
    WINLOCAL_TYPE = 9
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3291
try:
    WINFMAP_TYPE = 10
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3292
try:
    WINSHM_TYPE = 11
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3293
try:
    SOCKET2_TYPE = 12
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3294
try:
    GENERIC_TYPE = 13
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3295
try:
    UNIXSHM_TYPE = 14
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3296
try:
    INTRAPROCESS_TYPE = 15
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3299
try:
    MLDEVICE_TYPE = (MLDEVICE_MASK + 0)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3300
try:
    MLDEVICE_NAME = (MLDEVICE_MASK + 1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3301
try:
    MLDEVICE_NAME_SIZE = (MLDEVICE_MASK + 2)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3302
try:
    MLDEVICE_WORLD_ID = (MLDEVICE_MASK + 5)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3303
try:
    SHM_FD = (MLDEVICE_MASK + ((UNIXSHM_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3304
try:
    PIPE_FD = (MLDEVICE_MASK + ((UNIXPIPE_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3305
try:
    PIPE_CHILD_PID = (MLDEVICE_MASK + ((UNIXPIPE_TYPE * 256) + 1))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3306
try:
    SOCKET_FD = (MLDEVICE_MASK + ((UNIXSOCKET_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3307
try:
    INTRA_FD = (MLDEVICE_MASK + ((INTRAPROCESS_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3308
try:
    SOCKET_PARTNER_ADDR = (MLDEVICE_MASK + ((UNIXSOCKET_TYPE * 256) + 1))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3309
try:
    SOCKET_PARTNER_PORT = (MLDEVICE_MASK + ((UNIXSOCKET_TYPE * 256) + 2))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3310
try:
    LOOPBACK_FD = (MLDEVICE_MASK + ((LOOPBACK_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3311
try:
    INTRAPROCESS_FD = (MLDEVICE_MASK + ((INTRAPROCESS_TYPE * 256) + 0))
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3313
try:
    WINDOWS_SET_NOTIFY_WINDOW = (MLDEVICE_MASK + 2330)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3314
try:
    WINDOWS_REMOVE_NOTIFY_WINDOW = (MLDEVICE_MASK + 2331)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3315
try:
    WINDOWS_READY_CONDITION = (MLDEVICE_MASK + 2332)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3318
try:
    WORLD_THISLOCATION = (1 + WORLD_MASK)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3319
try:
    WORLD_MODES = (2 + WORLD_MASK)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3320
try:
    WORLD_PROTONAME = (3 + WORLD_MASK)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3321
try:
    WORLD_STREAMCAPACITY = (4 + WORLD_MASK)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3322
try:
    WORLD_ID = (5 + WORLD_MASK)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3330
try:
    MLDEVICE_MODE = (MLDEVICE_MASK + 6)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3331
try:
    MLDEVICE_OPTIONS = (MLDEVICE_MASK + 7)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3335
try:
    YIELDVERSION = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3342
try:
    INTERNAL_YIELDING = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3343
try:
    MAKE_YIELDING = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3344
try:
    CONNECT_YIELDING = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3345
try:
    READ_YIELDING = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3346
try:
    WRITE_YIELDING = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3347
try:
    DESTROY_YIELDING = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3348
try:
    READY_YIELDING = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3354
try:
    MAX_SLEEP = 600
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3378
def NewMLYielderProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3380
try:
    NewMLDeviceYielderProc = NewMLYielderProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3399
def NewMLHandlerProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3401
try:
    NewMLDeviceHandlerProc = NewMLHandlerProc
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3479
try:
    MLPARAMETERSIZE_R1 = 256
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3480
try:
    MLPARAMETERSIZE = 356
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3485
try:
    MLLoopBackOpen = MLLoopbackOpen
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3494
def NewMLUserProc(userRoutine):
    return userRoutine

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3508
try:
    MLNetworkVisibleMask = (c_ulong (ord_if_char(0x00000003))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3509
try:
    MLBrowseMask = (c_ulong (ord_if_char(0x00000010))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3510
try:
    MLNonBlockingMask = (c_ulong (ord_if_char(0x00000020))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3511
try:
    MLInteractMask = (c_ulong (ord_if_char(0x00000100))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3512
try:
    MLYieldMask = (c_ulong (ord_if_char(0x00000200))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3513
try:
    MLUseIPV6Mask = (c_ulong (ord_if_char(0x00010000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3514
try:
    MLUseIPV4Mask = (c_ulong (ord_if_char(0x00020000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3515
try:
    MLVersionMask = (c_ulong (ord_if_char(0x0000F000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3516
try:
    MLUseNewTCPIPConnectionMask = (c_ulong (ord_if_char(0x00100000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3517
try:
    MLUseOldTCPIPConnectionMask = (c_ulong (ord_if_char(0x00200000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3518
try:
    MLUseUUIDTCPIPConnectionMask = (c_ulong (ord_if_char(0x00000004))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3519
try:
    MLUseAnyNetworkAddressMask = (c_ulong (ord_if_char(0x00000008))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3523
try:
    MLDefaultOptions = (c_ulong (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3524
try:
    MLNetworkVisible = (c_ulong (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3525
try:
    MLLocallyVisible = (c_ulong (ord_if_char(0x00000001))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3526
try:
    MLInternetVisible = (c_ulong (ord_if_char(0x00000002))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3528
try:
    MLBrowse = (c_ulong (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3529
try:
    MLDontBrowse = (c_ulong (ord_if_char(0x00000010))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3531
try:
    MLNonBlocking = (c_ulong (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3532
try:
    MLBlocking = (c_ulong (ord_if_char(0x00000020))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3534
try:
    MLInteract = (c_ulong (ord_if_char(0x00000000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3535
try:
    MLDontInteract = (c_ulong (ord_if_char(0x00000100))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3537
try:
    MLForceYield = (c_ulong (ord_if_char(0x00000200))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3538
try:
    MLUseIPV6 = (c_ulong (ord_if_char(0x00010000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3539
try:
    MLUseIPV4 = (c_ulong (ord_if_char(0x00020000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3541
try:
    MLUseNewTCPIPConnection = (c_ulong (ord_if_char(0x00100000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3542
try:
    MLUseOldTCPIPConnection = (c_ulong (ord_if_char(0x00200000))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3543
try:
    MLUseUUIDTCPIPConnection = (c_ulong (ord_if_char(0x00000004))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3545
try:
    MLUseAnyNetworkAddress = (c_ulong (ord_if_char(0x00000008))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3548
try:
    MLASCII_ENC = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3549
try:
    MLBYTES_ENC = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3550
try:
    MLUCS2_ENC = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3551
try:
    MLOLD_ENC = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3552
try:
    MLUTF8_ENC = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3553
try:
    MLUTF16_ENC = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3554
try:
    MLUTF32_ENC = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3556
try:
    MLTOTAL_TEXT_ENCODINGS = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3558
try:
    MLLOGERROR = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3559
try:
    MLLOGWARNING = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3560
try:
    MLLOGNOTICE = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3561
try:
    MLLOGINFO = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3562
try:
    MLLOGDEBUG = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3563
try:
    MLLOGDEBUG1 = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3564
try:
    MLLOGDEBUG2 = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3565
try:
    MLLOGDEBUG3 = 7
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3566
try:
    MLLOGDEBUG4 = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3673
try:
    MLNTESTPOINTS = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3678
# #undef MLNTESTPOINTS
try:
    del MLNTESTPOINTS
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3679
try:
    MLNTESTPOINTS = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3684
# #undef MLNTESTPOINTS
try:
    del MLNTESTPOINTS
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3685
try:
    MLNTESTPOINTS = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3690
# #undef MLNTESTPOINTS
try:
    del MLNTESTPOINTS
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3691
try:
    MLNTESTPOINTS = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3885
try:
    MLSDADDSERVICE = 0x0001
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3886
try:
    MLSDREMOVESERVICE = 0x0002
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3887
try:
    MLSDBROWSEERROR = 0x0003
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3888
try:
    MLSDRESOLVEERROR = 0x0004
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3889
try:
    MLSDREGISTERERROR = 0x0005
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3890
try:
    MLSDMORECOMING = 0x0010
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3891
try:
    MLSDNAMECONFLICT = 0x0007
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3953
try:
    WSEUNKNOWN = (-1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3954
try:
    WSEOK = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3955
try:
    WSEDEAD = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3956
try:
    WSEGBAD = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3957
try:
    WSEGSEQ = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3958
try:
    WSEPBTK = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3959
try:
    WSEPSEQ = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3960
try:
    WSEPBIG = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3961
try:
    WSEOVFL = 7
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3962
try:
    WSEMEM = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3963
try:
    WSEACCEPT = 9
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3964
try:
    WSECONNECT = 10
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3965
try:
    WSECLOSED = 11
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3966
try:
    WSEDEPTH = 12
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3967
try:
    WSENODUPFCN = 13
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3969
try:
    WSENOACK = 15
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3970
try:
    WSENODATA = 16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3971
try:
    WSENOTDELIVERED = 17
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3972
try:
    WSENOMSG = 18
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3973
try:
    WSEFAILED = 19
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3975
try:
    WSEGETENDEXPR = 20
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3976
try:
    WSEPUTENDPACKET = 21
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3981
try:
    WSENEXTPACKET = 22
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3982
try:
    WSEUNKNOWNPACKET = 23
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3983
try:
    WSEGETENDPACKET = 24
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3984
try:
    WSEABORT = 25
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3985
try:
    WSEMORE = 26
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3986
try:
    WSENEWLIB = 27
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3987
try:
    WSEOLDLIB = 28
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3988
try:
    WSEBADPARAM = 29
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3989
try:
    WSENOTIMPLEMENTED = 30
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3992
try:
    WSEINIT = 32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3993
try:
    WSEARGV = 33
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3994
try:
    WSEPROTOCOL = 34
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3995
try:
    WSEMODE = 35
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3996
try:
    WSELAUNCH = 36
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3997
try:
    WSELAUNCHAGAIN = 37
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3998
try:
    WSELAUNCHSPACE = 38
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3999
try:
    WSENOPARENT = 39
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4000
try:
    WSENAMETAKEN = 40
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4001
try:
    WSENOLISTEN = 41
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4002
try:
    WSEBADNAME = 42
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4003
try:
    WSEBADHOST = 43
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4004
try:
    WSERESOURCE = 44
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4005
try:
    WSELAUNCHFAILED = 45
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4006
try:
    WSELAUNCHNAME = 46
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4007
try:
    WSEPDATABAD = 47
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4008
try:
    WSEPSCONVERT = 48
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4009
try:
    WSEGSCONVERT = 49
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4010
try:
    WSENOTEXE = 50
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4011
try:
    WSESYNCOBJECTMAKE = 51
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4012
try:
    WSEBACKOUT = 52
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4013
try:
    WSEBADOPTSYM = 53
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4014
try:
    WSEBADOPTSTR = 54
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4015
try:
    WSENEEDBIGGERBUFFER = 55
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4016
try:
    WSEBADNUMERICSID = 56
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4017
try:
    WSESERVICENOTAVAILABLE = 57
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4018
try:
    WSEBADARGUMENT = 58
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4019
try:
    WSEBADDISCOVERYHOSTNAME = 59
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4020
try:
    WSEBADDISCOVERYDOMAINNAME = 60
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4021
try:
    WSEBADSERVICENAME = 61
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4022
try:
    WSEBADDISCOVERYSTATE = 62
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4023
try:
    WSEBADDISCOVERYFLAGS = 63
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4024
try:
    WSEDISCOVERYNAMECOLLISION = 64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4025
try:
    WSEBADSERVICEDISCOVERY = 65
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4026
try:
    WSELAST = WSESERVICENOTAVAILABLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4028
try:
    WSETRACEON = 996
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4029
try:
    WSETRACEOFF = 997
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4030
try:
    WSEDEBUG = 998
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4031
try:
    WSEASSERT = 999
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4032
try:
    WSEUSER = 1000
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4063
try:
    MLEUNKNOWN = (-1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4064
try:
    MLEOK = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4065
try:
    MLEDEAD = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4066
try:
    MLEGBAD = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4067
try:
    MLEGSEQ = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4068
try:
    MLEPBTK = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4069
try:
    MLEPSEQ = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4070
try:
    MLEPBIG = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4071
try:
    MLEOVFL = 7
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4072
try:
    MLEMEM = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4073
try:
    MLEACCEPT = 9
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4074
try:
    MLECONNECT = 10
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4075
try:
    MLECLOSED = 11
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4076
try:
    MLEDEPTH = 12
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4077
try:
    MLENODUPFCN = 13
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4079
try:
    MLENOACK = 15
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4080
try:
    MLENODATA = 16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4081
try:
    MLENOTDELIVERED = 17
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4082
try:
    MLENOMSG = 18
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4083
try:
    MLEFAILED = 19
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4085
try:
    MLEGETENDEXPR = 20
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4086
try:
    MLEPUTENDPACKET = 21
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4091
try:
    MLENEXTPACKET = 22
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4092
try:
    MLEUNKNOWNPACKET = 23
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4093
try:
    MLEGETENDPACKET = 24
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4094
try:
    MLEABORT = 25
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4095
try:
    MLEMORE = 26
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4096
try:
    MLENEWLIB = 27
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4097
try:
    MLEOLDLIB = 28
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4098
try:
    MLEBADPARAM = 29
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4099
try:
    MLENOTIMPLEMENTED = 30
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4102
try:
    MLEINIT = 32
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4103
try:
    MLEARGV = 33
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4104
try:
    MLEPROTOCOL = 34
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4105
try:
    MLEMODE = 35
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4106
try:
    MLELAUNCH = 36
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4107
try:
    MLELAUNCHAGAIN = 37
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4108
try:
    MLELAUNCHSPACE = 38
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4109
try:
    MLENOPARENT = 39
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4110
try:
    MLENAMETAKEN = 40
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4111
try:
    MLENOLISTEN = 41
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4112
try:
    MLEBADNAME = 42
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4113
try:
    MLEBADHOST = 43
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4114
try:
    MLERESOURCE = 44
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4115
try:
    MLELAUNCHFAILED = 45
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4116
try:
    MLELAUNCHNAME = 46
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4117
try:
    MLEPDATABAD = 47
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4118
try:
    MLEPSCONVERT = 48
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4119
try:
    MLEGSCONVERT = 49
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4120
try:
    MLENOTEXE = 50
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4121
try:
    MLESYNCOBJECTMAKE = 51
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4122
try:
    MLEBACKOUT = 52
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4123
try:
    MLEBADOPTSYM = 53
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4124
try:
    MLEBADOPTSTR = 54
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4125
try:
    MLENEEDBIGGERBUFFER = 55
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4126
try:
    MLEBADNUMERICSID = 56
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4127
try:
    MLESERVICENOTAVAILABLE = 57
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4128
try:
    MLEBADARGUMENT = 58
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4129
try:
    MLEBADDISCOVERYHOSTNAME = 59
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4130
try:
    MLEBADDISCOVERYDOMAINNAME = 60
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4131
try:
    MLEBADSERVICENAME = 61
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4132
try:
    MLEBADDISCOVERYSTATE = 62
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4133
try:
    MLEBADDISCOVERYFLAGS = 63
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4134
try:
    MLEDISCOVERYNAMECOLLISION = 64
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4135
try:
    MLEBADSERVICEDISCOVERY = 65
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4136
try:
    MLELAST = MLESERVICENOTAVAILABLE
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4138
try:
    MLETRACEON = 996
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4139
try:
    MLETRACEOFF = 997
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4140
try:
    MLEDEBUG = 998
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4141
try:
    MLEASSERT = 999
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4142
try:
    MLEUSER = 1000
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4284
try:
    WSTKOLDINT = 'I'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4285
try:
    WSTKOLDREAL = 'R'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4288
try:
    WSTKFUNC = 'F'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4290
try:
    WSTKERROR = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4291
try:
    WSTKERR = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4294
def WSTK__IS_TEXT(tok):
    return ((tok & 0x00F6) == 0x0022)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4297
try:
    WSTKSTR = '"'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4298
try:
    WSTKSYM = '#'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4301
try:
    WSTKOPTSYM = 'O'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4302
try:
    WSTKOPTSTR = 'Q'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4305
try:
    WSTKREAL = '*'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4306
try:
    WSTKINT = '+'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4311
try:
    WSTKPCTEND = ']'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4312
try:
    WSTKAPCTEND = '\\n'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4313
try:
    WSTKEND = '\\n'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4314
try:
    WSTKAEND = '\\r'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4315
try:
    WSTKSEND = ','
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4317
try:
    WSTKCONT = '\\\\'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4318
try:
    WSTKELEN = ' '
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4320
try:
    WSTKNULL = '.'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4321
try:
    WSTKOLDSYM = 'Y'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4322
try:
    WSTKOLDSTR = 'S'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4326
try:
    WSTKPACKED = 'P'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4327
try:
    WSTKARRAY = 'A'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4328
try:
    WSTKDIM = 'D'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4330
try:
    WSLENGTH_DECODER = ((decoder_mask (ord_if_char(1))).value << 16)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4331
try:
    WSTKPACKED_DECODER = ((decoder_mask (ord_if_char(1))).value << 17)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4332
try:
    WSTKARRAY_DECODER = ((decoder_mask (ord_if_char(1))).value << 18)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4333
try:
    WSTKMODERNCHARS_DECODER = ((decoder_mask (ord_if_char(1))).value << 19)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4334
try:
    WSTKNULLSEQUENCE_DECODER = (decoder_mask (ord_if_char(0))).value
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4335
try:
    WSTKALL_DECODERS = ((((WSLENGTH_DECODER | WSTKPACKED_DECODER) | WSTKARRAY_DECODER) | WSTKMODERNCHARS_DECODER) | WSTKNULLSEQUENCE_DECODER)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4337
try:
    WSTK_FIRSTUSER = '0'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 4338
try:
    WSTK_LASTUSER = 'F'
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6064
try:
    MAX_BYTES_PER_OLD_CHARACTER = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6068
try:
    MAX_BYTES_PER_NEW_CHARACTER = 10
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6071
try:
    WS_MAX_BYTES_PER_CHARACTER = MAX_BYTES_PER_NEW_CHARACTER
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6087
def WSStringFirstPos(s, pos):
    return (WSStringFirstPosFun (s, pointer(pos)))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6092
def WSStringChar(pos):
    return (WSStringCharacter ((pos.str), (pos.end)))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6094
try:
    WSPutCharToString = WSConvertCharacter
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6119
def WSOldStringChar(pos):
    return (((pos.mode).value) <= 1) and (uint_ct (ord_if_char(((ucharp_ct (ord_if_char(((pos.cc).value)))).value[0])))).value or (WSOldStringCharFun (pointer(pos)))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6123
def WSOldStringFirstPos(s, pos):
    return (WSOldStringFirstPosFun (s, pointer(pos)))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6125
def WSOldStringNextPos(pos):
    return (((pos.mode).value) == 0) and (((pos.cc)[0]) and (((pos.cc).value) + 1) or (pos.cc)[0]) and 0 or 0 or (WSOldStringNextPosFun (pointer(pos)))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6293
try:
    ML_USES_NEW_PUTBYTEARRAY_API = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6597
def MLUCS2String(container):
    return ((container.contents.pointer).ucs2)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6598
def MLUTF8String(container):
    return ((container.contents.pointer).utf8)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6599
def MLUTF16String(container):
    return ((container.contents.pointer).utf16)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6600
def MLUTF32String(container):
    return ((container.contents.pointer).utf32)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6601
def MLUnicodeStringLength(container):
    return (container.contents.length)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6602
def MLUnicodeStringType(container):
    return (container.contents.type)

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6697
try:
    ILLEGALPKT = 0
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6699
try:
    CALLPKT = 7
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6700
try:
    EVALUATEPKT = 13
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6701
try:
    RETURNPKT = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6703
try:
    INPUTNAMEPKT = 8
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6704
try:
    ENTERTEXTPKT = 14
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6705
try:
    ENTEREXPRPKT = 15
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6706
try:
    OUTPUTNAMEPKT = 9
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6707
try:
    RETURNTEXTPKT = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6708
try:
    RETURNEXPRPKT = 16
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6710
try:
    DISPLAYPKT = 11
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6711
try:
    DISPLAYENDPKT = 12
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6713
try:
    MESSAGEPKT = 5
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6714
try:
    TEXTPKT = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6716
try:
    INPUTPKT = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6717
try:
    INPUTSTRPKT = 21
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6718
try:
    MENUPKT = 6
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6719
try:
    SYNTAXPKT = 10
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6721
try:
    SUSPENDPKT = 17
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6722
try:
    RESUMEPKT = 18
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6724
try:
    BEGINDLGPKT = 19
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6725
try:
    ENDDLGPKT = 20
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6727
try:
    FIRSTUSERPKT = 128
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6728
try:
    LASTUSERPKT = 255
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6776
def NewMLAlertProc(userRoutine):
    return (MLAlertCast (userRoutine))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6777
def NewMLRequestProc(userRoutine):
    return (MLRequestCast (userRoutine))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6778
def NewMLConfirmProc(userRoutine):
    return (MLConfirmCast (userRoutine))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6779
def NewMLRequestArgvProc(userRoutine):
    return (MLRequestArgvCast (userRoutine))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6780
def NewMLRequestToInteractProc(userRoutine):
    return (MLRequestToInteractCast (userRoutine))

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6897
try:
    MLALERT = MLAlert_unix
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6898
try:
    MLREQUEST = MLRequest_unix
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6899
try:
    MLCONFIRM = MLConfirm_unix
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6900
try:
    MLPERMIT = MLPermit_unix
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6901
try:
    MLREQUESTARGV = default_request_argv
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6934
try:
    WSWAITSUCCESS = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6935
try:
    WSWAITERROR = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6936
try:
    WSWAITTIMEOUT = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6937
try:
    WSWAITCALLBACKABORTED = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6968
try:
    MLREADYPARALLELERROR = (-1)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6969
try:
    MLREADYPARALLELTIMEDOUT = (-2)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6970
try:
    MLREADYPARALLELINVALIDARGUMENT = (-3)
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6987
try:
    MLWAITSUCCESS = 1
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6988
try:
    MLWAITERROR = 2
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6989
try:
    MLWAITTIMEOUT = 3
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6990
try:
    MLWAITCALLBACKABORTED = 4
except:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7057
# #undef MLActivate
try:
    del MLActivate
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7058
# #undef MLAlert
try:
    del MLAlert
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7059
# #undef MLAlertCast
try:
    del MLAlertCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7060
# #undef MLAlign
try:
    del MLAlign
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7061
# #undef MLAllocParameter
try:
    del MLAllocParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7062
# #undef MLAllocatorCast
try:
    del MLAllocatorCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7063
# #undef MLBegin
try:
    del MLBegin
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7064
# #undef MLBrowseForLinkServices
try:
    del MLBrowseForLinkServices
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7065
# #undef MLBytesToGet
try:
    del MLBytesToGet
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7066
# #undef MLBytesToPut
try:
    del MLBytesToPut
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7067
# #undef MLCallMessageHandler
try:
    del MLCallMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7068
# #undef MLCallYieldFunction
try:
    del MLCallYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7069
# #undef MLCharacterOffset
try:
    del MLCharacterOffset
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7070
# #undef MLCheckFunction
try:
    del MLCheckFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7071
# #undef MLCheckFunctionWithArgCount
try:
    del MLCheckFunctionWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7072
# #undef MLClearAllSymbolReplacements
try:
    del MLClearAllSymbolReplacements
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7073
# #undef MLClearError
try:
    del MLClearError
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7074
# #undef MLClearSymbolReplacement
try:
    del MLClearSymbolReplacement
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7075
# #undef MLClose
try:
    del MLClose
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7076
# #undef MLCompilerID
try:
    del MLCompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7077
# #undef MLConfirm
try:
    del MLConfirm
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7078
# #undef MLConfirmCast
try:
    del MLConfirmCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7079
# #undef MLConnect
try:
    del MLConnect
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7080
# #undef MLContextFromLinkServer
try:
    del MLContextFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7081
# #undef MLConvertByteString
try:
    del MLConvertByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7082
# #undef MLConvertByteStringNL
try:
    del MLConvertByteStringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7083
# #undef MLConvertCharacter
try:
    del MLConvertCharacter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7084
# #undef MLConvertDoubleByteString
try:
    del MLConvertDoubleByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7085
# #undef MLConvertDoubleByteStringNL
try:
    del MLConvertDoubleByteStringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7086
# #undef MLConvertNewLine
try:
    del MLConvertNewLine
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7087
# #undef MLConvertUCS2String
try:
    del MLConvertUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7088
# #undef MLConvertUCS2StringNL
try:
    del MLConvertUCS2StringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7089
# #undef MLConvertUTF16String
try:
    del MLConvertUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7090
# #undef MLConvertUTF16StringNL
try:
    del MLConvertUTF16StringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7091
# #undef MLConvertUTF32String
try:
    del MLConvertUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7092
# #undef MLConvertUTF32StringNL
try:
    del MLConvertUTF32StringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7093
# #undef MLConvertUTF8String
try:
    del MLConvertUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7094
# #undef MLConvertUTF8StringNL
try:
    del MLConvertUTF8StringNL
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7095
# #undef MLCountYP
try:
    del MLCountYP
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7096
# #undef MLCreateMark
try:
    del MLCreateMark
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7097
# #undef MLCreateMessageHandler
try:
    del MLCreateMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7098
# #undef MLCreateYieldFunction
try:
    del MLCreateYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7099
# #undef MLDeallocatorCast
try:
    del MLDeallocatorCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7100
# #undef MLDefaultYieldFunction
try:
    del MLDefaultYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7101
# #undef MLDeinit
try:
    del MLDeinit
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7102
# #undef MLDeinitialize
try:
    del MLDeinitialize
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7103
# #undef MLDestroyMark
try:
    del MLDestroyMark
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7104
# #undef MLDestroyMessageHandler
try:
    del MLDestroyMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7105
# #undef MLDestroyYieldFunction
try:
    del MLDestroyYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7106
# #undef MLDeviceInformation
try:
    del MLDeviceInformation
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7107
# #undef MLDisableLinkLock
try:
    del MLDisableLinkLock
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7108
# #undef MLDisableLoggingStream
try:
    del MLDisableLoggingStream
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7109
# #undef MLDoNotHandleSignalParameter
try:
    del MLDoNotHandleSignalParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7110
# #undef MLDuplicateLink
try:
    del MLDuplicateLink
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7111
# #undef MLEnableLinkLock
try:
    del MLEnableLinkLock
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7112
# #undef MLEnableLoggingStream
try:
    del MLEnableLoggingStream
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7113
# #undef MLEnclosingEnvironment
try:
    del MLEnclosingEnvironment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7114
# #undef MLEnd
try:
    del MLEnd
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7115
# #undef MLEndPacket
try:
    del MLEndPacket
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7116
# #undef MLEnvironmentData
try:
    del MLEnvironmentData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7117
# #undef MLError
try:
    del MLError
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7118
# #undef MLErrorMessage
try:
    del MLErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7119
# #undef MLErrorParameter
try:
    del MLErrorParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7120
# #undef MLErrorString
try:
    del MLErrorString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7121
# #undef MLEstablish
try:
    del MLEstablish
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7122
# #undef MLEstablishString
try:
    del MLEstablishString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7123
# #undef MLExpressionsToGet
try:
    del MLExpressionsToGet
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7124
# #undef MLFeatureString
try:
    del MLFeatureString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7125
# #undef MLFill
try:
    del MLFill
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7126
# #undef MLFilterArgv
try:
    del MLFilterArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7127
# #undef MLFlush
try:
    del MLFlush
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7128
# #undef MLForwardReset
try:
    del MLForwardReset
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7129
# #undef MLFromLinkID
try:
    del MLFromLinkID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7130
# #undef MLGet7BitCharacters
try:
    del MLGet7BitCharacters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7131
# #undef MLGet8BitCharacters
try:
    del MLGet8BitCharacters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7132
# #undef MLGetAvailableLinkProtocolNames
try:
    del MLGetAvailableLinkProtocolNames
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7133
# #undef MLGetArgCount
try:
    del MLGetArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7134
# #undef MLGetArrayDimensions
try:
    del MLGetArrayDimensions
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7135
# #undef MLGetArrayType
try:
    del MLGetArrayType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7136
# #undef MLGetArrayTypeWithDepthAndLeafType
try:
    del MLGetArrayTypeWithDepthAndLeafType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7137
# #undef MLGetBinaryNumber
try:
    del MLGetBinaryNumber
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7138
# #undef MLGetBinaryNumberArray
try:
    del MLGetBinaryNumberArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7139
# #undef MLGetBinaryNumberArrayData
try:
    del MLGetBinaryNumberArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7140
# #undef MLGetBinaryNumberArrayDataWithHeads
try:
    del MLGetBinaryNumberArrayDataWithHeads
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7141
# #undef MLGetBinaryNumberArrayWithLeafType
try:
    del MLGetBinaryNumberArrayWithLeafType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7142
# #undef MLGetBinaryNumberList
try:
    del MLGetBinaryNumberList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7143
# #undef MLGetByteArray
try:
    del MLGetByteArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7144
# #undef MLGetByteArrayData
try:
    del MLGetByteArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7145
# #undef MLGetByteString
try:
    del MLGetByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7146
# #undef MLGetByteSymbol
try:
    del MLGetByteSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7147
# #undef MLGetData
try:
    del MLGetData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7148
# #undef MLGetDomainNameList
try:
    del MLGetDomainNameList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7149
# #undef MLGetDouble
try:
    del MLGetDouble
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7150
# #undef MLGetDoubleArray
try:
    del MLGetDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7151
# #undef MLGetDoubleArrayData
try:
    del MLGetDoubleArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7152
# #undef MLGetFloat
try:
    del MLGetFloat
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7153
# #undef MLGetFloatArray
try:
    del MLGetFloatArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7154
# #undef MLGetFloatArrayData
try:
    del MLGetFloatArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7155
# #undef MLGetFunction
try:
    del MLGetFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7156
# #undef MLGetInteger
try:
    del MLGetInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7157
# #undef MLGetInteger16
try:
    del MLGetInteger16
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7158
# #undef MLGetInteger16Array
try:
    del MLGetInteger16Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7159
# #undef MLGetInteger16ArrayData
try:
    del MLGetInteger16ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7160
# #undef MLGetInteger16List
try:
    del MLGetInteger16List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7161
# #undef MLGetInteger32
try:
    del MLGetInteger32
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7162
# #undef MLGetInteger32Array
try:
    del MLGetInteger32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7163
# #undef MLGetInteger32ArrayData
try:
    del MLGetInteger32ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7164
# #undef MLGetInteger32List
try:
    del MLGetInteger32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7165
# #undef MLGetInteger64
try:
    del MLGetInteger64
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7166
# #undef MLGetInteger64Array
try:
    del MLGetInteger64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7167
# #undef MLGetInteger64ArrayData
try:
    del MLGetInteger64ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7168
# #undef MLGetInteger64List
try:
    del MLGetInteger64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7169
# #undef MLGetInteger8
try:
    del MLGetInteger8
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7170
# #undef MLGetInteger8Array
try:
    del MLGetInteger8Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7171
# #undef MLGetInteger8ArrayData
try:
    del MLGetInteger8ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7172
# #undef MLGetInteger8List
try:
    del MLGetInteger8List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7173
# #undef MLGetIntegerArray
try:
    del MLGetIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7174
# #undef MLGetIntegerArrayData
try:
    del MLGetIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7175
# #undef MLGetIntegerList
try:
    del MLGetIntegerList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7176
# #undef MLGetLinkedEnvIDString
try:
    del MLGetLinkedEnvIDString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7177
# #undef MLGetLinksFromEnvironment
try:
    del MLGetLinksFromEnvironment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7178
# #undef MLGetLongDouble
try:
    del MLGetLongDouble
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7179
# #undef MLGetLongDoubleArray
try:
    del MLGetLongDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7180
# #undef MLGetLongDoubleArrayData
try:
    del MLGetLongDoubleArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7181
# #undef MLGetLongInteger
try:
    del MLGetLongInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7182
# #undef MLGetLongIntegerArray
try:
    del MLGetLongIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7183
# #undef MLGetLongIntegerArrayData
try:
    del MLGetLongIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7184
# #undef MLGetMessage
try:
    del MLGetMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7185
# #undef MLGetMessageHandler
try:
    del MLGetMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7186
# #undef MLGetNetworkAddressList
try:
    del MLGetNetworkAddressList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7187
# #undef MLGetNext
try:
    del MLGetNext
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7188
# #undef MLGetNextRaw
try:
    del MLGetNextRaw
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7189
# #undef MLGetNumberAsByteString
try:
    del MLGetNumberAsByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7190
# #undef MLGetNumberAsString
try:
    del MLGetNumberAsString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7191
# #undef MLGetNumberAsUCS2String
try:
    del MLGetNumberAsUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7192
# #undef MLGetNumberAsUTF16String
try:
    del MLGetNumberAsUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7193
# #undef MLGetNumberAsUTF32String
try:
    del MLGetNumberAsUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7194
# #undef MLGetNumberAsUTF8String
try:
    del MLGetNumberAsUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7195
# #undef MLGetRawArgCount
try:
    del MLGetRawArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7196
# #undef MLGetRawData
try:
    del MLGetRawData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7197
# #undef MLGetRawType
try:
    del MLGetRawType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7198
# #undef MLGetReal
try:
    del MLGetReal
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7199
# #undef MLGetReal128
try:
    del MLGetReal128
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7200
# #undef MLGetReal128Array
try:
    del MLGetReal128Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7201
# #undef MLGetReal128ArrayData
try:
    del MLGetReal128ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7202
# #undef MLGetReal128List
try:
    del MLGetReal128List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7203
# #undef MLGetReal32
try:
    del MLGetReal32
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7204
# #undef MLGetReal32Array
try:
    del MLGetReal32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7205
# #undef MLGetReal32ArrayData
try:
    del MLGetReal32ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7206
# #undef MLGetReal32List
try:
    del MLGetReal32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7207
# #undef MLGetReal64
try:
    del MLGetReal64
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7208
# #undef MLGetReal64Array
try:
    del MLGetReal64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7209
# #undef MLGetReal64ArrayData
try:
    del MLGetReal64ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7210
# #undef MLGetReal64List
try:
    del MLGetReal64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7211
# #undef MLGetRealArray
try:
    del MLGetRealArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7212
# #undef MLGetRealList
try:
    del MLGetRealList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7213
# #undef MLGetShortInteger
try:
    del MLGetShortInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7214
# #undef MLGetShortIntegerArray
try:
    del MLGetShortIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7215
# #undef MLGetShortIntegerArrayData
try:
    del MLGetShortIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7216
# #undef MLGetString
try:
    del MLGetString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7217
# #undef MLGetSymbol
try:
    del MLGetSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7218
# #undef MLGetType
try:
    del MLGetType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7219
# #undef MLGetUCS2Characters
try:
    del MLGetUCS2Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7220
# #undef MLGetUCS2String
try:
    del MLGetUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7221
# #undef MLGetUCS2Symbol
try:
    del MLGetUCS2Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7222
# #undef MLGetUTF16Characters
try:
    del MLGetUTF16Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7223
# #undef MLGetUTF16String
try:
    del MLGetUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7224
# #undef MLGetUTF16Symbol
try:
    del MLGetUTF16Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7225
# #undef MLGetUTF32Characters
try:
    del MLGetUTF32Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7226
# #undef MLGetUTF32String
try:
    del MLGetUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7227
# #undef MLGetUTF32Symbol
try:
    del MLGetUTF32Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7228
# #undef MLGetUTF8Characters
try:
    del MLGetUTF8Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7229
# #undef MLGetUTF8String
try:
    del MLGetUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7230
# #undef MLGetUTF8Symbol
try:
    del MLGetUTF8Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7231
# #undef MLGetYieldFunction
try:
    del MLGetYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7232
# #undef MLHandlerCast
try:
    del MLHandlerCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7233
# #undef MLHandleSignal
try:
    del MLHandleSignal
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7234
# #undef MLInit
try:
    del MLInit
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7235
# #undef MLInitialize
try:
    del MLInitialize
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7236
# #undef MLInterfaceFromLinkServer
try:
    del MLInterfaceFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7237
# #undef MLIsLinkLoopback
try:
    del MLIsLinkLoopback
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7238
# #undef MLLinkEnvironment
try:
    del MLLinkEnvironment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7239
# #undef MLLinkName
try:
    del MLLinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7240
# #undef MLLogFileNameForLink
try:
    del MLLogFileNameForLink
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7241
# #undef MLLogStreamToFile
try:
    del MLLogStreamToFile
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7242
# #undef MLLoopbackOpen
try:
    del MLLoopbackOpen
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7243
# #undef MLLowLevelDeviceName
try:
    del MLLowLevelDeviceName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7244
# #undef MLMessageHandler
try:
    del MLMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7245
# #undef MLMessageReady
try:
    del MLMessageReady
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7246
# #undef MLName
try:
    del MLName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7247
# #undef MLNewLinkServer
try:
    del MLNewLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7248
# #undef MLNewLinkServerWithPort
try:
    del MLNewLinkServerWithPort
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7249
# #undef MLNewLinkServerWithPortAndInterface
try:
    del MLNewLinkServerWithPortAndInterface
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7250
# #undef MLNewPacket
try:
    del MLNewPacket
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7251
# #undef MLNewParameters
try:
    del MLNewParameters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7252
# #undef MLNewUnicodeContainer
try:
    del MLNewUnicodeContainer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7253
# #undef MLNextCharacter
try:
    del MLNextCharacter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7254
# #undef MLNextCharacterFromStringWithLength
try:
    del MLNextCharacterFromStringWithLength
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7255
# #undef MLNextPacket
try:
    del MLNextPacket
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7256
# #undef MLNumber
try:
    del MLNumber
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7257
# #undef MLNumericsQuery
try:
    del MLNumericsQuery
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7258
# #undef MLOldConvertByteString
try:
    del MLOldConvertByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7259
# #undef MLOldConvertUCS2String
try:
    del MLOldConvertUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7260
# #undef MLOldPutCharToString
try:
    del MLOldPutCharToString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7261
# #undef MLOldStringCharFun
try:
    del MLOldStringCharFun
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7262
# #undef MLOldStringFirstPosFun
try:
    del MLOldStringFirstPosFun
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7263
# #undef MLOldStringNextPosFun
try:
    del MLOldStringNextPosFun
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7264
# #undef MLOpen
try:
    del MLOpen
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7265
# #undef MLOpenArgcArgv
try:
    del MLOpenArgcArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7266
# #undef MLOpenArgv
try:
    del MLOpenArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7267
# #undef MLOpenInEnv
try:
    del MLOpenInEnv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7268
# #undef MLOpenString
try:
    del MLOpenString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7269
# #undef MLPortFromLinkServer
try:
    del MLPortFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7270
# #undef MLPrintArgv
try:
    del MLPrintArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7271
# #undef MLPut7BitCharacters
try:
    del MLPut7BitCharacters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7272
# #undef MLPut7BitCount
try:
    del MLPut7BitCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7273
# #undef MLPut8BitCharacters
try:
    del MLPut8BitCharacters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7274
# #undef MLPutArgCount
try:
    del MLPutArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7275
# #undef MLPutArray
try:
    del MLPutArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7276
# #undef MLPutArrayLeaves
try:
    del MLPutArrayLeaves
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7277
# #undef MLPutArrayType
try:
    del MLPutArrayType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7278
# #undef MLPutBinaryNumber
try:
    del MLPutBinaryNumber
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7279
# #undef MLPutBinaryNumberArray
try:
    del MLPutBinaryNumberArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7280
# #undef MLPutBinaryNumberArrayData
try:
    del MLPutBinaryNumberArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7281
# #undef MLPutBinaryNumberArrayDataWithHeads
try:
    del MLPutBinaryNumberArrayDataWithHeads
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7282
# #undef MLPutBinaryNumberList
try:
    del MLPutBinaryNumberList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7283
# #undef MLPutByteArray
try:
    del MLPutByteArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7284
# #undef MLPutByteArrayData
try:
    del MLPutByteArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7285
# #undef MLPutByteString
try:
    del MLPutByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7286
# #undef MLPutByteSymbol
try:
    del MLPutByteSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7287
# #undef MLPutComposite
try:
    del MLPutComposite
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7288
# #undef MLPutData
try:
    del MLPutData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7289
# #undef MLPutDouble
try:
    del MLPutDouble
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7290
# #undef MLPutDoubleArray
try:
    del MLPutDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7291
# #undef MLPutDoubleArrayData
try:
    del MLPutDoubleArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7292
# #undef MLPutFloat
try:
    del MLPutFloat
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7293
# #undef MLPutFloatArray
try:
    del MLPutFloatArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7294
# #undef MLPutFloatArrayData
try:
    del MLPutFloatArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7295
# #undef MLPutFunction
try:
    del MLPutFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7296
# #undef MLPutInteger
try:
    del MLPutInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7297
# #undef MLPutInteger16
try:
    del MLPutInteger16
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7298
# #undef MLPutInteger16Array
try:
    del MLPutInteger16Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7299
# #undef MLPutInteger16ArrayData
try:
    del MLPutInteger16ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7300
# #undef MLPutInteger16List
try:
    del MLPutInteger16List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7301
# #undef MLPutInteger32
try:
    del MLPutInteger32
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7302
# #undef MLPutInteger32Array
try:
    del MLPutInteger32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7303
# #undef MLPutInteger32ArrayData
try:
    del MLPutInteger32ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7304
# #undef MLPutInteger32List
try:
    del MLPutInteger32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7305
# #undef MLPutInteger64
try:
    del MLPutInteger64
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7306
# #undef MLPutInteger64Array
try:
    del MLPutInteger64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7307
# #undef MLPutInteger64ArrayData
try:
    del MLPutInteger64ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7308
# #undef MLPutInteger64List
try:
    del MLPutInteger64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7309
# #undef MLPutInteger8
try:
    del MLPutInteger8
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7310
# #undef MLPutInteger8Array
try:
    del MLPutInteger8Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7311
# #undef MLPutInteger8ArrayData
try:
    del MLPutInteger8ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7312
# #undef MLPutInteger8List
try:
    del MLPutInteger8List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7313
# #undef MLPutIntegerArray
try:
    del MLPutIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7314
# #undef MLPutIntegerArrayData
try:
    del MLPutIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7315
# #undef MLPutIntegerList
try:
    del MLPutIntegerList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7316
# #undef MLPutLongDouble
try:
    del MLPutLongDouble
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7317
# #undef MLPutLongDoubleArray
try:
    del MLPutLongDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7318
# #undef MLPutLongDoubleArrayData
try:
    del MLPutLongDoubleArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7319
# #undef MLPutLongInteger
try:
    del MLPutLongInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7320
# #undef MLPutLongIntegerArray
try:
    del MLPutLongIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7321
# #undef MLPutLongIntegerArrayData
try:
    del MLPutLongIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7322
# #undef MLPutMessage
try:
    del MLPutMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7323
# #undef MLPutMessageWithArg
try:
    del MLPutMessageWithArg
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7324
# #undef MLPutNext
try:
    del MLPutNext
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7325
# #undef MLPutRawData
try:
    del MLPutRawData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7326
# #undef MLPutRawSize
try:
    del MLPutRawSize
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7327
# #undef MLPutReal
try:
    del MLPutReal
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7328
# #undef MLPutReal128
try:
    del MLPutReal128
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7329
# #undef MLPutReal128Array
try:
    del MLPutReal128Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7330
# #undef MLPutReal128ArrayData
try:
    del MLPutReal128ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7331
# #undef MLPutReal128List
try:
    del MLPutReal128List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7332
# #undef MLPutReal32
try:
    del MLPutReal32
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7333
# #undef MLPutReal32Array
try:
    del MLPutReal32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7334
# #undef MLPutReal32ArrayData
try:
    del MLPutReal32ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7335
# #undef MLPutReal32List
try:
    del MLPutReal32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7336
# #undef MLPutReal64
try:
    del MLPutReal64
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7337
# #undef MLPutReal64Array
try:
    del MLPutReal64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7338
# #undef MLPutReal64ArrayData
try:
    del MLPutReal64ArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7339
# #undef MLPutReal64List
try:
    del MLPutReal64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7340
# #undef MLPutRealArray
try:
    del MLPutRealArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7341
# #undef MLPutRealList
try:
    del MLPutRealList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7342
# #undef MLPutRealNumberAsByteString
try:
    del MLPutRealNumberAsByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7343
# #undef MLPutRealNumberAsString
try:
    del MLPutRealNumberAsString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7344
# #undef MLPutRealNumberAsUCS2String
try:
    del MLPutRealNumberAsUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7345
# #undef MLPutRealNumberAsUTF16String
try:
    del MLPutRealNumberAsUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7346
# #undef MLPutRealNumberAsUTF32String
try:
    del MLPutRealNumberAsUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7347
# #undef MLPutRealNumberAsUTF8String
try:
    del MLPutRealNumberAsUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7348
# #undef MLPutShortInteger
try:
    del MLPutShortInteger
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7349
# #undef MLPutShortIntegerArray
try:
    del MLPutShortIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7350
# #undef MLPutShortIntegerArrayData
try:
    del MLPutShortIntegerArrayData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7351
# #undef MLPutSize
try:
    del MLPutSize
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7352
# #undef MLPutString
try:
    del MLPutString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7353
# #undef MLPutSymbol
try:
    del MLPutSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7354
# #undef MLPutType
try:
    del MLPutType
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7355
# #undef MLPutUCS2Characters
try:
    del MLPutUCS2Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7356
# #undef MLPutUCS2Function
try:
    del MLPutUCS2Function
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7357
# #undef MLPutUCS2String
try:
    del MLPutUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7358
# #undef MLPutUCS2Symbol
try:
    del MLPutUCS2Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7359
# #undef MLPutUTF16Characters
try:
    del MLPutUTF16Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7360
# #undef MLPutUTF16Function
try:
    del MLPutUTF16Function
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7361
# #undef MLPutUTF16String
try:
    del MLPutUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7362
# #undef MLPutUTF16Symbol
try:
    del MLPutUTF16Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7363
# #undef MLPutUTF32Characters
try:
    del MLPutUTF32Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7364
# #undef MLPutUTF32Function
try:
    del MLPutUTF32Function
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7365
# #undef MLPutUTF32String
try:
    del MLPutUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7366
# #undef MLPutUTF32Symbol
try:
    del MLPutUTF32Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7367
# #undef MLPutUTF8Characters
try:
    del MLPutUTF8Characters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7368
# #undef MLPutUTF8Function
try:
    del MLPutUTF8Function
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7369
# #undef MLPutUTF8String
try:
    del MLPutUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7370
# #undef MLPutUTF8Symbol
try:
    del MLPutUTF8Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7371
# #undef MLRawBytesToGet
try:
    del MLRawBytesToGet
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7372
# #undef MLReady
try:
    del MLReady
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7373
# #undef MLReadyParallel
try:
    del MLReadyParallel
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7374
# #undef MLRegisterCallbackFunctionWithLinkServer
try:
    del MLRegisterCallbackFunctionWithLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7375
# #undef MLRegisterLinkService
try:
    del MLRegisterLinkService
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7376
# #undef MLRegisterLinkServiceFromLinkServer
try:
    del MLRegisterLinkServiceFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7377
# #undef MLRegisterLinkServiceWithHostname
try:
    del MLRegisterLinkServiceWithHostname
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7378
# #undef MLRegisterLinkServiceWithPortAndHostname
try:
    del MLRegisterLinkServiceWithPortAndHostname
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7379
# #undef MLReleaseBinaryNumberArray
try:
    del MLReleaseBinaryNumberArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7380
# #undef MLReleaseBinaryNumberList
try:
    del MLReleaseBinaryNumberList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7381
# #undef MLReleaseByteArray
try:
    del MLReleaseByteArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7382
# #undef MLReleaseByteString
try:
    del MLReleaseByteString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7383
# #undef MLReleaseByteSymbol
try:
    del MLReleaseByteSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7384
# #undef MLReleaseCompilerID
try:
    del MLReleaseCompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7385
# #undef MLReleaseDomainNameList
try:
    del MLReleaseDomainNameList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7386
# #undef MLReleaseDoubleArray
try:
    del MLReleaseDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7387
# #undef MLReleaseEnvIDString
try:
    del MLReleaseEnvIDString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7388
# #undef MLReleaseErrorMessage
try:
    del MLReleaseErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7389
# #undef MLReleaseFloatArray
try:
    del MLReleaseFloatArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7390
# #undef MLReleaseGetArrayState
try:
    del MLReleaseGetArrayState
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7391
# #undef MLReleaseInteger16Array
try:
    del MLReleaseInteger16Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7392
# #undef MLReleaseInteger16List
try:
    del MLReleaseInteger16List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7393
# #undef MLReleaseInteger32Array
try:
    del MLReleaseInteger32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7394
# #undef MLReleaseInteger32List
try:
    del MLReleaseInteger32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7395
# #undef MLReleaseInteger64Array
try:
    del MLReleaseInteger64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7396
# #undef MLReleaseInteger64List
try:
    del MLReleaseInteger64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7397
# #undef MLReleaseInteger8Array
try:
    del MLReleaseInteger8Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7398
# #undef MLReleaseInteger8List
try:
    del MLReleaseInteger8List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7399
# #undef MLReleaseIntegerArray
try:
    del MLReleaseIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7400
# #undef MLReleaseIntegerList
try:
    del MLReleaseIntegerList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7401
# #undef MLReleaseInterfaceFromLinkServer
try:
    del MLReleaseInterfaceFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7402
# #undef MLReleaseLinkName
try:
    del MLReleaseLinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7403
# #undef MLReleaseLinkProtocolNames
try:
    del MLReleaseLinkProtocolNames
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7404
# #undef MLReleaseLinksFromEnvironment
try:
    del MLReleaseLinksFromEnvironment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7405
# #undef MLReleaseLogFileNameForLink
try:
    del MLReleaseLogFileNameForLink
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7406
# #undef MLReleaseLongDoubleArray
try:
    del MLReleaseLongDoubleArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7407
# #undef MLReleaseLongIntegerArray
try:
    del MLReleaseLongIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7408
# #undef MLReleaseLowLevelDeviceName
try:
    del MLReleaseLowLevelDeviceName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7409
# #undef MLReleaseNetworkAddressList
try:
    del MLReleaseNetworkAddressList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7410
# #undef MLReleaseParameters
try:
    del MLReleaseParameters
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7411
# #undef MLReleasePutArrayState
try:
    del MLReleasePutArrayState
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7412
# #undef MLReleaseReal128Array
try:
    del MLReleaseReal128Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7413
# #undef MLReleaseReal128List
try:
    del MLReleaseReal128List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7414
# #undef MLReleaseReal32Array
try:
    del MLReleaseReal32Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7415
# #undef MLReleaseReal32List
try:
    del MLReleaseReal32List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7416
# #undef MLReleaseReal64Array
try:
    del MLReleaseReal64Array
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7417
# #undef MLReleaseReal64List
try:
    del MLReleaseReal64List
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7418
# #undef MLReleaseRealArray
try:
    del MLReleaseRealArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7419
# #undef MLReleaseRealList
try:
    del MLReleaseRealList
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7420
# #undef MLReleaseShortIntegerArray
try:
    del MLReleaseShortIntegerArray
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7421
# #undef MLReleaseString
try:
    del MLReleaseString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7422
# #undef MLReleaseSymbol
try:
    del MLReleaseSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7423
# #undef MLReleaseUCS2CompilerID
try:
    del MLReleaseUCS2CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7424
# #undef MLReleaseUCS2ErrorMessage
try:
    del MLReleaseUCS2ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7425
# #undef MLReleaseUCS2LinkName
try:
    del MLReleaseUCS2LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7426
# #undef MLReleaseUCS2String
try:
    del MLReleaseUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7427
# #undef MLReleaseUCS2Symbol
try:
    del MLReleaseUCS2Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7428
# #undef MLReleaseUnicodeContainer
try:
    del MLReleaseUnicodeContainer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7429
# #undef MLReleaseUTF16CompilerID
try:
    del MLReleaseUTF16CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7430
# #undef MLReleaseUTF16ErrorMessage
try:
    del MLReleaseUTF16ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7431
# #undef MLReleaseUTF16LinkName
try:
    del MLReleaseUTF16LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7432
# #undef MLReleaseUTF16String
try:
    del MLReleaseUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7433
# #undef MLReleaseUTF16Symbol
try:
    del MLReleaseUTF16Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7434
# #undef MLReleaseUTF32CompilerID
try:
    del MLReleaseUTF32CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7435
# #undef MLReleaseUTF32ErrorMessage
try:
    del MLReleaseUTF32ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7436
# #undef MLReleaseUTF32LinkName
try:
    del MLReleaseUTF32LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7437
# #undef MLReleaseUTF32String
try:
    del MLReleaseUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7438
# #undef MLReleaseUTF32Symbol
try:
    del MLReleaseUTF32Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7439
# #undef MLReleaseUTF8CompilerID
try:
    del MLReleaseUTF8CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7440
# #undef MLReleaseUTF8ErrorMessage
try:
    del MLReleaseUTF8ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7441
# #undef MLReleaseUTF8LinkName
try:
    del MLReleaseUTF8LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7442
# #undef MLReleaseUTF8String
try:
    del MLReleaseUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7443
# #undef MLReleaseUTF8Symbol
try:
    del MLReleaseUTF8Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7444
# #undef MLRequest
try:
    del MLRequest
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7445
# #undef MLRequestArgv
try:
    del MLRequestArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7446
# #undef MLRequestArgvCast
try:
    del MLRequestArgvCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7447
# #undef MLRequestCast
try:
    del MLRequestCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7448
# #undef MLRequestToInteract
try:
    del MLRequestToInteract
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7449
# #undef MLRequestToInteractCast
try:
    del MLRequestToInteractCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7450
# #undef MLResolveLinkService
try:
    del MLResolveLinkService
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7451
# #undef MLScanString
try:
    del MLScanString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7452
# #undef MLSeekMark
try:
    del MLSeekMark
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7453
# #undef MLSeekToMark
try:
    del MLSeekToMark
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7454
# #undef MLServiceProtocolFromReference
try:
    del MLServiceProtocolFromReference
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7455
# #undef MLSetAllocParameter
try:
    del MLSetAllocParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7456
# #undef MLSetDefaultYieldFunction
try:
    del MLSetDefaultYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7457
# #undef MLSetDeviceParameter
try:
    del MLSetDeviceParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7458
# #undef MLSetDialogFunction
try:
    del MLSetDialogFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7459
# #undef MLSetEncodingParameter
try:
    del MLSetEncodingParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7460
# #undef MLSetEnvIDString
try:
    del MLSetEnvIDString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7461
# #undef MLSetEnvironmentData
try:
    del MLSetEnvironmentData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7462
# #undef MLSetError
try:
    del MLSetError
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7463
# #undef MLSetMessageHandler
try:
    del MLSetMessageHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7464
# #undef MLSetName
try:
    del MLSetName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7465
# #undef MLSetResourceParameter
try:
    del MLSetResourceParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7466
# #undef MLSetSignalHandler
try:
    del MLSetSignalHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7467
# #undef MLSetSignalHandlerFromFunction
try:
    del MLSetSignalHandlerFromFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7468
# #undef MLSetSymbolReplacement
try:
    del MLSetSymbolReplacement
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7469
# #undef MLSetThreadSafeLinksParameter
try:
    del MLSetThreadSafeLinksParameter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7470
# #undef MLSetUserBlock
try:
    del MLSetUserBlock
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7471
# #undef MLSetUserData
try:
    del MLSetUserData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7472
# #undef MLSetYieldFunction
try:
    del MLSetYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7473
# #undef MLShutdownLinkServer
try:
    del MLShutdownLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7474
# #undef MLSleepYP
try:
    del MLSleepYP
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7475
# #undef MLStopBrowsingForLinkServices
try:
    del MLStopBrowsingForLinkServices
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7476
# #undef MLStopHandlingSignal
try:
    del MLStopHandlingSignal
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7477
# #undef MLStopLoggingStream
try:
    del MLStopLoggingStream
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7478
# #undef MLStopLoggingStreamToFile
try:
    del MLStopLoggingStreamToFile
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7479
# #undef MLStopRegisteringLinkService
try:
    del MLStopRegisteringLinkService
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7480
# #undef MLStopResolvingLinkService
try:
    del MLStopResolvingLinkService
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7481
# #undef MLStopRegisteringLinkServiceForLink
try:
    del MLStopRegisteringLinkServiceForLink
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7482
# #undef MLStringCharacter
try:
    del MLStringCharacter
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7483
# #undef MLStringFirstPosFun
try:
    del MLStringFirstPosFun
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7484
# #undef MLStringToArgv
try:
    del MLStringToArgv
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7485
# #undef MLTakeLast
try:
    del MLTakeLast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7486
# #undef MLTestHead
try:
    del MLTestHead
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7487
# #undef MLTestHeadWithArgCount
try:
    del MLTestHeadWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7488
# #undef MLTestString
try:
    del MLTestString
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7489
# #undef MLTestSymbol
try:
    del MLTestSymbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7490
# #undef MLTestUCS2Head
try:
    del MLTestUCS2Head
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7491
# #undef MLTestUCS2HeadWithArgCount
try:
    del MLTestUCS2HeadWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7492
# #undef MLTestUCS2String
try:
    del MLTestUCS2String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7493
# #undef MLTestUCS2Symbol
try:
    del MLTestUCS2Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7494
# #undef MLTestUTF16Head
try:
    del MLTestUTF16Head
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7495
# #undef MLTestUTF16HeadWithArgCount
try:
    del MLTestUTF16HeadWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7496
# #undef MLTestUTF16String
try:
    del MLTestUTF16String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7497
# #undef MLTestUTF16Symbol
try:
    del MLTestUTF16Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7498
# #undef MLTestUTF32Head
try:
    del MLTestUTF32Head
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7499
# #undef MLTestUTF32HeadWithArgCount
try:
    del MLTestUTF32HeadWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7500
# #undef MLTestUTF32String
try:
    del MLTestUTF32String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7501
# #undef MLTestUTF32Symbol
try:
    del MLTestUTF32Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7502
# #undef MLTestUTF8Head
try:
    del MLTestUTF8Head
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7503
# #undef MLTestUTF8HeadWithArgCount
try:
    del MLTestUTF8HeadWithArgCount
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7504
# #undef MLTestUTF8String
try:
    del MLTestUTF8String
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7505
# #undef MLTestUTF8Symbol
try:
    del MLTestUTF8Symbol
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7506
# #undef MLToLinkID
try:
    del MLToLinkID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7507
# #undef MLTransferExpression
try:
    del MLTransferExpression
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7508
# #undef MLTransferToEndOfLoopbackLink
try:
    del MLTransferToEndOfLoopbackLink
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7509
# #undef MLUCS2CompilerID
try:
    del MLUCS2CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7510
# #undef MLUCS2ErrorMessage
try:
    del MLUCS2ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7511
# #undef MLUCS2LinkName
try:
    del MLUCS2LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7512
# #undef MLUTF16CompilerID
try:
    del MLUTF16CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7513
# #undef MLUTF16ErrorMessage
try:
    del MLUTF16ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7514
# #undef MLUTF16LinkName
try:
    del MLUTF16LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7515
# #undef MLUTF32CompilerID
try:
    del MLUTF32CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7516
# #undef MLUTF32ErrorMessage
try:
    del MLUTF32ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7517
# #undef MLUTF32LinkName
try:
    del MLUTF32LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7518
# #undef MLUTF8CompilerID
try:
    del MLUTF8CompilerID
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7519
# #undef MLUTF8ErrorMessage
try:
    del MLUTF8ErrorMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7520
# #undef MLUTF8LinkName
try:
    del MLUTF8LinkName
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7521
# #undef MLUnsetSignalHandler
try:
    del MLUnsetSignalHandler
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7522
# #undef MLUserBlock
try:
    del MLUserBlock
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7523
# #undef MLUserCast
try:
    del MLUserCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7524
# #undef MLUserData
try:
    del MLUserData
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7525
# #undef MLValid
try:
    del MLValid
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7526
# #undef MLVersionNumbers
try:
    del MLVersionNumbers
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7527
# #undef MLWaitForLinkActivity
try:
    del MLWaitForLinkActivity
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7528
# #undef MLWaitForLinkActivityWithCallback
try:
    del MLWaitForLinkActivityWithCallback
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7529
# #undef MLWaitForNewLinkFromLinkServer
try:
    del MLWaitForNewLinkFromLinkServer
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7530
# #undef MLYieldFunction
try:
    del MLYieldFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7531
# #undef MLYielderCast
try:
    del MLYielderCast
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7532
# #undef MLinkEnvironment
try:
    del MLinkEnvironment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7594
# #undef MLStringFirstPosFun
try:
    del MLStringFirstPosFun
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7613
# #undef MLREVISION
try:
    del MLREVISION
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7614
# #undef MLAPIREVISION
try:
    del MLAPIREVISION
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7628
# #undef MLTerminateMessage
try:
    del MLTerminateMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7629
# #undef MLInterruptMessage
try:
    del MLInterruptMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7630
# #undef MLAbortMessage
try:
    del MLAbortMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7631
# #undef MLEndPacketMessage
try:
    del MLEndPacketMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7632
# #undef MLSynchronizeMessage
try:
    del MLSynchronizeMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7633
# #undef MLImDyingMessage
try:
    del MLImDyingMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7634
# #undef MLWaitingAcknowledgment
try:
    del MLWaitingAcknowledgment
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7635
# #undef MLMarkTopLevelMessage
try:
    del MLMarkTopLevelMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7636
# #undef MLLinkClosingMessage
try:
    del MLLinkClosingMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7637
# #undef MLAuthenticateFailure
try:
    del MLAuthenticateFailure
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7638
# #undef MLSuspendActivitiesMessage
try:
    del MLSuspendActivitiesMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7639
# #undef MLResumeActivitiesMessage
try:
    del MLResumeActivitiesMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7640
# #undef MLFirstUserMessage
try:
    del MLFirstUserMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7641
# #undef MLLastUserMessage
try:
    del MLLastUserMessage
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7644
# #undef MLAlertFunction
try:
    del MLAlertFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7645
# #undef MLRequestFunction
try:
    del MLRequestFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7646
# #undef MLConfirmFunction
try:
    del MLConfirmFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7647
# #undef MLRequestArgvFunction
try:
    del MLRequestArgvFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7648
# #undef MLRequestToInteractFunction
try:
    del MLRequestToInteractFunction
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7657
# #undef MLREADYPARALLELERROR
try:
    del MLREADYPARALLELERROR
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7658
# #undef MLREADYPARALLELTIMEDOUT
try:
    del MLREADYPARALLELTIMEDOUT
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7659
# #undef MLREADYPARALLELINVALIDARGUMENT
try:
    del MLREADYPARALLELINVALIDARGUMENT
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7663
# #undef MLWAITSUCCESS
try:
    del MLWAITSUCCESS
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7664
# #undef MLWAITERROR
try:
    del MLWAITERROR
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7665
# #undef MLWAITTIMEOUT
try:
    del MLWAITTIMEOUT
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7666
# #undef MLWAITCALLBACKABORTED
try:
    del MLWAITCALLBACKABORTED
except NameError:
    pass

# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 7690
def P(s):
    return s

_wint = struct__wint# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 949

ml_environment = struct_ml_environment# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1327

MLink = struct_MLink# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1333

MLinkMark = struct_MLinkMark# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1338

read_buf = struct_read_buf# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 1419

_i87extended_nt = struct__i87extended_nt# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 2747

MLYieldParams = struct_MLYieldParams# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3351

MLYieldData = struct_MLYieldData# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3355

feature_set = struct_feature_set# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 3745

array_meter = struct_array_meter# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6257

_pointer = union__pointer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6585

_MLUnicodeContainer = struct__MLUnicodeContainer# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6595

_mltimeval = struct__mltimeval# /usr/local/Wolfram/Mathematica/13.2/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions/wstp.h: 6949

# No inserted files

# No prefix-stripping

