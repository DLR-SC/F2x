import ctypes

import numpy


def constructor(cfunc):
    cfunc.argtypes = []
    cfunc.restype = ctypes.c_void_p
    return staticmethod(cfunc)


def destructor(cfunc):
    cfunc.argtypes = [ctypes.c_void_p]
    cfunc.restype = None
    return staticmethod(cfunc)


c_int32_p = ctypes.POINTER(ctypes.c_int32)


def allocator(cfunc, dims=None):
    if dims is None:
        cfunc.argtypes = [ctypes.c_void_p]
        cfunc.restype = None
        return cfunc

    else:
        csizes_type = (ctypes.c_int32 * len(dims))

        def alloc_func(ptr, *sizes):
            print(sizes, dims)
            if len(sizes) != len(dims):
                raise ValueError("Array size has invalid dimensions.")

            csizes = csizes_type(*sizes)
            csizes_ptr = ctypes.cast(csizes, c_int32_p)
            cfunc(ptr, ctypes.byref(csizes_ptr))
            dims[:] = sizes

        cfunc.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_int32))]
        cfunc.restype = None
        return alloc_func


def array_from_pointer(ctype, dims, ptr):
    array_size = 1
    for size in dims:
        array_size *= size

    array_type = ctype * array_size
    return array_type.from_address(ctypes.addressof(ptr.contents))


def getter(ctype, cfunc, dims=None):
    if isinstance(ctype, FType):
        def get_func(ptr):
            fptr = cfunc(ptr)
            return ctype(fptr, False)

        cfunc.argtypes = [ctypes.c_void_p]
        cfunc.restype = ctypes.c_void_p
        return get_func

    elif ctype == ctypes.c_char_p:
        def get_func(ptr):
            cptr = ctypes.c_char_p()
            cfunc(ptr, ctypes.byref(cptr))
            try:
                return cptr.value.decode('utf-8')
            except UnicodeDecodeError:
                return ""

        cfunc.argtypes = [ctypes.c_void_p]
        cfunc.restype = ctypes.c_char_p
        return get_func

    elif dims:
        def get_func(ptr):
            cptr = ctypes.POINTER(ctype)()
            cptr_address = ctypes.pointer(cptr)

            cfunc(ptr, cptr_address)
            carray = array_from_pointer(ctype, dims, cptr)
            return numpy.ndarray(dims, dtype=ctype, buffer=carray, order='F')

        cfunc.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.POINTER(ctype))]
        cfunc.restype = None
        return get_func

    else:
        cfunc.argtypes = [ctypes.c_void_p]
        cfunc.restype = ctype
        return cfunc


class NullPointerError(BaseException):
    pass


def setter(ctype, cfunc, dims=None):
    if dims is not None:
        def set_func(ptr, value):
            value = numpy.array(value, order='F')
            cptr = ctypes.POINTER(ctype)()
            cfunc(ptr, ctypes.byref(cptr))
            try:
                array = array_from_pointer(ctype, dims, cptr)
            except ValueError:
                raise NullPointerError()
            array[:] = value

        return set_func

    elif issubclass(ctype, FType):
        def set_func(ptr, value):
            instance = cfunc(ptr)
            for name, field in ctype.fields():
                if field.setter:
                    val = getattr(value, name)
                    setattr(instance, name, val)

        return set_func

    elif ctype == ctypes.c_char_p:
        def set_func(ptr, value):
            cvalue = ctypes.create_string_buffer(value.encode("ascii"))
            cptr = ctypes.cast(cvalue, ctypes.c_char_p)
            cfunc(ptr, ctypes.byref(cptr))

        cfunc.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p)]
        cfunc.restype = None
        return set_func

    else:
        def set_func(ptr, value):
            cvalue = ctype(value)
            cfunc(ptr, ctypes.byref(cvalue))

        cfunc.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctype)]
        cfunc.restype = None
        return set_func


class FType(object):
    _new = None
    _free = None

    def __init__(self, ptr=None, owned=None):
        if ptr is None:
            self.ptr = self._new()
            self.owned = True if owned is None else owned
        else:
            self.ptr = ptr
            self.owned = owned or False

    @classmethod
    def fields(cls):
        for name, field in cls.__dict__.items():
            if isinstance(field, Field):
                yield name, field


class Field(object):
    def __init__(self, getter, setter, allocator=None):
        self.getter = getter
        self.setter = setter
        self.allocator = allocator

    def __get__(self, instance, owner):
        if instance is None:
            return self

        if self.allocator:
            try:
                self.allocator(instance.ptr)
            except ValueError:
                pass

        return self.getter(instance.ptr)

    def __set__(self, instance, value):
        if self.setter is None:
            raise AttributeError("Field not settable.")

        if self.allocator:
            try:
                self.allocator(instance.ptr)
            except ValueError:
                pass

        try:
            self.setter(instance.ptr, value)
        except NullPointerError:
            if self.allocator:
                value = numpy.array(value, order='F')
                self.allocator(instance.ptr, *value.shape)
                self.setter(instance.ptr, value)
