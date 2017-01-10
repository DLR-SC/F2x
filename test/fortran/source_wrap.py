


import ctypes
import os

import numpy

from F2x.lib.python.ftype import FType, Field, getter, setter, constructor, destructor, allocator

_lib = 'libSOURCE.so'
_path = os.path.join(os.path.dirname(__file__), _lib)

SOURCE = ctypes.cdll.LoadLibrary(_path)


class BASIC_TYPE(FType):
    _new = constructor(SOURCE.BASIC_TYPE_new)
    _free = destructor(SOURCE.BASIC_TYPE_free)
    INTFIELD = Field(
        getter(ctypes.c_int, SOURCE.BASIC_TYPE_get_INTFIELD),
        setter(ctypes.c_int, SOURCE.BASIC_TYPE_set_INTFIELD)
    )
    REALFIELD = Field(
        getter(ctypes.c_double, SOURCE.BASIC_TYPE_get_REALFIELD),
        setter(ctypes.c_double, SOURCE.BASIC_TYPE_set_REALFIELD)
    )
    LOGICALFIELD = Field(
        getter(ctypes.c_bool, SOURCE.BASIC_TYPE_get_LOGICALFIELD),
        setter(ctypes.c_bool, SOURCE.BASIC_TYPE_set_LOGICALFIELD)
    )
    CHARFIELD = Field(
        getter(ctypes.c_char_p, SOURCE.BASIC_TYPE_get_CHARFIELD),
        setter(ctypes.c_char_p, SOURCE.BASIC_TYPE_set_CHARFIELD)
    )
    INTARRAY_dims = [3]
    INTARRAY = Field(
        getter(ctypes.c_int, SOURCE.BASIC_TYPE_get_INTARRAY, dims=INTARRAY_dims),
        setter(ctypes.c_int, SOURCE.BASIC_TYPE_get_INTARRAY, dims=INTARRAY_dims),
    )
    REALARRAY_dims = [0]
    REALARRAY = Field(
        getter(ctypes.c_double, SOURCE.BASIC_TYPE_get_REALARRAY, dims=REALARRAY_dims),
        setter(ctypes.c_double, SOURCE.BASIC_TYPE_get_REALARRAY, dims=REALARRAY_dims),
        allocator(SOURCE.BASIC_TYPE_alloc_REALARRAY, dims=REALARRAY_dims)
    )
    LOGICALARRAY_dims = [0]
    LOGICALARRAY = Field(
        getter(ctypes.c_bool, SOURCE.BASIC_TYPE_get_LOGICALARRAY, dims=LOGICALARRAY_dims),
        setter(ctypes.c_bool, SOURCE.BASIC_TYPE_get_LOGICALARRAY, dims=LOGICALARRAY_dims),
        allocator(SOURCE.BASIC_TYPE_alloc_LOGICALARRAY, dims=LOGICALARRAY_dims)
    )

class COMPOUND_TYPE(FType):
    _new = constructor(SOURCE.COMPOUND_TYPE_new)
    _free = destructor(SOURCE.COMPOUND_TYPE_free)
    BASICFIELD = Field(
        getter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_BASICFIELD),
        setter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_BASICFIELD),
    )
    ALLOCATEFIELD = Field(
        getter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_ALLOCATEFIELD),
        setter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_ALLOCATEFIELD),
        allocator(SOURCE.COMPOUND_TYPE_alloc_ALLOCATEFIELD)
    )
    POINTERFIELD = Field(
        getter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_POINTERFIELD),
        setter(BASIC_TYPE, SOURCE.COMPOUND_TYPE_get_POINTERFIELD),
        allocator(SOURCE.COMPOUND_TYPE_alloc_POINTERFIELD)
    )
    BASICARRAY_dims = [0]
    BASICARRAY = Field(
        getter(ctypes.c_void_p, SOURCE.COMPOUND_TYPE_get_BASICARRAY, dims=BASICARRAY_dims),
        setter(ctypes.c_void_p, SOURCE.COMPOUND_TYPE_get_BASICARRAY, dims=BASICARRAY_dims),
        allocator(SOURCE.COMPOUND_TYPE_alloc_BASICARRAY, dims=BASICARRAY_dims)
    )


########################################################################################################################
# Exported methods.

# BASIC_ARGS_IN
SOURCE.SRC_BAI.restype = None
SOURCE.SRC_BAI.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_bool), ]
def BASIC_ARGS_IN(ININT, INREAL, INLOGICAL):
    
    ININT_INTERN = ctypes.c_int(ININT)
    INREAL_INTERN = ctypes.c_double(INREAL)
    INLOGICAL_INTERN = ctypes.c_bool(INLOGICAL)
    SOURCE.SRC_BAI(ctypes.byref(ININT_INTERN), ctypes.byref(INREAL_INTERN), ctypes.byref(INLOGICAL_INTERN))
    

# BASIC_ARGS_OUT
SOURCE.SRC_BAO.restype = None
SOURCE.SRC_BAO.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_bool), ]
def BASIC_ARGS_OUT():
    
    OUTINT_INTERN = ctypes.c_int()
    OUTREAL_INTERN = ctypes.c_double()
    OUTLOGICAL_INTERN = ctypes.c_bool()
    SOURCE.SRC_BAO(ctypes.byref(OUTINT_INTERN), ctypes.byref(OUTREAL_INTERN), ctypes.byref(OUTLOGICAL_INTERN))
    
        
        
        
    return OUTINT_INTERN.value, OUTREAL_INTERN.value, OUTLOGICAL_INTERN.value

# BASIC_ARGS_INOUT
SOURCE.SRC_BAIO.restype = None
SOURCE.SRC_BAIO.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_bool), ]
def BASIC_ARGS_INOUT(INOUTINT, INOUTREAL, INOUTLOGICAL):
    
    INOUTINT_INTERN = ctypes.c_int(INOUTINT)
    INOUTREAL_INTERN = ctypes.c_double(INOUTREAL)
    INOUTLOGICAL_INTERN = ctypes.c_bool(INOUTLOGICAL)
    SOURCE.SRC_BAIO(ctypes.byref(INOUTINT_INTERN), ctypes.byref(INOUTREAL_INTERN), ctypes.byref(INOUTLOGICAL_INTERN))
    
        
        
        
    return INOUTINT_INTERN.value, INOUTREAL_INTERN.value, INOUTLOGICAL_INTERN.value

# BASIC_ARGS_ARRAY
SOURCE.SRC_BAA.restype = None
SOURCE.SRC_BAA.argtypes = [ctypes.POINTER(ctypes.POINTER(ctypes.c_int)), ctypes.POINTER(ctypes.POINTER(ctypes.c_int)), ctypes.POINTER(ctypes.POINTER(ctypes.c_int)), ]
def BASIC_ARGS_ARRAY(INARRAY, INOUTARRAY):
    
    INARRAY_ARRAY = numpy.array(INARRAY, ctypes.c_int, order='F')
    INARRAY_INTERN = INARRAY_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_int))
    OUTARRAY_ARRAY = numpy.empty((3, ), dtype=ctypes.c_int, order='F')
    OUTARRAY_INTERN = OUTARRAY_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_int))
    INOUTARRAY_ARRAY = numpy.array(INOUTARRAY, ctypes.c_int, order='F')
    INOUTARRAY_INTERN = INOUTARRAY_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_int))
    SOURCE.SRC_BAA(ctypes.byref(INARRAY_INTERN), ctypes.byref(OUTARRAY_INTERN), ctypes.byref(INOUTARRAY_INTERN))
    
        
        
    return OUTARRAY_ARRAY, INOUTARRAY_ARRAY

# BASIC_ARGS_NDARRAY
SOURCE.SRC_BAN.restype = None
SOURCE.SRC_BAN.argtypes = [ctypes.POINTER(ctypes.POINTER(ctypes.c_int)), ctypes.POINTER(ctypes.POINTER(ctypes.c_double)), ctypes.POINTER(ctypes.POINTER(ctypes.c_int)), ]
def BASIC_ARGS_NDARRAY(INARRAY2D, INOUTARRAY2D):
    
    INARRAY2D_ARRAY = numpy.array(INARRAY2D, ctypes.c_int, order='F')
    INARRAY2D_INTERN = INARRAY2D_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_int))
    OUTARRAY3D_ARRAY = numpy.empty((1, 2, 3, ), dtype=ctypes.c_double, order='F')
    OUTARRAY3D_INTERN = OUTARRAY3D_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_double))
    INOUTARRAY2D_ARRAY = numpy.array(INOUTARRAY2D, ctypes.c_int, order='F')
    INOUTARRAY2D_INTERN = INOUTARRAY2D_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_int))
    SOURCE.SRC_BAN(ctypes.byref(INARRAY2D_INTERN), ctypes.byref(OUTARRAY3D_INTERN), ctypes.byref(INOUTARRAY2D_INTERN))
    
        
        
    return OUTARRAY3D_ARRAY, INOUTARRAY2D_ARRAY

# STRING_ARGS
SOURCE.SRC_BSA.restype = None
SOURCE.SRC_BSA.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p), ]
def STRING_ARGS(INSTR, INOUTSTR):
    
    INSTR_INTERN = ctypes.c_char_p(INSTR.encode("utf8"))
    OUTSTR_INTERN = ctypes.c_char_p()
    INOUTSTR_BUFFER = ctypes.create_string_buffer(INOUTSTR.encode("utf8"), 32)
    INOUTSTR_INTERN = ctypes.cast(INOUTSTR_BUFFER, ctypes.c_char_p)
    SOURCE.SRC_BSA(ctypes.byref(INSTR_INTERN), ctypes.byref(OUTSTR_INTERN), ctypes.byref(INOUTSTR_INTERN))
    
        
        
    return OUTSTR_INTERN.value.decode("utf8"), INOUTSTR_INTERN.value.decode("utf8")

# DERIVED_TYPE_ARGS
SOURCE.SRC_DTA.restype = None
SOURCE.SRC_DTA.argtypes = [ctypes.POINTER(ctypes.c_void_p), ctypes.POINTER(ctypes.c_void_p), ctypes.POINTER(ctypes.c_void_p), ]
def DERIVED_TYPE_ARGS(INTYPE, INOUTTYPE):
    
    INTYPE_INTERN = ctypes.c_void_p(INTYPE.ptr)
    OUTTYPE = BASIC_TYPE()
    OUTTYPE_INTERN = ctypes.c_void_p(OUTTYPE.ptr)
    INOUTTYPE_INTERN = ctypes.c_void_p(INOUTTYPE.ptr)
    SOURCE.SRC_DTA(ctypes.byref(INTYPE_INTERN), ctypes.byref(OUTTYPE_INTERN), ctypes.byref(INOUTTYPE_INTERN))
    
        
    # BASIC_TYPE OUTTYPE
        
    # BASIC_TYPE INOUTTYPE

# BASIC_RETURN_VALUE
SOURCE.SRC_BRV.restype = ctypes.c_int
SOURCE.SRC_BRV.argtypes = []
def BASIC_RETURN_VALUE():
    
    SRC_BRV = SOURCE.SRC_BRV()
    
    
    return SRC_BRV

# DERIVED_TYPE_RETURN_VALUE
SOURCE.SRC_DTRV.restype = ctypes.c_void_p
SOURCE.SRC_DTRV.argtypes = []
def DERIVED_TYPE_RETURN_VALUE():
    
    SRC_DTRV = SOURCE.SRC_DTRV()
    
    # BASIC_TYPE SRC_DTRV
    

# STRING_RETURN_VALUE
SOURCE.SRC_SRV.restype = None
SOURCE.SRC_SRV.argtypes = [ctypes.POINTER(ctypes.c_char_p)]
def STRING_RETURN_VALUE():
    
    
    SRC_SRV_OUT_INTERN = ctypes.c_char_p()
    SOURCE.SRC_SRV(ctypes.byref(SRC_SRV_OUT_INTERN))
    
    
    return SRC_SRV_OUT_INTERN.value.decode("utf8")

# ARRAY_RETURN_VALUE
SOURCE.SRC_ARV.restype = ctypes.POINTER(ctypes.c_int)
SOURCE.SRC_ARV.argtypes = []
def ARRAY_RETURN_VALUE():
    
    SRC_ARV = SOURCE.SRC_ARV()
    SRC_ARV_CARRAY = array_from_pointer(ctypes.c_int, [3], SRC_ARV)
    SRC_ARV_ARRAY = numpy.ndarray((3, ), dtype=ctypes.c_int, buffer=SRC_ARV_CARRAY, order='F')
    
    return SRC_ARV_ARRAY