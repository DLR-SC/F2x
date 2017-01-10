import ctypes
import source_wrap

bt_new = source_wrap.SOURCE.BASIC_TYPE_new
bt_new.artypes = []
bt_new.restype = ctypes.c_void_p
bt_get_int = source_wrap.SOURCE.BASIC_TYPE_get_INTFIELD
bt_get_int.argtypes = [ctypes.c_void_p]
bt_get_int.restype = ctypes.c_int32
bt_set_int = source_wrap.SOURCE.BASIC_TYPE_set_INTFIELD
bt_set_int.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_int32)]
bt_set_int.restype = None

bt = bt_new()
val = ctypes.c_int32(123)
bt_set_int(bt, ctypes.byref(val))
print(bt_get_int(bt))
