'''
This module contains helper classes that can be used to access Fortran types that have been wrapped with F2x.

@author: Michael Meinel <michael.meinel@dlr.de>
'''

class Array(object):
    def __init__(self, ctype, init_func, size_func, getter):
        self.ctype = ctype
        self.init_func = init_func
        self.size_func = size_func
        self.getter = getter
    
    def __get__(self, instance, owner):
        if instance is None:
            return self
        return Array.Reader(self, instance)
    
    class Reader(object):
        def __init__(self, accessor, instance):
            self._accessor = accessor
            self._instance = instance
            address = self._accessor.getter(self._instance._ptr)
            if address is not None:
                arrtype = self._accessor.ctype * self._accessor.size_func(self._instance._ptr)
                self._carray = arrtype.from_address(address)
            else:
                self._carray = None
        
        def __getitem__(self, index):
            if self._carray is None:
                raise ValueError("Array not allocated.")
            elif index < 0 or index >= len(self._carray):
                raise IndexError()
            else:
                return self._carray[index]
        
        def __setitem__(self, index, value):
            if self._carray is None:
                raise ValueError("Array not allocated.")
            elif index < 0 or index >= len(self._carray):
                raise IndexError()
            else:
                self._carray[index] = value

        def len(self):
            if self._carray is None:
                raise ValueError("Array not allocated.")
            else:
                return len(self._carray)
        
        def allocate(self, size):
            self._accessor.init_func(self._instance._ptr, size)
            arrtype = self._accessor.ctype * size
            self._carray = arrtype.from_address(self._accessor.getter(self._instance._ptr))

class DTArray(object):
    def __init__(self, arrtype, init_func, size_func, item_func):
        self.arrtype = arrtype
        self.init_func = init_func
        self.size_func = size_func
        self.item_func = item_func
    
    def __get__(self, instance, owner):
        if instance is None:
            return self
        return DTArray.Reader(self, instance)
    
    class Reader(object):
        def __init__(self, accessor, instance):
            self._accessor = accessor
            self._instance = instance

        def __getitem__(self, index):
            if index >= 0 and index < self.len():
                return self._accessor.arrtype(self._accessor.item_func(self._instance._ptr, index))
            else:
                raise IndexError()
        
        def len(self):
            return self._accessor.size_func(self._instance._ptr)
        
        def allocate(self, size):
            self._accessor.init_func(self._instance._ptr, size)
