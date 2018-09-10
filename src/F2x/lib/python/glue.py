# Copyright 2018 German Aerospace Center (DLR)
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
'''
This module contains helper classes that can be used to access Fortran types that have been wrapped with F2x.

@author: Michael Meinel <michael.meinel@dlr.de>
'''


class DerivedType(object):
    _new = None
    _free = None

    def __init__(self, ptr=None, owned=False, **kw):
        if ptr is not None:
            self._ptr = ptr
            self._owned = owned
        else:
            self._ptr = self._new()
            self._owned = True

        for name, value in kw.items():
            setattr(self, name, value)

    def __del__(self):
        if self._owned:
            self._free(self._ptr)
            self._ptr = None

    def __repr__(self):
        return "<{0} at {1:X}>".format(self.__class__.__name__, self._ptr)

    def call(self, method, *args):
        return method(self._ptr, *args)


class _DynamicArray(object):
    def __init__(self, array, instance):
        self._array = array
        self._instance = instance

    def alloc(self, size):
        return self._array.resize(self._instance, size)

    def __len__(self):
        return self._array.size(self._instance)


class DynamicArray(object):
    def __init__(self, typ, init, clear, size, get):
        self._type = typ
        self._init = init
        self._clear = clear
        self._size = size
        self._get = get

    def size(self, instance):
        return instance.call(self._size)

    def make_array(self, instance):
        array_size = instance.call(self._size)
        array_type = self._type * array_size
        address = instance.call(self._get)
        return array_type.from_address(address)

    def resize(self, instance, size):
        old_size = instance.call(self._size)
        if old_size and not size == old_size:
            instance.call(self._clear)

        instance.call(self._init, size)
        return self.make_array(instance)

    def __get__(self, instance, owner):
        if instance is None:
            return self
        else:
            array_size = instance.call(self._size)
            if array_size > 0:
                return self.make_array(instance)
            else:
                return _DynamicArray(self, instance)

    def __set__(self, instance, value):
        array_size = instance.call(self._size)
        if array_size and not array_size == len(value):
            instance.call(self._clear)
        instance.call(self._init, len(value))
        array = self.make_array(instance)
        array[:] = value


class DerivedTypeArray(DynamicArray):
    class Accessor(object):
        def __init__(self, array, instance):
            self._array = array
            self._instance = instance

        def __len__(self):
            return self._array.size(self._instance)

        def __getitem__(self, index):
            if type(index) is slice:
                return (self[i] for i in slice.indices(len(self)))
            elif index < 0 or index >= len(self):
                raise IndexError(index)
            else:
                return self._array.getitem(self._instance, index)

        def __setitem__(self, index, value):
            if type(index) is slice:
                for i, v in zip(index.indices(len(self)), value):
                    self[i] = v
            elif index < 0 or index >= len(self):
                raise IndexError(index)
            else:
                item = self._array.getitem(self._instance, index)
                item.copy_from(value)

    def getitem(self, instance, index):
        address = instance.call(self._get, index)
        return self._type(address)

    def make_array(self, instance):
        return DerivedTypeArray.Accessor(self, instance)

if __name__ == '__main__':
    import os
    path = os.path.dirname(__file__)
    print(path)
