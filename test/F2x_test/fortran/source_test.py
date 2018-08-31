# -*- encoding: utf-8 -*-
#
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
"""
These tests use the generated interfaces for 'source.f90'. To see details on the exported types and routines, please
see the base source file itself.
"""
import pytest

import numpy

import F2x
from .glue import F2xError
from F2x_test.fortran import source_glue as src


def test_basic_type_intfield():
    bt = src.BASIC_TYPE()
    bt.INTFIELD = 3
    assert bt.INTFIELD == 3


def test_basic_type_realfield():
    bt = src.BASIC_TYPE()
    bt.REALFIELD = 3.4
    assert bt.REALFIELD == 3.4


def test_basic_type_logicalfield_true():
    bt = src.BASIC_TYPE()
    bt.LOGICALFIELD = True
    assert bt.LOGICALFIELD == True


def test_basic_type_logicalfield_false():
    bt = src.BASIC_TYPE()
    bt.LOGICALFIELD = False
    assert bt.LOGICALFIELD == False


def test_basic_type_charfield():
    bt = src.BASIC_TYPE()
    bt.CHARFIELD = "test"
    assert bt.CHARFIELD == "test"


def test_basic_type_init():
    bt = src.BASIC_TYPE(INTFIELD=2, REALFIELD=3.4, LOGICALFIELD=False)
    assert bt.INTFIELD == 2
    assert bt.REALFIELD == 3.4
    assert bt.LOGICALFIELD == False


def test_basic_type_intarray():
    bt = src.BASIC_TYPE()
    bt.INTARRAY = [1, 2, 3]
    assert numpy.array_equal(bt.INTARRAY, [1, 2, 3])


def test_basic_type_intarray_init():
    bt = src.BASIC_TYPE(INTARRAY=[1, 2, 3])
    assert numpy.array_equal(bt.INTARRAY, [1, 2, 3])


def test_basic_type_realarray():
    bt = src.BASIC_TYPE()
    bt.REALARRAY = [1.2, 3.4, 5.6, 7.8]
    assert len(bt.REALARRAY) == 4
    assert numpy.array_equal(bt.REALARRAY, [1.2, 3.4, 5.6, 7.8])


def test_basic_type_realarray_init():
    bt = src.BASIC_TYPE(REALARRAY=[2.3, 4.5])
    assert numpy.array_equal(bt.REALARRAY, [2.3, 4.5])


def test_basic_type_realarray_indexerror():
    bt = src.BASIC_TYPE(REALARRAY=[2.3, 4.5])
    with pytest.raises(IndexError):
        bt.REALARRAY[3] = 3.2


def test_basic_type_stringarray():
    bt = src.BASIC_TYPE()
    bt.STRINGARRAY.allocate(3)
    bt.STRINGARRAY[0] = "Foo"
    assert bt.STRINGARRAY[0] == "Foo"


def test_compound_type_basicfield():
    ct = src.COMPOUND_TYPE()
    ct.BASICFIELD.INTFIELD = 3
    ct.BASICFIELD.REALARRAY = [1.2]
    assert ct.BASICFIELD.INTFIELD == 3
    assert ct.BASICFIELD.REALARRAY[0] == 1.2


def test_compound_type_allocatablefield():
    ct = src.COMPOUND_TYPE()
    ct.ALLOCATEFIELD.INTFIELD = 3
    ct.ALLOCATEFIELD.REALARRAY = [1.2]
    assert ct.ALLOCATEFIELD.INTFIELD == 3
    assert ct.ALLOCATEFIELD.REALARRAY[0] == 1.2


def test_compound_type_pointerfield():
    ct = src.COMPOUND_TYPE()
    ct.POINTERFIELD.INTFIELD = 3
    ct.POINTERFIELD.REALARRAY = [1.2]
    assert ct.POINTERFIELD.INTFIELD == 3
    assert ct.POINTERFIELD.REALARRAY[0] == 1.2


def test_compund_type_pointerfield_assign():
    ct = src.COMPOUND_TYPE()
    bt = src.BASIC_TYPE(INTFIELD=1, ARRAYFIELD=[1, 2, 3])
    ct.POINTERFIELD = bt
    assert ct.POINTERFIELD.INTFIELD == 1


# The following test will fail with invalid memory error. This is a problem of the
# template...
@pytest.mark.skipif("F2x.VERSION < 0x10")
def test_compound_type_basicarray():
    ct = src.COMPOUND_TYPE()
    ba = [src.BASIC_TYPE(INTFIELD=1, CHARFIELD="INT"), src.BASIC_TYPE(REALFIELD=2.3, CHARFIELD="REAL")]
    ct.BASICARRAY = ba
    assert ct.BASICARRAY[0].INTFIELD == 1
    assert ct.BASICARRAY[1].REALFIELD == 2.3


# The following test will fail with invalid memory error. This is a problem of the
# template...
@pytest.mark.skipif("F2x.VERSION < 0x10")
def test_compound_type_basicarray_init():
    bt = src.BASIC_TYPE(INTFIELD=5)
    ba = [bt]
    ct = src.COMPOUND_TYPE(BASICARRAY=ba)
    assert len(ct.BASICARRAY) == 1
    assert ct.BASICARRAY[0].INTFIELD == 5


def test_basic_args_in():
    src.BASIC_ARGS_IN(1, 2.3, True)


def test_basic_args_inout():
    intarg, realarg, logicalarg = src.BASIC_ARGS_INOUT(1, 2.3, True)
    assert intarg == 2
    assert realarg == 1.15
    assert logicalarg == False


def test_basic_args_out():
    intarg, realarg, logicalarg = src.BASIC_ARGS_OUT()
    assert intarg == 1
    assert abs(realarg - 2.3) < 0.1
    assert logicalarg == True


def test_array_args():
    outarray, inoutarray = src.BASIC_ARGS_ARRAY([1, 2, 3], [6, 7, 8])
    assert numpy.array_equal(outarray, [4, 5, 6])
    assert numpy.array_equal(inoutarray, [7, 7, 8])


def test_ndarray_args():
    a, b = [[[1, 2, 3], [4, 5, 6]]], [[7, 8], [9, 0]]
    c, d = src.BASIC_ARGS_NDARRAY(a, b)
    assert numpy.array_equal(c.shape, (1, 2, 3))
    assert abs(c[0, 0, 1] - 4.3) < 0.1
    assert numpy.array_equal([[3, 2], [9, 0]], d)


def test_string_args():
    outstr, inoutstr = src.STRING_ARGS("in", "inout")
    assert outstr == "inout"
    assert inoutstr == "in"


def test_derived_type_args():
    a = src.BASIC_TYPE(INTFIELD=2, CHARFIELD="INT")
    b = src.BASIC_TYPE(REALFIELD=3.4, CHARFIELD="REAL")
    c, d = src.DERIVED_TYPE_ARGS(a, b)
    assert a.INTFIELD == b.INTFIELD
    assert b.REALFIELD == c.REALFIELD
    assert b.ptr == d.ptr


def test_basic_return_value():
    assert src.BASIC_RETURN_VALUE() == 123


def test_derived_type_return_value():
    res = src.DERIVED_TYPE_RETURN_VALUE()
    assert type(res) is src.BASIC_TYPE
    assert len(res.REALARRAY) == 2
    assert abs(res.REALARRAY[0] - 1.2) < 0.1
    assert abs(res.REALARRAY[1] - 3.4) < 0.1


def test_string_return_value():
    assert src.STRING_RETURN_VALUE().rstrip() == "Foo Bar"


@pytest.mark.skip
def test_array_return_value():
    assert numpy.array_equal(src.ARRAY_RETURN_VALUE(), [1, 2, 3])


def test_global_value():
    assert src.globals.BASICS_READY == False
    src.globals.BASICS_READY = True
    assert src.globals.BASICS_READY == True


def test_global_array():
    assert len(src.globals.BASICS) == 0

    src.globals.BASICS.allocate(2)
    assert len(src.globals.BASICS) == 2

    src.globals.BASICS[0].INTFIELD = 1
    src.globals.BASICS[1].REALFIELD = 2.3
    assert src.globals.BASICS[0].INTFIELD == 1
    assert abs(src.globals.BASICS[1].REALFIELD - 2.3) < 0.01
