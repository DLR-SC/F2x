# -*- encoding: utf-8 -*-
"""
These tests use the generated interfaces for 'source.f90'. To see details on the exported types and routines, please
see the base source file itself.
"""
import pytest

import F2x
from F2x_test.fortran import source_glue as src
from F2x.lib.python.array import F2INTEGERArray, F2REALArray


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
    assert bt.INTARRAY[:] == [1, 2, 3]


def test_basic_type_intarray_init():
    bt = src.BASIC_TYPE(INTARRAY=[1, 2, 3])
    assert bt.INTARRAY[:] == [1, 2, 3]


def test_basic_type_realarray():
    bt = src.BASIC_TYPE()
    bt.REALARRAY = [1.2, 3.4, 5.6, 7.8]
    assert len(bt.REALARRAY) == 4
    assert bt.REALARRAY[:] == [1.2, 3.4, 5.6, 7.8]


def test_basic_type_realarray_init():
    bt = src.BASIC_TYPE(REALARRAY=[2.3, 4.5])
    assert bt.REALARRAY[:] == [2.3, 4.5]


def test_basic_type_realarray_indexerror():
    bt = src.BASIC_TYPE(REALARRAY=[2.3, 4.5])
    with pytest.raises(IndexError):
        bt.REALARRAY[3] = 3.2


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


def test_compound_type_basicarray():
    ct = src.COMPOUND_TYPE()
    ct.BASICARRAY.alloc(2)
    ct.BASICARRAY[0].INTFIELD = 1
    ct.BASICARRAY[1].REALFIELD = 2.3
    assert ct.BASICARRAY[0].INTFIELD == 1
    assert ct.BASICARRAY[1].REALFIELD == 2.3


# The following test will fail with invalid memory error. This is a problem of the
# template...
@pytest.mark.skipif("F2x.version < 0x10")
def test_compound_type_basicarray_init():
    bt = src.BASIC_TYPE(INTFIELD=5)
    ba = [bt]
    ct = src.COMPOUND_TYPE(BASICARRAY=ba)
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
    assert outarray == [4, 5, 6]
    assert inoutarray == [7, 7, 8]


def test_ndarray_args():
    a, b = F2INTEGERArray(1, 2), F2INTEGERArray(3, 4, 5)
    c, d = src.BASIC_ARGS_NDARRAY(a, b)
    print(a, b, c, d)
    

def test_string_args():
    outstr, inoutstr = src.STRING_ARGS("in", "inout")
    assert outstr == "inout"
    assert inoutstr == "in"


def test_derived_type_args():
    a = src.BASIC_TYPE(INTFIELD=2)
    b = src.BASIC_TYPE(REALFIELD=3.4)
    c, d = src.DERIVED_TYPE_ARGS(a, b)
    assert a.INTFIELD == b.INTFIELD
    assert b.REALFIELD == c.REALFIELD
    assert b.REALFIELD == d.REALFIELD
    assert b._ptr == d._ptr


def test_basic_return_value():
    assert src.BASIC_RETURN_VALUE() == 123


def test_derived_type_return_value():
    res = src.DERIVED_TYPE_RETURN_VALUE()
    assert type(res) is src.BASIC_TYPE
    assert len(res.REALARRAY) == 2
    assert abs(res.REALARRAY[0] - 1.2) < 0.1
    assert abs(res.REALARRAY[1] - 3.4) < 0.1


def test_string_return_value():
    assert src.STRING_RETURN_VALUE() == "Foo Bar"


def test_array_return_value():
    assert src.ARRAY_RETURN_VALUE() == [1, 2, 3]
