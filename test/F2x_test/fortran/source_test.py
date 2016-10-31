import pytest

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


def test_compound_type_basicarray_init():
    bt = src.BASIC_TYPE(INTFIELD=5)
    with pytest.raises(NotImplementedError):
        src.COMPOUND_TYPE(BASICARRAY=[bt])
