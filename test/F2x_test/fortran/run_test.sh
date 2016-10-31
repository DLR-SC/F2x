#!/bin/sh
OLD_PYTHONPATH=$PYTHONPATH
export PYTHONPATH=$PYTHONPATH:$PWD/../../src

python -m F2x.main -t @_glue.f90.t -t @_glue.py.t source.f90
gfortran -fPIC -shared -g -o libSOURCE.so ../../src/F2x/template/wrap_util.f90 source.f90 source_glue.f90
pytest source_test.py
