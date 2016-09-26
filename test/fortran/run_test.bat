@echo off
set OLD_PATH=%PATH%
set PATH=C:\MinGW\bin;C:\MinGW\msys\1.0\bin;%OLD_PATH%
set OLD_PYTHONPATH=%PYTHONPATH%
set PYTHONPATH=..\..\src;%OLD_PYTHONPATH%

@echo "Generating interfaces..."
:: D:\Python27\python.exe -m F2x.main -t @_bindc.f90.t -t @_ctypes.py.t -P mathlib.f90
:: D:\Python27\python.exe -m F2x.main -t @_bindc.f90.t -P mathlib.f90

@echo "Compiling module..."
gfortran -g -shared -o libmath.so ..\..\src\F2x\template\wrap_util.f90 mathlib.f90 mathlib_bindc.f90

@echo "Running tests..."
:: gdb -ex run --args D:\Python27\python.exe test.py
D:\Python27\python.exe test.py

set PATH=%OLD_PATH%
set PYTHONPATH=%OLD_PYTHONPATH%
