# Makefile for test modules.
#

# Tool configuration
CP = cp
FC = gfortran -fPIC -g
CSC = C:/Progra~2/MSBuild/14.0/Bin/csc.exe
F2X = python -m F2x.main
F2X_LIB = ../src/F2x/glue

# Output targets
FLIB = lib/libSOURCE.so
PYLIB = F2x_test/fortran/source_glue.py
CSLIB = cs/SOURCE.dll
CSTEST = cs/source_test.exe

# General dependencies
SOURCE = fortran/source.f90
SOURCE_CFG = fortran/source.f90-wrap

# Fortran shared library dependencies
FLIB_GEN = fortran/source_glue.f90
FLIB_SOURCE = $(SOURCE) $(FLIB_GEN)
FLIB_LIBS = $(F2X_LIB)/fortran/c_interface_module.f90 $(F2X_LIB)/fortran/f2x_glue.f90

# Python does not need anything else
PYLIB_GEN = fortran/source_glue.py

# C# library dependencies and tests
CSLIB_GEN = fortran/source_glue.cs
CSLIB_SOURCE = $(CSLIB_GEN)
CSLIB_LIBS = -r:$(F2X_LIB)/cs/F2x.Glue.dll
CSTEST_SOURCE = cs/source_test.cs
CSTEST_LIBS = -r:cs/SOURCE.dll $(CSLIB_LIBS)

# Primary target
all : $(FLIB) $(PYLIB) #$(CSLIB) $(CSTEST)

# Code generation
$(FLIB_GEN) $(PYLIB_GEN) $(CSLIB_GEN) : $(SOURCE) $(SOURCE_CFG)
	$(F2X) -t @_glue.f90.t -t @_glue.py.t -t @_glue.cs.t $(SOURCE)

# Fortran library build
$(FLIB) : $(FLIB_GEN) $(FLIB_SOURCE)
	$(FC) -shared -o $(FLIB) $(FLIB_LIBS) $(FLIB_SOURCE)

$(PYLIB) : $(FLIB) $(PYLIB_GEN)
	$(CP) $(PYLIB_GEN) $(PYLIB)
	$(CP) $(FLIB) F2x_test/fortran

# C# library build
$(CSLIB) : $(FLIB) $(CSLIB_GEN)
	$(CSC) -t:library -out:$(CSLIB) $(CSLIB_LIBS) $(CSLIB_SOURCE)

# C# test build
$(CSTEST) : $(CSTEST_SOURCE) $(FLIB)
	$(CSC) -out:$(CSTEST) $(CSTEST_LIBS) $(CSTEST_SOURCE)

clean :
	$(RM) $(PYLIB_GEN) $(CSLIB_GEN) $(FLIB_GEN) *.mod

