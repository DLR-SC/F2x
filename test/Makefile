# Makefile for test modules.
#

# Tool configuration
CP = cp
FC = gfortran -fPIC -g
CSC = mcs
F2X = python -m F2x.main
F2X_INC = $(shell python -m F2x.lib.main -Fc)

# Output targets
FLIB = lib/libSOURCE.so
PYLIB = F2x_test/fortran/source_wrap.py
CSLIB = dotnet/SOURCE.dll
CSTEST = dotnet/source_test.exe

# General dependencies
SOURCE = fortran/source.f90
SOURCE_CFG = fortran/source.f90-wrap

# Fortran shared library dependencies
FLIB_GEN = fortran/source_wrap.f90
FLIB_SOURCE = $(SOURCE) $(FLIB_GEN)
FLIB_LIBS = $(shell python -m F2x.lib.main -FIx)

# Python does not need anything else
PYLIB_GEN = fortran/source_wrap.py

# C# library dependencies and tests
CSLIB_GEN = fortran/source_glue.cs
CSLIB_SOURCE = $(CSLIB_GEN)
CSLIB_LIBS = $(shell python -m F2x.lib.main -N -I)
CSTEST_SOURCE = dotnet/source_test.cs
CSTEST_LIBS = -r:dotnet/SOURCE.dll $(CSLIB_LIBS)

# Primary target
all : $(FLIB) $(PYLIB) #$(CSLIB) $(CSTEST)

# Code generation
$(FLIB_GEN) $(PYLIB_GEN) $(CSLIB_GEN) : $(SOURCE) $(SOURCE_CFG)
	$(F2X) -t @bindc/_wrap.f90.t -t @ctypes/_wrap.py.t -t @_glue.cs.t $(SOURCE)

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

