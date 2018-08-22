# Makefile for test modules.
#

# Tool configuration
CP = cp
FC = gfortran -fPIC -g -ffree-line-length-none
#FC = ifort -fPIC -g
CC = gcc -fPIC -g
F2X = python -m F2x.main -P
F2X_INC = $(shell python -m F2x.lib.main -Fc)

# Output targets
FLIB = lib/libSOURCE.so
PYLIB = F2x_test/fortran/source_glue.py

# General dependencies
SOURCE = fortran/source.f90
SOURCE_CFG = fortran/source.f90-wrap

# Fortran shared library dependencies
FLIB_GEN = fortran/source_glue.f90
FLIB_SOURCE = $(SOURCE) $(FLIB_GEN)
FLIB_LIBS = $(shell python -m F2x.lib.main -FI) ../src/F2x/lib/fortran/c_interface_module.o

# Python does not need anything else
PYLIB_GEN = fortran/source_glue.py


# Primary target
all : $(FLIB) $(PYLIB)

# Code generation
$(FLIB_GEN) $(PYLIB_GEN) $(CSLIB_GEN) : $(SOURCE) $(SOURCE_CFG)
	$(F2X) -t @bindc/_glue.f90.t -t @ctypes/_glue.py.t  $(SOURCE)

# Fortran library build
$(FLIB) : $(FLIB_GEN) $(FLIB_SOURCE)
	$(FC) -shared -o $(FLIB) $(FLIB_LIBS) $(FLIB_SOURCE)

$(PYLIB) : $(FLIB) $(PYLIB_GEN)
	$(CP) $(PYLIB_GEN) $(PYLIB)
	$(CP) $(FLIB) F2x_test/fortran

	
clean :
	$(RM) $(PYLIB_GEN) $(CSLIB_GEN) $(FLIB_GEN) *.mod

