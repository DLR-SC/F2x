# Build libSOURCE.so for use with tests.

FC = gfortran -fPIC
F2X = python -m F2x.main
LIB = libSOURCE.so
SOURCES = source.f90 source_glue.f90

libSOURCE.so: $(SOURCES)
	$(FC) -shared -o libSOURCE.so ../../../lib/c_interface_module.f90 $(SOURCES)

source_glue.f90: source.f90 source.f90-wrap
	$(F2X) -t @_glue.f90.t -t @_glue.py.t source.f90

clean:
	$(RM) *.mod source_glue.f90

