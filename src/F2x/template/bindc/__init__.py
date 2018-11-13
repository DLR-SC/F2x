"""
bindc

Generate a ISO C compliant interface to a Fortran module using BIND(C).
"""
templates = ['@bindc/_glue.f90.t']
depends = None
modules = None
libraries = [
    ('bindc_f2x', {
        'sources': ['lib/c_interface_module.f90', ],
    }),
]
