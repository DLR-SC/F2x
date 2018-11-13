"""
cerr

Generates a thin C layer that is used as clean stack snapshot for longjmp error handling.
"""
templates = ['@cerr/_cerr.c.t']
depends = ['bindc']
modules = None
libraries = [
    ('cerr_f2x', {
         'sources': ['lib/f2x_err.f90', 'lib/f2x_err_impl.c'],
     }),
]
