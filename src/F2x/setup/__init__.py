from numpy.distutils.core import setup, numpy_cmdclass

from F2x.setup.extension import Extension, build_src


numpy_cmdclass['build_src'] = build_src
