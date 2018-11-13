from numpy.distutils.core import setup, numpy_cmdclass

from F2x.distutils.command import build_ext, build_src
from F2x.distutils.extension import Extension


numpy_cmdclass['build_src'] = build_src.build_src
numpy_cmdclass['build_ext'] = build_ext.build_ext
