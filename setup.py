import os
import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

import setuptools
from distutils import core

from F2x.lib import setup

core.setup(
    name='F2x',
    version='0.1.dev0',

    description='Template-based Fortran wrapper.',
    author='Michael Meinel',
    author_email='michael.meinel@dlr.de',
    url='http://www.dlr.de/sc',
    
    packages=setuptools.find_packages('src'),
    package_dir={ '': 'src' },
    package_data={
        'F2x.grammar': ["*.g"],
        'F2x.template': ["*.t", "*.tl"],
        'F2x.lib': [
            "*/Makefile",
            "fortran/libF2x.so", "fortran/*.f90", "fortran/*.mod",
            "dotnet/F2x.Glue.dll", "dotnet/F2x.Glue/*.cs", "dotnet/F2x.Glue/Properties/AssemblyInfo.cs"]
    },

    setup_requires=[
        'pytest-runner',
    ],
    tests_require=[
        'pytest',
    ],
    install_requires=[
        'plyplus',
        'jinja2',
        'numpy',
    ],

    entry_points={
        'console_scripts': [
            'F2x=F2x.main:main',
            'F2x-lib=F2x.lib.main:main',
        ],
    },

    cmdclass={
        'build': setup.Build,
        'build_glue': setup.BuildGlue,
    }
)
