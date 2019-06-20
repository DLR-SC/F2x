# F2x installation script (setup.py)
#
# Copyright 2018 German Aerospace Center (DLR)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import os
import sys

import setuptools
from distutils.core import setup


package_data = {
    'F2x.parser.plyplus.grammar': ["*.g"],
}

cmdclass = {}
command_options = {}


sys.path.append(os.path.abspath('src'))
try:
    # Try to extract program information from sources.
    import F2x
    from F2x.template import collect_template_data

    name = F2x.program_name
    version = F2x.get_version_string()
    release = F2x.get_version_string(full=True)

    for package, data_files in collect_template_data():
        package_data[package.__name__] = package_data.get(package.__name__, []) + data_files

except ImportError:
    # Fallback: set them manual :(
    name = 'F2x'
    version = '0.0.0'
    release = '0.0'


try:
    from F2x.distutils.command import build_sphinx

    cmdclass['build_sphinx'] = build_sphinx.build_sphinx
    command_options['build_sphinx'] = {
        'project': ('setup.py', name),
        'version': ('setup.py', version),
        'release': ('setup.py', release),
        'source_dir': ('setup.py', 'doc/src'),
        'build_dir': ('setup.py', 'doc'),
    }

except ImportError:
    pass


setup(
    name=name,
    version=version,

    description='Template-based Fortran wrapper.',
    author='Michael Meinel',
    author_email='michael.meinel@dlr.de',
    url='http://www.dlr.de/sc',

    cmdclass=cmdclass,
    command_options=command_options,

    packages=setuptools.find_packages('src'),
    package_dir={ '': 'src' },
    package_data=package_data,

    install_requires=[
        'plyplus',
        'jinja2',
        'numpy',
    ],
    extras_require={
        'cython': ['Cython', ],

        'docs': [
            'six',
            'sphinx',
            'sphinx-argparse',
        ],

        'tests': [
            'pytest-runner',
            'pytest',
        ],
    },

    entry_points={
        'console_scripts': [
            'F2x=F2x.runtime.main:main',
            'F2x-d=F2x.runtime.daemon:main',
        ],
    },
)
