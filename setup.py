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
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

import setuptools
from distutils.core import setup

setup(
    name='F2x',
    version='0.1.dev0',

    description='Template-based Fortran wrapper.',
    author='Michael Meinel',
    author_email='michael.meinel@dlr.de',
    url='http://www.dlr.de/sc',
    
    packages=setuptools.find_packages('src'),
    package_dir={ '': 'src' },
    package_data={
        'F2x.parser.plyplus.grammar': ["*.g"],
        'F2x.template': ["*/*.t", "*/*.tl"],
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
        ],
    },
)
