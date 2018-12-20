# -*- encoding: utf-8 -*-
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
from F2x.distutils import setup, Extension, strategy
from F2x.template import register_template

from F2x_test.template import cython as cython_template
from F2x_test.template.cython.strategy import CythonExtBuildStrategy

register_template(cython_template)
strategy.register_strategy('cython', CythonExtBuildStrategy(['cython']))


setup(
    name="F2x tests",

    packages=['F2x_test', 'F2x_test.interface', 'cython_ex'],

    ext_modules=[
        Extension('F2x_test.interface.lib.*', ['F2x_test/interface/src/*.f90'],
                  library_name='flib_bindc',
                  strategy='lib',
                  inline_sources=False),

        Extension('F2x_test.interface.bindc_new.*', ['F2x_test/interface/src/*.f90', 'cython_ex/*.f90'],
                  library_name='flib_bindc_new',
                  strategy='lib',
                  inline_sources=False,
                  templates=['bindc_new', 'ctypes_new']),

        Extension('cython_ex.*', ['cython_ex/*.f90'],
                  strategy='cython',
                  inline_sources=True),

#        Extension('F2x_test.doc.interface.*', ['F2x_test/interface/src/*.f90'],
#                  autosplit=True,
#                  strategy='sphinx_docs'),
    ],
)
