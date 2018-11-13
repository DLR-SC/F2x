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
from F2x.template import register_template
from F2x.distutils import setup, Extension, strategy

from cython_ex import template as cython_template
from cython_ex.template.strategy import CythonExtBuildStrategy

register_template(cython_template)
strategy.register_strategy('cython', CythonExtBuildStrategy(['bindc', 'cython']))


setup(
    name="F2x tests",

    packages=['basic', 'boolarray', 'cython_ex'],

    ext_modules=[
        Extension('basic.common.ext', ['basic/source.f90', 'basic/boolarray.f90'],
                  f2x_target='lib',
                  extra_f90_compile_args=['-ffree-line-length-none']),

        Extension('basic.err.ext', ['basic/source.f90', 'basic/boolarray.f90', 'basic/errors.f90'],
                  f2x_target='lib_err',
                  define_macros=[('INCLUDE_ERR_TESTS', None)],
                  extra_f90_compile_args=['-ffree-line-length-none']),

        Extension('cython_ex.ext', ['cython_ex/simple.f90', 'cython_ex/second.f90'],
                  f2x_autosplit_ext=True,
                  f2x_target='cython'),
    ],
)
