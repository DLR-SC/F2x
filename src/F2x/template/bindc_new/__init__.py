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
"""
Generate a ISO C compliant interface to a Fortran module using BIND(C).
"""
name = 'bindc_new'
templates = ['@bindc_new/_glue.f90.t']
requires = None
modules = None
libraries = [
    ('f2x_bindc', {
        'sources': ['lib/f2x_bindc.f90', ],
    }),
]
