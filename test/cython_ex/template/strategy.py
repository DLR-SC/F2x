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
import re

from numpy.distutils import log
from Cython.Build import cythonize

from F2x.distutils.strategy.extension import ExtensionBuildStrategy, ext_matches


cython_ext_match = re.compile(r'.*[.](pyx)\Z', re.IGNORECASE).match


class CythonExtBuildStrategy(ExtensionBuildStrategy):
    def __init__(self, templates=None):
        super(CythonExtBuildStrategy, self).__init__(templates)

        self.has_pyx_template = any(map(cython_ext_match, self.get_template_files()))

    def build_sources(self, build_src, extension):
        if build_src.distribution.have_run.get('build_clib'):
            log.warn('build_clib has already been run, cannot generate libraries anymore')
            return

        # Using super class as base for super to allow skipping of ExtensionBuildStrategy implementation
        super(ExtensionBuildStrategy, self).build_sources(build_src, extension)
        sources = list(extension.sources)

        # Split should be done before build, for Cython only wrap first Fortran module
        if self.has_pyx_template:
            (target_file, f2x_info), *f2x_sources = extension.f2x_sources
            sources += [target_file] + f2x_info.get('output', []) + [target_file for target_file, _ in f2x_sources]

        else:
            for target_file, f2x_info in extension.f2x_sources:
                sources += [target_file] + f2x_info.get('output', [])

        # Collect all compile sources
        ext_sources, pyx_sources = [], []
        for target_file in sources[:]:
            if any(map(lambda m: m(target_file), ext_matches)):
                ext_sources.append(target_file)

            elif cython_ext_match(target_file):
                pyx_sources.append(target_file)

        extension.sources[:] = ext_sources + pyx_sources

        if pyx_sources:
            if len(pyx_sources) > 1:
                log.warn(f'extension "{extension.name}" has multiple Cython sources {cy_sources}')

            pyx_extensions = cythonize([extension])
            for cy_extension in pyx_extensions:
                cy_extension.module_dirs = (cy_extension.module_dirs or [])
                cy_extension.module_dirs += extension.module_dirs

            old_index = build_src.distribution.ext_modules.index(extension)
            build_src.distribution.ext_modules[old_index:old_index + 1] = pyx_extensions
