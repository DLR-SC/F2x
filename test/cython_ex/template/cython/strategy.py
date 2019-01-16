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
import os
import re

from numpy.distutils import log
from Cython.Build import cythonize

from F2x.distutils.strategy.extension import ExtensionBuildStrategy, ext_matches


cython_ext_match = re.compile(r'.*[.](pyx)\Z', re.IGNORECASE).match


class CythonExtBuildStrategy(ExtensionBuildStrategy):
    def __init__(self, templates=None):
        super(CythonExtBuildStrategy, self).__init__(templates)

        self.has_pyx_template = any(map(cython_ext_match,
                                        [os.path.splitext(filename)[0] for filename in self.get_template_files()]))

    def finish_wrap_sources(self, build_src, extension, target_dir):
        if build_src.distribution.have_run.get('build_clib'):
            log.warn('build_clib has already been run, cannot generate libraries anymore')
            return

        # Using super class as base for super to allow skipping of ExtensionBuildStrategy implementation
        super(CythonExtBuildStrategy, self).finish_wrap_sources(build_src, extension, target_dir)
        sources = list(extension.sources)

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
                log.warn(f'extension "{extension.name}" has multiple Cython sources {pyx_sources}')

            _, pyx_basename = os.path.split(pyx_sources[0])
            pyx_module_name, _ = os.path.splitext(pyx_basename)
            extension.name = '.'.join(list(extension.name.split('.')[:-1]) + [pyx_module_name])

            pyx_extensions = cythonize([extension])
            for pyx_extension in pyx_extensions:
                extension.copy_to(pyx_extension)

            old_index = build_src.distribution.ext_modules.index(extension)
            build_src.distribution.ext_modules[old_index:old_index + 1] = pyx_extensions
