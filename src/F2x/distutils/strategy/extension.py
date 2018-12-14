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
from numpy.distutils.misc_util import fortran_ext_match, cxx_ext_match

from F2x import template
from F2x.distutils.strategy.base import BuildStrategy

c_ext_match = re.compile(r'.*[.](c)', re.IGNORECASE).match
ext_matches = cxx_ext_match, fortran_ext_match, c_ext_match


class ExtensionBuildStrategy(BuildStrategy):
    def finish_extension(self, extension):
        super(ExtensionBuildStrategy, self).finish_extension(extension)

        for template_name in self.templates:
            template_mod = template.get_template(template_name)
            if template_mod is None:
                continue

            for dep in template_mod.depends or []:
                if dep not in self.templates:
                    log.warn(f'{template_name} depends on {dep} but is not in template list')

            for lib_name, build_info in template_mod.libraries or []:
                if lib_name not in extension.libraries:
                    extension.libraries.append(lib_name)

            for module in template_mod.modules or []:
                module_path = os.path.join(os.path.dirname(template_mod.__file__), module)
                extension.sources.append(module_path)

    def finish_distribution(self, build_src, extension):
        super(ExtensionBuildStrategy, self).finish_distribution(build_src, extension)

        # Collect libraries required by templates
        if build_src.distribution.libraries is None:
            build_src.distribution.libraries = []

        for template_name in self.templates:
            template_mod = template.get(template_name)
            template_dir = os.path.relpath(os.path.dirname(template_mod.__file__))

            for lib_name, build_info in template_mod.libraries or []:
                dist_lib_name, dist_build_info = build_src.find_library(lib_name)

                if dist_build_info is None:
                    dist_build_info = dict(build_info)
                    dist_build_info['sources'] = []
                    build_src.distribution.libraries.append((lib_name, dist_build_info))

                for source in build_info['sources'][:]:
                    target_file = os.path.join(template_dir, source)
                    if target_file not in dist_build_info['sources']:
                        dist_build_info['sources'].append(target_file)

        # Split extensions wit multiple modules
        if len(extension.f2x_sources) > 1 and extension.f2x_autosplit_ext:
            self.split_extension(build_src, extension)

    def split_extension(self, build_src, extension):
        package = '.'.join(extension.name.split('.')[:-1])
        old_index = build_src.distribution.ext_modules.index(extension)
        new_extensions = []

        # Distinguish non-wrappable sources
        sources = list(extension.sources)
        for target_file, f2x_info in extension.f2x_sources:
            sources.remove(f2x_info['source'])

        # Create new extensions
        for target_file, f2x_info in extension.f2x_sources:
            new_name = None

            if 'config' in f2x_info:
                new_name = f2x_info['config'].get('generate', 'namespace', fallback=None)
            if new_name is None:
                new_name = f2x_info.get('library') or os.path.splitext(os.path.basename(target_file))[0]
            if package:
                new_name = f'{package}.{new_name}'

            new_ext = extension.__class__(new_name, sources + [f2x_info['source']],
                                          f2x_target=extension.f2x_target)

            new_extensions.append(new_ext)
            new_ext.f2x_target.finish_distribution(build_src, new_ext)

        build_src.distribution.ext_modules[old_index:old_index + 1] = new_extensions
        log.info(f'auto-split "{extension.name}" -> {[new_ext.name for new_ext in new_extensions]}')

    def build_sources(self, build_src, extension):
        if build_src.distribution.have_run.get('build_clib'):
            log.warn('build_clib has already been run, cannot generate libraries anymore')
            return

        super(ExtensionBuildStrategy, self).build_sources(build_src, extension)

        # Collect sources and wrapper input/output
        sources = list(extension.sources)
        for target_file, f2x_info in extension.f2x_sources:
            sources += [target_file] + f2x_info.get('output', [])

        package_name = '.'.join(build_src.get_ext_fullname(extension.name).split('.')[:-1])
        ext_sources, py_sources = build_src.filter_py_files(sources)

        if py_sources and package_name not in build_src.py_modules_dict:
            build_src.py_modules_dict[package_name] = []

        for source in py_sources:
            module_base = os.path.splitext(os.path.basename(source))[0]
            build_src.py_modules_dict[package_name].append((package_name, module_base, source))

        ext_sources = [target_file for target_file in ext_sources
                       if any(map(lambda ext_match: ext_match(target_file), ext_matches))]

        extension.sources[:] = ext_sources
