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
from distutils.dep_util import newer_group

from numpy.distutils import log

from F2x import template


class BuildStrategy(object):
    def __init__(self, templates=None):
        self.templates = templates or []
        self.template_files = []

        for template_name in self.templates:
            template_mod = template.get(template_name)

            if template_mod is None:
                log.warn(f'unknwon template "{template_name}"')
                continue

            for template_file in template_mod.templates or []:
                if template_file.startswith('@'):
                    template_file = os.path.join(os.path.dirname(template.__file__), template_file[1:])
                    package_dir = os.path.dirname(template_file)
                else:
                    package_dir = template_mod.package_dir
                    template_file = os.path.join(package_dir, template_file)

                self.template_files.append((template_file, package_dir))

    def get_template_files(self):
        template_dir = os.path.dirname(template.__file__)
        return [os.path.join(template_dir, filename[1:]) if filename.startswith('@') else filename
                for filename, _ in self.template_files]

    def finish_extension(self, extension):
        pass

    def finish_distribution(self, build_src, extension):
        extension.f2x_sources = build_src.scan_f_sources(extension)

    def finish_wrapper_input(self, build_src, extension, f2x_input):
        common_depends = self.get_template_files() + extension.depends or []

        f2x_input_updates = []
        for target_file, f2x_info, extension in f2x_input:
            depends = [target_file] + common_depends
            if build_src.force or any(map(lambda output: newer_group(depends, output, 'newer'), f2x_info['output'])):
                f2x_input_updates.append((target_file, f2x_info, extension))

        return f2x_input_updates

    def finish_libraries(self, extension, libraries):
        return libraries

    def build_sources(self, build_src, extension):
        build_src.populate_target(extension)
        build_src.generate_wrapper(extension)

    def get_ext_filename(self, build_src, extension):
        return None
