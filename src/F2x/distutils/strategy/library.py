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
import configparser
import os

from distutils.sysconfig import get_config_vars as distuils_get_config_vars
from numpy.distutils import log

from F2x import template as F2x_template
from F2x.distutils.strategy.extension import ExtensionBuildStrategy


class ExtensionLibBuildStrategy(ExtensionBuildStrategy):
    def prepare_extension(self, build_src, extension):
        *package_path, ext_name = extension.name.split('.')

        if ext_name == '*':
            if extension.library_name:
                extension.name = '.'.join(package_path + [extension.library_name])

            else:
                extension.ext_modules = self._collect_ext_sources(build_src, extension)

                if len(extension.ext_modules) == 1:
                    filename, _ = extension.ext_modules[0]
                    extension.name = '.'.join(package_path + [os.path.splitext(os.path.basename(filename))[0]])

                elif not extension.autosplit:
                    log.warn(f'forcing autosplit for extension "{extension.name}"')
                    extension.autosplit = True

        super(ExtensionLibBuildStrategy, self).prepare_extension(build_src, extension)

    def prepare_wrap_sources(self, build_src, extension, target_dir):
        for source, ext_info in extension.ext_modules:
            if not 'config' in ext_info:
                ext_info['config'] = configparser.RawConfigParser()
                ext_info['wrapper'] = os.path.join(target_dir, os.path.basename(source) + '-wrap')

            config = ext_info['config']
            if not config.has_section('generate'):
                config.add_section('generate')

            config.set('generate', 'dll', self.get_ext_filename(build_src, extension.library_name or extension.name))

    def select_wrap_sources(self, build_src, extension, target_dir):
        wrap_sources = super(ExtensionLibBuildStrategy, self).select_wrap_sources(build_src, extension, target_dir)
        ext_infos = dict(extension.ext_modules)

        for source, *_ in wrap_sources:
            ext_info = ext_infos.get(source)

            if not ext_info:
                log.warn(f'no wrap info for "{source}"')
                continue

            config = ext_info.get('config')
            target_wrap = ext_info['wrapper']
            with open(target_wrap, 'w') as wrap_file:
                config.write(wrap_file)

        return wrap_sources

    def prepare_build_extension(self, build_ext, extension):
        super(ExtensionLibBuildStrategy, self).prepare_build_extension(build_ext, extension)

        if extension.library_name:
            library_name = '.'.join(extension.name.split('.')[:-1] + [extension.library_name])
            extension.name, extension.library_name = library_name, extension.name

    def finish_build_extension(self, build_ext, extension):
        super(ExtensionLibBuildStrategy, self).finish_build_extension(build_ext, extension)

        if extension.library_name:
            extension.name, extension.library_name = extension.library_name, extension.name.split('.')[-1]

    def get_ext_filename(self, build_src, ext_name):
        suffix = distuils_get_config_vars('SHLIB_SUFFIX')[0]
        *_, ext_name = build_src.get_ext_fullname(ext_name).split('.')
        return f'lib{ext_name}{suffix}'
