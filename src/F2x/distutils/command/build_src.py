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
import shlex
import sys

from distutils import errors as distutils_errors
from distutils.dep_util import newer, newer_group
from distutils.util import get_platform

from numpy.distutils import log
from numpy.distutils.command.build_src import build_src as numpy_build_src

from F2x.template import show_templates
from F2x.main import main as F2x_main


class build_src(numpy_build_src):
    description = 'build sources from F2x'

    user_options = [
        ('build-src=',     'd',  'directory to "build" sources to'),
        ('f2x-templates=', None, 'list of F2x templates to use'),
        ('f2x-opts=',      None, 'list of F2x command line options'),
        ('force',          'f',  'forcibly build everything (ignore file timestamps)'),
        ('inplace',        'i',  'ignore build-lib and put compiled extensions into the source directory '
                                 'alongside your pure Python modules'),
    ]

    boolean_options = ['force', 'inplace']

    help_options = [
        ('help-templates', None, 'list available built-in templates', show_templates),
    ]

    def initialize_options(self):
        self.extensions = None
        self.package = None
        self.py_modules = None
        self.py_modules_dict = None
        self.build_src = None
        self.build_lib = None
        self.build_base = None
        self.force = None
        self.inplace = None
        self.package_dir = None
        self.f2x_templates = None
        self.f2x_opts = None

    def finalize_options(self):
        self.set_undefined_options('build',
                                   ('build_base', 'build_base'), ('build_lib', 'build_lib'), ('force', 'force'))

        if self.package is None:
            self.package = self.distribution.ext_package

        self.extensions = self.distribution.ext_modules
        self.libraries = self.distribution.libraries or []
        self.py_modules = self.distribution.py_modules or []
        self.data_files =  self.distribution.data_files or []

        if self.build_src is None:
            self.build_src = os.path.join(self.build_base, f"src.{get_platform()}-{sys.version[:3]}")

        self.py_modules_dict = {}

        if self.f2x_templates is not None:
            self.f2x_templates = shlex.split(self.f2x_templates)

        if self.f2x_opts is None:
            self.f2x_opts = []
        else:
            self.f2x_opts = shlex.split(self.f2x_opts)

        build_ext = self.get_finalized_command('build_ext')
        if self.inplace is None:
            self.inplace = build_ext.inplace

    def build_sources(self):
        if self.inplace:
            self.get_package_dir = self.get_finalized_command('build_py').get_package_dir

        if self.extensions:
            self.check_extensions_list(self.extensions)

            for extension in self.extensions[:]:
                extension.f2x_target.finish_distribution(self, extension)

            for extension in self.extensions[:]:
                self.build_extension_sources(extension)

    def build_extension_sources(self, extension):
        log.info(f'building extension "{extension.name}" sources')

        extension.f2x_target.build_sources(self, extension)

        build_clib = self.get_finalized_command('build_clib')
        build_clib.libraries = self.distribution.libraries

    def scan_f_sources(self, extension):
        templates = self.f2x_templates or extension.f2x_target.template_files
        template_suffixes = [os.path.splitext(os.path.basename(path))[0] for path in templates]

        target_dir = os.path.join(self.build_src, *self.get_ext_fullname(extension.name).split('.')[:-1])
        f2x_sources = []

        for source, module in extension.find_modules():
            target_file = os.path.join(target_dir, os.path.basename(source))
            base, ext = os.path.splitext(target_file)

            f2x_info = {
                'source': source,
                'module': module,
                'output': [base + suffix for suffix in template_suffixes],
            }

            source_wrap = source + '-wrap'
            if os.path.isfile(source_wrap):
                config = configparser.RawConfigParser()
                config.read(source_wrap)
                f2x_info['wrapper'] = os.path.join(target_dir, os.path.basename(source_wrap))
                f2x_info['config'] = config

                if config.has_option('generate', 'library'):
                    f2x_info['library'] = config.get('generate', 'library')

            f2x_sources.append((target_file, f2x_info))

        return f2x_sources

    def populate_target(self, extension):
        fullname = self.get_ext_fullname(extension.name)
        fullname_parts = fullname.split('.')
        package_name = '.'.join(fullname_parts[:-1])
        package_dir = os.path.join(self.build_src, *fullname_parts[:-1])
        sources = list(extension.sources)
        new_sources = []

        if package_name in self.distribution.packages:
            build_py = self.get_finalized_command('build_py')
            modules = build_py.find_package_modules(package_name, build_py.get_package_dir(package_name))
            sources += [source for _, _, source in modules if source not in sources]

        self.mkpath(package_dir)
        package_init = os.path.join(package_dir, '__init__.py')
        if not os.path.isfile(package_init):
            with open(package_init, 'w'):
                pass
        new_sources.append(package_init)

        for target_file, f2x_info in extension.f2x_sources:
            source = f2x_info['source']
            sources.remove(source)

            if not os.path.isfile(target_file) or newer(source, target_file):
                self.copy_file(source, target_file)

            wrapper = f2x_info.get('wrapper')
            if wrapper is not None:
                source_wrapper = f2x_info['source'] + '-wrap'

                if not os.path.isfile(wrapper) or newer(source_wrapper, wrapper):
                    self.copy_file(source_wrapper, wrapper)

        for source in sources:
            target_file = os.path.join(package_dir, os.path.basename(source))
            new_sources.append(target_file)

            if not os.path.isfile(target_file) or newer(source, target_file):
                self.copy_file(source, target_file)

        if package_name not in self.py_modules_dict:
            self.py_modules_dict[package_name] = []

        # TODO check if this is required
        if package_name not in self.distribution.packages:
            self.distribution.packages.append(package_name)

        if self.distribution.package_dir is None:
            self.distribution.package_dir = dict()

        if package_name not in self.distribution.package_dir:
            self.distribution.package_dir[package_name] = self.build_src

        extension.sources[:] = new_sources

    def generate_wrapper(self, extension):
        f2x_input = []
        build_ext = self.get_finalized_command('build_ext')

        extension.module_dirs = extension.module_dirs or []
        if build_ext.build_temp not in extension.module_dirs:
            extension.module_dirs.append(build_ext.build_temp)

        for target_file, f2x_info in extension.f2x_sources:
            f2x_input.append((target_file, f2x_info, extension))

        f2x_input = extension.f2x_target.finish_wrapper_input(self, extension, f2x_input)

        if f2x_input:
            log.info('generating wrappers for %s', ', '.join([i[0] for i in f2x_input]))

            argv = self.f2x_opts + extension.f2x_options
            for template in extension.f2x_target.template_files:
                argv += ['-t', template]

            for target_file, f2x_info, extension in f2x_input:
                argv.append(target_file)

            F2x_main(argv)

    def find_library(self, name):
        for lib_name, build_info in self.distribution.libraries or []:
            if lib_name == name:
                return lib_name, build_info

        return name, None

    def find_library_name(self, extension):
        lib_names = set()

        for target_file, f2x_info in extension.f2x_sources:
            if 'library' in f2x_info:
                lib_names.add(f2x_info['library'])

        if len(lib_names) < 1:
            library_name = extension.library_name or extension.name.split('.')[-1]

        elif len(lib_names) > 1:
            raise distutils_errors.DistutilsError(f'Too many library names found in "{extension.name}". {lib_names}')

        else:
            library_name = list(lib_names)[0]

        if extension.library_name is not None and extension.library_name != library_name:
            raise distutils_errors.DistutilsError(f'Library names do not match in "{extension.name}". '
                                                  f'{library_name}, {extension.library_name}')

        return library_name

    def build_extension_sources_module(self, extension):
        pass

    def find_extension(self, ext_name):
        for extension in self.distribution.ext_modules:
            ext_full_name = self.get_ext_fullname(extension.name)
            if ext_name == ext_full_name:
                return extension

        return None