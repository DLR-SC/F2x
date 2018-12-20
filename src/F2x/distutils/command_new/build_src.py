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
import shlex
import sys

from distutils.dep_util import newer, newer_group
from distutils.util import get_platform
from numpy.distutils import log
from numpy.distutils.command import build_src as numpy_build_src

from F2x import argp
from F2x.distutils.strategy import get_strategy, show_strategies, base as strategy_base
from F2x.template import get_template, show_templates


class build_src(numpy_build_src.build_src):
    """
    Build sources for an F2x extension.

    This module creates source files for an extension. It proceeds by applying F2x with a given set of templates on the
    (appropriate) sources. For transformation of the sources, a given :class:`BuildStrategy` may be applied.
    """
    description = 'build sources from F2x'

    user_options = [
        ('build-src=',   'd',  'directory to "build" sources to'),
        ('strategy=',    None, 'appy the given strategy'),
        ('templates=',   None, 'list of F2x templates to use'),
        ('f2x-options=', None, 'list of F2x command line options'),
        ('force',        'f',  'forcibly build everything (ignore file timestamps)'),
        ('inplace',      'i',  'ignore build-lib and put compiled extensions into the source directory '
                               'alongside your pure Python modules'),
    ]

    boolean_options = ['force', 'inplace']

    help_options = [
        ('help-strategies', None, 'list available strategies', show_strategies),
        ('help-templates', None, 'list available templates', show_templates),
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
        self.strategy = None
        self.templates = None
        self.f2x_options = None

    def finalize_options(self):
        self.set_undefined_options('build', ('build_base', 'build_base'),
                                            ('build_lib', 'build_lib'),
                                            ('force', 'force'))

        if self.package is None:
            self.package = self.distribution.ext_package

        self.extensions = self.distribution.ext_modules
        self.libraries = self.distribution.libraries or []
        self.py_modules = self.distribution.py_modules or []
        self.py_modules_dict = {}
        self.data_files =  self.distribution.data_files or []

        if self.build_src is None:
            self.build_src = os.path.join(self.build_base, f"src.{get_platform()}-{sys.version[:3]}")

        if self.templates is not None:
            self.templates = [get_template(name) for name in shlex.split(self.templates)]

        if self.strategy is not None:
            self.strategy = get_strategy(self.strategy)

        if self.f2x_options is None:
            self.f2x_options = []
        else:
            self.f2x_options = shlex.split(self.f2x_options)

        if self.inplace is None:
            build_ext = self.get_finalized_command('build_ext')
            self.inplace = build_ext.inplace

    def build_sources(self):
        if self.inplace:
            self.get_package_dir = self.get_finalized_command('build_py').get_package_dir

        base_strategy = self.strategy or strategy_base.BuildStrategy()
        base_strategy.prepare_distribution(self, self.distribution)

        sources_to_wrap = dict()
        if self.extensions:
            self.check_extensions_list(self.extensions)

            # Update distribution with extension contents.
            for extension in self.extensions[:]:
                strategy = extension.strategy or base_strategy
                strategy.prepare_extension(self, extension)
                self.populate_build_src(extension)

            for extension in self.extensions:
                strategy = extension.strategy or base_strategy
                target_dir = self.get_target_dir(extension)

                # Wrap the sources.
                strategy.prepare_wrap_sources(self, extension, target_dir)
                self.select_sources(extension, strategy, target_dir, sources_to_wrap)

            self.wrap_sources(sources_to_wrap)

            for extension in self.extensions:
                target_dir = self.get_target_dir(extension)
                strategy.finish_wrap_sources(self, extension, target_dir)

        base_strategy.finish_distribution(self, self.distribution)

    def get_target_dir(self, extension):
        *package_path, _ = self.get_ext_fullname(extension.name).split('.')

        # Decide where to write output to.
        if self.inplace:
            return self.get_package_dir('.'.join(package_path))
        else:
            return os.path.join(self.build_src, *package_path)

    def populate_build_src(self, extension):
        for source_file, ext_info in extension.ext_modules:
            target_dir = self.get_target_dir(extension)
            target_file = os.path.join(target_dir, os.path.basename(source_file))

            if source_file != target_file:
                self.mkpath(target_dir)
                if not os.path.isfile(target_file) or newer(source_file, target_file):
                    self.copy_file(source_file, target_file)

    def prepare_package(self, package_name):
        package_path = package_name.split('.')
        package_dir = os.path.join(self.build_src, *package_path)
        if not os.path.isdir(package_dir):
            self.mkpath(os.path.join(self.build_src, *package_path))

        package_dir = self.build_src
        for package in package_path:
            package_dir = os.path.join(package_dir, package)
            package_init = os.path.join(package_dir, '__init__.py')
            if not os.path.isfile(package_init):
                with open(package_init, 'w') as init_file:
                    init_file.write("# Autogenerated by F2x\n")

    def select_sources(self, extension, strategy, target_dir, sources_to_wrap):
        templates = extension.templates or strategy.templates
        wrap_input = strategy.select_wrap_sources(self, extension, target_dir)

        if wrap_input:
            for _, template_file, _, template_dir in templates:
                if template_dir not in sources_to_wrap:
                    sources_to_wrap[template_dir] = { template_file: [] }
                elif template_file not in sources_to_wrap[template_dir]:
                    sources_to_wrap[template_dir][template_file] = []

                template_sources = sources_to_wrap[template_dir][template_file]

                for source_file, target_file, output, depends in wrap_input:
                    depends = [target_file] + depends
                    if target_file not in template_sources:
                        if self.force or any([newer_group(depends, output_file) for output_file in output]):
                            template_sources.append(target_file)

    def _filter_compile_sets(self, sources_to_wrap):
        input_file_sets = dict()  # Collect (input files) -> (templates)
        template_sets = dict()    # Collect (templates) -> (input files)

        for template_dir, templates in sources_to_wrap.items():
            for template_file, input_files in templates.items():
                input_files = set(input_files)
                input_files_key = tuple(sorted(input_files))

                # We need a stack of templates to circumvent templates with identical names...
                if input_files_key not in input_file_sets:
                    input_file_sets[input_files_key] = [(set(), set())]
                (template_dirs, template_files), *tail = input_file_sets[input_files_key]

                # Now find a template collection that won't have name collisions...
                if template_dir not in template_dirs:
                    while template_file in template_files:
                        if tail:
                            (template_dirs, template_files), *tail = tail
                        else:
                            template_dirs, template_files = set(), set()
                            input_file_sets[input_files_key].append((template_dirs, template_files))

                    # Add current template to input files entry
                    if template_dir is not None:
                        template_dirs.add(template_dir)
                    template_files.add(template_file)

                # Create new entroes for this template
                def update_template_set(update_template_dirs, update_template_files, update_input_files):
                    if update_input_files:
                        update_template_key = (tuple(sorted(update_template_dirs)), tuple(sorted(update_template_files)))
                        if update_template_key not in template_sets:
                            template_sets[update_template_key] = set(update_input_files)
                        else:
                            template_sets[update_template_key].intersection_update(update_input_files)

                # Base entry for just this template
                template_dirs, template_files = set(), {template_file}
                if template_dir is not None:
                    template_dirs.add(template_dir)
                update_template_set(template_dirs, template_files, input_files)

                # Update any possible combination
                for other_template, other_input_files in list(template_sets.items()):
                    other_template_dirs, other_template_files = map(set, other_template)
                    update_template_set(template_dirs.union(other_template_dirs),
                                        template_files.union(other_template_files),
                                        input_files.intersection(other_input_files))

        # Collection (input files) -> (templates) into a list (template_dirs, template_files, input_files)
        compile_sets = []
        for input_file_set, templates in input_file_sets.items():
            for template_dirs, template_files in templates:
                compile_sets.append((template_dirs, template_files, tuple(sorted(input_file_set))))
        input_file_sets = compile_sets

        # Find out optimal (templates) -> (input files) by look for longest templates list.
        compile_sets = []
        # Sort items by length of (template_dirs + template_files)
        template_sets = list(map(lambda p: (*map(set, p[0]), p[1]),
                                 sorted(template_sets.items(),
                                        key=lambda i: sum(map(len, i[0])),
                                        reverse=True)))

        # Process list of sets until everything is consumed
        while template_sets:
            # Keep head (i.e., longest list of templates) for output
            (template_dirs, template_files, input_file_set), *tail = template_sets
            compile_sets.append(tuple(map(lambda s: tuple(sorted(s)), (template_dirs, template_files, input_file_set))))
            template_sets = []

            # Check which parts of the tail to keep (i.e., input files list is not empty)
            for other_template_dirs, other_template_files, other_input_file_set in tail:

                # Make sure nothing is wrapped twice
                if not other_template_files.isdisjoint(template_files) \
                        or not other_template_dirs.isdisjoint(template_dirs):
                    other_input_file_set = other_input_file_set.difference(input_file_set)

                if other_input_file_set:
                    template_sets.append((other_template_dirs, other_template_files, other_input_file_set))
        template_sets = compile_sets

        return input_file_sets, template_sets

    def wrap_sources(self, sources_to_wrap):
        from F2x.wrapper import F2xWrapper

        compile_sets, *_ = sorted(self._filter_compile_sets(sources_to_wrap), key=len)

        for template_dirs, template_files, input_files in compile_sets:
            join = lambda head, *tail: (head) if not tail else (head + join(*tail))

            argv = self.f2x_options[:]
            argv = join(argv, *(['-T', template_dir] for template_dir in template_dirs))
            argv = join(argv, *(['-t', template_file] for template_file in template_files))

            log.info(f'wrapping sources {", ".join(input_files)}...')
            log.info(f'  F2x args: {" ".join(argv)}')

            args = argp.get_args_parser().parse_args(argv + list(input_files))
            wrapper = F2xWrapper(args, log)
            wrapper.run()
