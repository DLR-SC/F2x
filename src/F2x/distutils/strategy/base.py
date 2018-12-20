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
import glob
import os

from distutils.errors import DistutilsFileError
from distutils.dep_util import newer, newer_group
from numpy.distutils import log
from numpy.distutils.misc_util import fortran_ext_match, f90_module_name_match

from F2x.template import get_template, package_dir as template_package_dir


class BuildStrategy(object):
    """
    Basic build strategy.

    A build strategy follows the build process and interacts with it at certain well-defined points.

    1. :code:`build_src` - Generate and prepare sources for compilation.

        1. :py:meth:`prepare_distribution`
        2. :py:meth:`prepare_extension` for each :py:class:`Extension <F2x.distutils.extension.Extension>`
        3. individually wrap each :py:class:`Extension <F2x.distutils.extension.Extension>`

            1. :py:meth:`prepare_wrap_sources`
            2. wrap sources from :py:meth:`select_wrap_sources`
            3. :py:meth:`finish_wrap_sources`

        4. :py:meth:`finish_distribution`

    2. :code:`build_ext` - Actually build binaries from (generated) sources.

        1. individually build each :py:class:`Extension <F2x.distutils.extension.Extension>`

            1. :py:meth:`prepare_build_extension`
            2. compile extension to :py:meth:`get_ext_filename`
            3. :py:meth:`finish_build_extension`

    Where each strategy intervenes and what it does is documented in the implementations.
    """

    def __init__(self, templates=None):
        self.templates = self.load_templates(templates or [])

    def prepare_distribution(self, build_src, distribution):
        """ Make sure `distribution.libraries` is at least an empty list. """
        if distribution.libraries is None:
            distribution.libraries = []

    def prepare_extension(self, build_src, extension):
        """ Prepare extension for code generation.

        * Collect information about extension sources into :py:attr:`ext_modules`.
        * Decide whether to split and split.
        * Collect libraries and modules form templates.
        """
        extension.ext_modules = self._collect_ext_sources(build_src, extension)

        if extension.autosplit and len(extension.ext_modules) > 1:
            self._split_extension(build_src, extension)
            return

        for template, *_ in (extension.templates or self.templates):
            for lib_name, lib_info in template.libraries or []:
                self._add_library(build_src, template.package_dir, lib_name, lib_info)

                if lib_name not in extension.libraries:
                    extension.libraries.append(lib_name)

            if template.modules:
                package_name = '.'.join(extension.name.split('.')[:-1])
                sources = [os.path.join(template.package_dir, module) for module in template.modules]
                self._add_python_modules(build_src, package_name, sources)

    def prepare_wrap_sources(self, build_src, extension, target_dir):
        """ Prepare sources for wrapping. Make sure everything is where it is expected. """
        build_src.mkpath(target_dir)

        # Ensure all target files are ready
        for source_file, ext_info in extension.ext_modules:
            target_file = os.path.join(target_dir, os.path.basename(source_file))
            if source_file != target_file:
                if build_src.force or not os.path.isfile(target_file) or newer(source_file, target_file):
                    build_src.copy_file(source_file, target_file)

    def select_wrap_sources(self, build_src, extension, target_dir):
        """
        Collect information about sources to be built.

        This method collects the following information about the module surces and passes them on:

        - name of original source file
        - target name of source file (from where code generation will take place)
        - names of new files to be expected
        - dependencies of those new files (i.e., templates, sources, interface config)
        """
        wrap_sources = []
        common_depends = self.get_template_files(with_imports=True) + list(extension.depends)

        for source, ext_info in extension.ext_modules:
            target_file = os.path.join(target_dir, os.path.basename(source))
            output = [os.path.join(target_dir, output_file) for output_file in ext_info['output']]
            depends = ext_info.get('depends', []) + common_depends + [target_file]

            for output_file in output:
                if build_src.force or newer_group(depends, output_file, missing='newer'):
                    wrap_sources.append((source, target_file, output, depends))
                    break

        return wrap_sources

    def finish_wrap_sources(self, build_src, extension, target_dir):
        """
        Clean up after code generation. Put newly generated files where they belong.

        Also creates a new library for the primary sources if requested by :py:attr:`inline_sources`.
        """
        # Add generated libraries where they belong
        sources, py_sources = build_src.filter_py_files(extension.sources)
        output, py_output = build_src.filter_py_files(self._get_wrap_output(extension, target_dir))
        *package_path, ext_name = build_src.get_ext_fullname(extension.name).split('.')
        package_name = '.'.join(package_path)

        if not extension.inline_sources:
            ext_lib_name = extension.library_name or f'{ext_name}_glue'
            ext_sources = [os.path.basename(source) for source in sources]
            ext_lib_info = {
                'sources': ext_sources,
                'libraries': extension.libraries,
            }
            self._add_library(build_src, target_dir, ext_lib_name, ext_lib_info)
            if ext_lib_name not in extension.libraries:
                extension.libraries.append(ext_lib_name)

            sources = output

        else:
            sources += output

        extension.sources[:] = sources
        py_sources += py_output

        if not build_src.inplace:
            build_py = build_src.get_finalized_command('build_py')
            source_dir = build_py.get_package_dir(package_name)

            # Be graceful if package directory does not yet exists.
            try:
                py_sources += [source for *_, source in build_py.find_package_modules(package_name, source_dir)]
            except DistutilsFileError:
                pass

        if py_sources:
            self._add_python_modules(build_src, package_name, py_sources)

    def finish_distribution(self, build_src, distribution):
        """ Update `build_clib` and `build_py` steps with newly collected libraries. """
        build_clib = build_src.get_finalized_command('build_clib')
        build_clib.include_dirs.append(build_clib.build_temp)
        build_clib.libraries = distribution.libraries

        build_py = build_src.get_finalized_command('build_py')
        build_py.package_dir.update(distribution.package_dir)
        build_py.packages += [package for package in distribution.packages if package not in build_py.packages]

    # build_ext

    def prepare_build_extension(self, build_ext, extension):
        """ Prepare build by updating include directories. """
        if build_ext.build_temp not in extension.module_dirs:
            extension.module_dirs.append(build_ext.build_temp)

    def get_ext_filename(self, build_src, extension):
        """ No customization here (i.e.,  return None). """
        return None

    def finish_build_extension(self, build_ext, extension):
        """ No customization here. """
        pass

    # general helpers

    def load_templates(self, template_names):
        """
        Load a list with template names into a list with templates and extra information:

        * loaded template
        * template file name (as passed to loader)
        * full path to loaded template
        * package directory of containing package
        """
        templates = []

        for template_name in template_names:
            if not isinstance(template_name, str):
                templates.append(template_name)
                continue

            template = get_template(template_name)

            if template is None:
                log.warn(f'unknwon template "{template_name}"')
                continue

            for template_file in template.templates or []:
                if template_file.startswith('@'):
                    package_dir = None
                    template_path = os.path.join(template_package_dir, template_file[1:])
                else:
                    package_dir = template.package_dir
                    template_path = os.path.join(package_dir, template_file)

                templates.append((template, template_file, template_path, package_dir))

        return templates

    def get_template_files(self, with_imports=False):
        """ Collect all template files required for this strategy.

        :param with_imports: If set to :code:`True` this will also try to extract imports for the
                             the templates recursively.
        """
        template_files = []
        for template, _, template_path, *_ in self.templates:
            template_files.append(template_path)
            if with_imports:
                template_files += template.depends

        return template_files

    # internal helpers

    def _collect_ext_sources(self, build_src, extension):
        templates = (extension.templates or self.templates)
        template_suffixes = [os.path.splitext(os.path.basename(file))[0] for _, file, *_ in templates]
        ext_sources = []

        # Expand wildcards
        sources = []
        for source_glob in extension.sources:
            sources += glob.glob(source_glob)

        extension.sources[:] = sources

        # Collect source information
        for source in extension.sources:
            if not fortran_ext_match(source):
                continue

            module_name = self._extract_module_name(source)
            base, _ = os.path.splitext(os.path.basename(source))
            ext_info = {
                'source': source,
                'module': module_name,
                'output': [base + suffix for suffix in template_suffixes],
            }

            source_wrap = source + '-wrap'
            if os.path.isfile(source_wrap):
                config = configparser.RawConfigParser()
                config.read(source_wrap)
                ext_info['config'] = config
                ext_info['wrapper'] = source_wrap

                if config.has_option('generate', 'library'):
                    ext_info['library'] = config.get('generate', 'library')

            ext_sources.append((source, ext_info))

        return ext_sources

    def _get_wrap_output(self, extension, target_dir):
        wrap_output = []

        for _, ext_info in extension.ext_modules:
            ext_output = [os.path.join(target_dir, output_file) for output_file in ext_info['output']]
            wrap_output += [output_file for output_file in ext_output if output_file not in wrap_output]

        return wrap_output

    def _split_extension(self, build_src, extension):
        index = build_src.extensions.index(extension)
        package_name = '.'.join(extension.name.split('.')[:-1])

        mod_sources = [source for source, _ in extension.ext_modules]
        common_sources = [source for source in extension.sources if source not in mod_sources]
        new_extensions = []

        for source in mod_sources:
            mod_name, _ = os.path.splitext(os.path.basename(source))
            new_extension = extension.clone(f'{package_name}.{mod_name}', [source] + common_sources)
            new_extensions.append(new_extension)
            self.prepare_extension(build_src, new_extension)
            build_src.populate_build_src(new_extension)

        log.info(f'split extension "{extension.name}" -> {[ext.name for ext in new_extensions]}')
        build_src.extensions[index:index + 1] = new_extensions
        extension.ext_modules = []

    def _extract_module_name(self, source):
        with open(source, 'r') as source_file:
            line = source_file.readline()
            while line is not None:
                match = f90_module_name_match(line)
                if match:
                    return match.group('name')

                line = source_file.readline()

        return None

    def _add_library(self, build_src, package_dir, lib_name, lib_info):
        sources = [os.path.relpath(os.path.join(package_dir, source)) for source in lib_info['sources']]

        for t_lib_name, t_lib_info in build_src.distribution.libraries:
            if t_lib_name == lib_name:
                t_lib_info['sources'] += [source for source in sources if source not in t_lib_info['sources']]

                return

        t_lib_info = dict(lib_info)
        t_lib_info['sources'] = sources
        build_src.distribution.libraries.append((lib_name, t_lib_info))

    def _add_python_modules(self, build_src, package_name, sources):
        package_path = package_name.split('.')

        if build_src.distribution.package_dir is None:
            build_src.distribution.package_dir = {}

        if build_src.inplace:
            target_dir = build_src.get_package_dir(package_name)

        else:
            target_dir = os.path.join(build_src.build_src, *package_path)
            if package_name not in build_src.distribution.package_dir:
                build_src.distribution.package_dir[package_name] = target_dir

            elif build_src.distribution.package_dir[package_name] != build_src.build_src:
                log.warn(f'misatching package directories for "{package_name} '
                         f'{build_src.build_src}, {build_src.distribution.package_dir[package_name]}')

        if package_name not in build_src.py_modules_dict:
            build_src.py_modules_dict[package_name] = []

        package_modules = build_src.py_modules_dict[package_name]
        for source in sources:
            module_file = os.path.basename(source)
            module_name, _ = os.path.splitext(module_file)
            target_file = os.path.join(target_dir, module_file)
            package_modules.append((package_name, module_name, target_file))

            if source != target_file:
                if not os.path.isfile(target_file) or newer(source, target_file):
                    build_src.mkpath(target_dir)
                    build_src.copy_file(source, target_file)

        # Ensure the whole thing is a package by now
        if build_src.inplace:
            package_dir = build_src.get_package_dir(package_name)

        else:
            package_dir = os.path.join(build_src.build_src, *package_path)

        package_init = os.path.join(package_dir, '__init__.py')
        if not os.path.isfile(package_init):
            with open(package_init, 'w') as init_file:
                init_file.write('# Automatically created by F2x.' + os.linesep)

        if build_src.distribution.packages is None:
            build_src.distribution.packages = []

        if package_name not in build_src.distribution.packages:
            build_src.distribution.packages.append(package_name)
