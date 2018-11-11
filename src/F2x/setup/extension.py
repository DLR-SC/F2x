import configparser
import os
import re
import shlex
import sys

from distutils import errors as distutils_errors
from distutils.dep_util import newer_group, newer
from distutils.util import get_platform

from numpy.distutils import log
from numpy.distutils.command.build_src import build_src as numpy_build_src
from numpy.distutils.extension import Extension as numpy_Extension
from numpy.distutils.misc_util import fortran_ext_match, cxx_ext_match, f90_module_name_match

from F2x.main import main as F2x_main
from F2x.lib import get_internal_lib as F2x_get_internal_lib


c_ext_match = re.compile(r'.*[.](c)\Z', re.I).match


class Extension(numpy_Extension):
    _FORTRAN_EXT_RE = re.compile(r'.*\.(f77|f90|f95|for|ftn|f)\Z', re.IGNORECASE)
    _FORTRAN_MOD_RE = re.compile(r'^\s*(\d+\s+)?module\s+([a-z][a-z0-9_]*)', re.IGNORECASE)

    def __init__(self, name, sources,
                 library_name=None,
                 f2x_options=None,
                 f2x_target='lib',
                 **kwargs):
        super(Extension, self).__init__(name, sources, **kwargs)

        self.library_name = library_name
        self.f2x_options = f2x_options or []
        self.f2x_target = f2x_target

    def find_modules(self):
        module_names = []

        for source in self.sources:
            if fortran_ext_match(os.path.splitext(source)[1]):
                module_name = self._extract_module_name(source)
                module_names.append((source, module_name))

        return module_names

    def _extract_module_name(self, source):
        with open(source, 'r') as source_file:
            line = source_file.readline()
            while line:
                match = f90_module_name_match(line)
                if match:
                    return match.group('name')
                line = source_file.readline()

        return None


def show_templates():
    pass


class build_src(numpy_build_src):
    description = 'build sources from F2x'

    user_options = [
        ('build-src=', 'd', 'directory to "build" sources to'),
        ('f2x-templates=', None, 'list of F2x templates to use'),
        ('f2x-opts=', None, 'list of F2x command line options'),
        ('force', 'f', 'forcibly build everything (ignore file timestamps)'),
        ('inpace', 'i', 'ignore build-lib and put compiled extensions into the source directory '
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
        self.inpace = None
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

        if self.f2x_templates is None:
            self.f2x_templates = ['@bindc/_glue.f90.t', '@ctypes/_glue.py.t'] # TODO let F2x define default templates
        else:
            self.f2x_templates = shlex.split(self.f2x_templates)

        if self.f2x_opts is None:
            self.f2x_opts = []
        else:
            self.f2x_opts = shlex.split(self.f2x_opts)

        build_ext = self.get_finalized_command('build_ext')
        if self.inpace is None:
            self.inpace = build_ext.inplace

    def build_sources(self):
        if self.inpace:
            self.get_package_dir = self.get_finalized_command('build_py').get_package_dir

        if self.extensions:
            self.check_extensions_list(self.extensions)

            for ext in self.extensions:
                self.build_extension_sources(ext)

    def build_extension_sources(self, extension):
        log.info(f'building extension "{extension.name}" sources')

        f2x_sources = self.scan_f_sources(extension)
        self.populate_target(extension, f2x_sources)
        self.generate_wrapper(extension, f2x_sources)

        if extension.f2x_target == 'lib':
            self.build_extension_sources_lib(extension, f2x_sources)

        elif extension.f2x_target == 'module':
            self.build_extension_sources_module(extension, f2x_sources)

        else:
            raise distutils_errors.DistutilsError(f'Unknown target: {extension.f2x_target}')

    def scan_f_sources(self, extension):
        template_suffixes = tuple(map(lambda path: os.path.splitext(os.path.basename(path))[0], self.f2x_templates))
        f2x_sources = []

        for source, module in extension.find_modules():
            target_file = os.path.join(self.build_src, source)
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
                f2x_info['wrapper'] = source_wrap
                f2x_info['config'] = config
                if config.has_option('generate', 'dll'):
                    f2x_info['library'] = config.get('generate', 'dll')

            f2x_sources.append((target_file, f2x_info))

        return f2x_sources

    def populate_target(self, extension, f2x_sources):
        fullname = self.get_ext_fullname(extension.name)
        fullname_parts = fullname.split('.')
        package_dir = os.path.join(self.build_src, *fullname_parts[:-1])
        self.mkpath(package_dir)

        sources = list(extension.sources)
        new_sources = []
        for target_file, f2x_info in f2x_sources:
            source = f2x_info['source']
            sources.remove(source)
            new_sources.append(target_file)

            if not os.path.isfile(target_file) or newer(source, target_file):
                self.copy_file(source, target_file)

            wrapper = f2x_info.get('wrapper')
            if wrapper is not None:
                target_wrapper = os.path.join(os.path.dirname(target_file), os.path.basename(wrapper))
                new_sources.append(target_wrapper)
                if not os.path.isfile(target_wrapper) or newer(wrapper, target_wrapper):
                    self.copy_file(wrapper, target_wrapper)

        for source in sources:
            target_file = os.path.join(self.build_src, source)

            if not os.path.isfile(target_file) or newer(source, target_file):
                self.copy_file(source, target_file)
                new_sources.append(target_file)

        extension.sources[:] = new_sources

    def generate_wrapper(self, extension, f2x_sources):
        f2x_input = []

        for target_file, f2x_info in f2x_sources:
            depends = [target_file] + extension.depends
            if 'wrapper' in f2x_info:
                depends.append(f2x_info['wrapper'])

            if self.force or any(map(lambda target: newer_group(depends, target, 'newer'), f2x_info['output'])):
                f2x_input.append(target_file)

        if f2x_input:
            log.info('generating wrappers for %s (F2x)', ', '.join(f2x_input))

            argv = self.f2x_opts + extension.f2x_options
            for template in self.f2x_templates:
                argv += ['-t', template]
            argv += f2x_input

            F2x_main(argv)

    def build_extension_sources_lib(self, extension, f2x_sources):
        if self.distribution.have_run.get('build_clib'):
            log.warn('build_clib has already been run, cannot generate libraries anymore')
            return

        lib_names = set()
        for target_file, f2x_info in f2x_sources:
            if 'library' in f2x_info:
                lib_names.add(f2x_info['library'])

        if len(lib_names) < 1:
            library_name = extension.library_name or ('lib' + f2x_sources[0][1]['module'] + '.so')

        elif len(lib_names) > 1:
            raise distutils_errors.DistutilsError(f'Too many library names found in "{extension.name}". {lib_names}')

        else:
            library_name = list(lib_names)[0]

        if extension.library_name is not None and extension.library_name != library_name:
            raise distutils_errors.DistutilsError(f'Library names do not match in "{extension.name}". '
                                                  '{library_name}, {extension.library_name}')

        for lib_name, build_info in self.distribution.libraries or []:
            if lib_name == library_name:
                break

        else:
            sources = list(extension.sources)
            f2x_target_sources = []

            for target_file, f2x_info in f2x_sources:
                f2x_target_sources.append(target_file)

                for f2x_output in f2x_info.get('output', []):
                    ext = os.path.splitext(f2x_output)[1]

                    if fortran_ext_match(ext) or cxx_ext_match(ext) or c_ext_match(ext):
                        f2x_target_sources.append(f2x_output)
                        sources.remove(target_file)

            build_info = {
                'sources': F2x_get_internal_lib() + f2x_target_sources,
                'libraries': extension.libraries,
                'depends': extension.depends,
                'language': extension.language,
                'extra_compiler_args': extension.extra_compile_args,

            }

            if self.distribution.libraries is None:
                self.distribution.libraries = []
            self.distribution.libraries.append((library_name, build_info))

        if self.distribution.libraries:
            build_clib = self.get_finalized_command('build_clib')
            build_clib.libraries = self.distribution.libraries

    def build_extension_sources_module(self, ext, f2x_sources):
        pass


if __name__ == '__main__':
    import sys

    ext = Extension('test', sys.argv[1:])
    print(ext.name, ext.module_names)
