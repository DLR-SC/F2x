# F2x installation script (setup.py)
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
import sys

import setuptools
from distutils.core import setup

sys.path.append(os.path.abspath('src'))
try:
    # Try to extract program information from sources.
    import F2x
    name = F2x.program_name
    major, minor, patch = F2x.__version__
    version = F2x.get_version_string()
    release = F2x.get_version_string(full=True)

except ImportError:
    # Fallback: set them manual :(
    name = 'F2x'
    version = '0.0.0'
    release = '0.0'


cmdclass = {}
command_options = {}

try:
    # Register Sphinx build process if possible

    from sphinx.ext import apidoc
    from sphinx.setup_command import BuildDoc

    class build_sphinx(BuildDoc):
        """
        Build documentation based on Sphinx.

        This implementation adds automatic generation of the API documentation (:code:`sphinx-apidoc ...`) during the build.
        """

        def run(self):
            source_dirs = list(self.distribution.package_dir.values()) \
                          or [os.path.dirname(__file__)]

            apidoc.main(['-f', '-T', '-e', '-M', '-o', os.path.join(self.source_dir, 'api')] + source_dirs)
            #self.document_template_libs()

            super(build_sphinx, self).run()

        def document_template_libs(self):
            from numpy.distutils.misc_util import fortran_ext_match

            from F2x.main import main
            from F2x.template import _templates

            # Find libraries in templates.
            sources = []
            for template in _templates.values():
                for lib_name, lib_info in template.libraries or []:
                    add_sources = [os.path.join(template.package_dir, source)
                                    for source in lib_info.get('sources', [])
                                    if fortran_ext_match(source)]
                    for source in add_sources:
                        with open(source + '-wrap', 'w') as config:
                            config.write(f'[generate]\nlibrary={lib_name}\n')
                    sources += add_sources

            # Generate documentation.
            argv = ['-t', '@sphinx/.rst.t'] + sources
            print('Generating docs...', argv)
            main(argv)

    cmdclass['build_sphinx'] = build_sphinx
    command_options['build_sphinx'] = {
        'project': ('setup.py', name),
        'version': ('setup.py', version),
        'release': ('setup.py', release),
        'source_dir': ('setup.py', 'doc/src'),
        'build_dir': ('setup.py', 'doc'),
    }

except ImportError:
    pass


setup(
    name=name,
    version=version,

    description='Template-based Fortran wrapper.',
    author='Michael Meinel',
    author_email='michael.meinel@dlr.de',
    url='http://www.dlr.de/sc',

    cmdclass=cmdclass,
    command_options=command_options,

    packages=setuptools.find_packages('src'),
    package_dir={ '': 'src' },
    package_data={
        'F2x.parser.plyplus.grammar': ["*.g"],
        'F2x.template': ["*/*.t", "*/*.tl"],
    },

    setup_requires=[
        'pytest-runner',
        'sphinx',
        'sphinx-fortran',
        'sphinxext-argparse',
    ],
    tests_require=[
        'pytest',
    ],
    install_requires=[
        'plyplus',
        'jinja2',
        'numpy',
    ],
    extras_require={
        'cython': ['Cython', ],
    },

    entry_points={
        'console_scripts': [
            'F2x=F2x.main:main',
        ],
    },
)
