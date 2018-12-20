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
from importlib import import_module

#from numpy.distutils import log
import logging
log = logging.getLogger(__name__)


# Attributes that every template should have.
_REQUIRED_ATTRS = '__package__', '__file__', '__doc__', 'name', 'templates', 'requires', 'modules', 'libraries'


package_dir = os.path.dirname(__file__)


# Holds the templates that have been loaded.
# type:
_templates = dict()


get_template = _templates.get


_imports_find_all = re.compile(r'[{]%-?\s*import\s+["\'](.*)["\']\s+as\s+(\w+)(\s+with\s+context)?\s*-?%[}]').findall
def _find_depends(basedir, files):
    files = list(files)
    depends = []

    while files:
        filename, *files = files
        if filename.startswith('@'):
            filename = os.path.join(package_dir, filename[1:])
        else:
            filename = os.path.join(basedir, filename)

        with open(filename, 'r') as file:
            data = file.read()
            for import_file, _, _ in _imports_find_all(data):
                if import_file not in depends:
                    depends.append(os.path.join(basedir, import_file))
                    files.append(import_file)

    return depends


_TOC_LIBRARIES = """

.. toctree::
    :caption: Libraries
    
"""

def register_template(template):
    if not all(map(lambda attr: hasattr(template, attr), _REQUIRED_ATTRS)):
        missing = [attr for attr in _REQUIRED_ATTRS if not hasattr(template, attr)]
        log.warning(f'template "{template.__name__}" missing required attributes: {missing}')
        return

    if not hasattr(template, 'package_dir'):
        template.package_dir = os.path.dirname(template.__file__)
        template.depends = _find_depends(template.package_dir, template.templates)

        if template.libraries:
            lib_entry = _TOC_LIBRARIES + '\n'.join([
                f'    lib/{lib_name}/toc' for lib_name, _ in template.libraries
            ])
            template.__doc__ += lib_entry

    if template.name in _templates:
        log.debug(f'replacing template "{template.name}"')
    else:
        log.debug(f'register new template "{template.name}"')
    _templates[template.name] = template


def _print_text(text, pad=2, cols=79):
    text_blocks = []
    current_block = []
    for line in map(str.strip, text.splitlines()):
        if not line:
            text_blocks.append(' '.join(current_block))
            current_block = []
        else:
            current_block.append(line)
    text_blocks.append(' '.join(current_block))

    for text in map(lambda line: line.split(), text_blocks):
        start = 0
        while start < len(text):
            end, length = start, 0
            while end < len(text) and length < cols - pad + 1:
                length += len(text[end])
                end += 1
            print(' ' * pad + ' '.join(text[start:end]))
            start = end
        print()


def show_templates():
    for template in _templates.values():
        print(f'* {template.name} ({template.__package__}) -> {template.templates}')
        if template.requires:
            print(f'    requires {template.requires}')
        if template.__doc__:
            _print_text(template.__doc__)
        print()


def get_library_sources(*templates):
    libs = []
    for template in templates:
        if isinstance(template, str):
            template = get_template(template)
        if template is not None and template.libraries:
            for _, lib_info in template.libraries:
                libs += [os.path.join(template.package_dir, source) for source in lib_info['sources']]
    return libs


def init_templates():
    for entry in os.listdir(package_dir):
        if entry.startswith('_') or not os.path.isdir(os.path.join(package_dir, entry)):
            continue

        builtin_template_mod = import_module(f'{__package__}.{entry}', __package__)
        register_template(builtin_template_mod)


if not _templates:
    init_templates()
