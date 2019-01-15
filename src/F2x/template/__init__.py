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
import sys
from importlib import import_module
from types import ModuleType

import logging
log = logging.getLogger(__name__)


# Attributes that every template should have.
_REQUIRED_ATTRS = '__package__', '__file__', '__doc__', 'name', 'templates', 'requires', 'modules', 'libraries'


# Store the path of this package for easier path building.
package_dir = os.path.dirname(__file__)


# Use a simple dictionary with `str` -> `module` mapping as registry.
_templates = dict()


def get_template(name: str) -> ModuleType:
    """
    Retrieve a loaded template from the registry.

    :param name: Name of a loaded template.
    :return: The template module.
    """
    return _templates.get(name)


# Build a regex that can extract `{% import ... %}` blocks from templates.
_imports_find_all = re.compile(r'[{]%-?\s*import\s+["\'](.*)["\']\s+as\s+(\w+)(\s+with\s+context)?\s*-?%[}]').findall


def _find_depends(basedir, files):
    """
    Recursively scan all files for imports and add them as dependency.

    :param basedir: Base search path for template files.
    :param files: List of files that should be searched for imports.
    :return: A list with all files that are imported directly or indirectly by the given files.
    """
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
                import_path = os.path.join(basedir, import_file)
                if import_path not in depends:
                    depends.append(import_path)
                    files.append(import_file)

    return depends


def register_template(template):
    """
    Add a new template to the registry.

    Before the module is registered, some preprocessing is made:

    - Check if all required attributes of the template are set. These are:
        -  `name`: The name of the template. This will later be used to reference the template.
        - `templates`: A list of templates that should be rendered.
        - `requires`: Other templates that need to be rendered in order for this template to work. May be `None`.
        - `modules`: A list of Python modules the rendered output requires to work.
        - `libraries`: A list of libraries the compiled output of this template needs to work. You may
                       use only module names if the required libraries are added to the distribution in any
                       other way. Otherwise use a tuple with the library name and a library spec dict like
                       used for numpy.
        - A docstring.
    - The following attributes are added:
        - `package_dir`: The directory of the template package.
        - `template_files`: A list with full pathes of all template files to be rendered.
        - `depends`: A list of dependencies for all rendered templates.
    - The docstring is extended to reference templates and libraries.

    :param template: The template module to register.
    """
    if not all(map(lambda attr: hasattr(template, attr), _REQUIRED_ATTRS)):
        missing = [attr for attr in _REQUIRED_ATTRS if not hasattr(template, attr)]
        log.warning(f'template "{template.__name__}" missing required attributes: {missing}')
        return

    if not hasattr(template, 'package_dir'):
        template.package_dir = os.path.dirname(template.__file__)
        log.debug(f'initialize template {template.name} ({template.package_dir})...')

        template.template_files = [
            os.path.join(*((package_dir, template_file[1:]) if template_file[0] == '@' else
                           (template.package_dir, template_file)))
            for template_file in (template.templates or [])
        ]
        log.debug(f'   template files: {template.template_files}')

        template.depends = _find_depends(template.package_dir, template.templates)
        log.debug(f'   dependencies: {template.depends}')

        _extend_doc(template)

    old_template = get_template(template.name)
    if old_template:
        log.debug(f'replacing template {template.name} (old pacakge dir: {old_template.package_dir})')

    _templates[template.name] = template


_TOC_HEADER = '.. toctree::\n\n'
_TOC_FOOTER = '\n'
_TOC_ENTRY_TEMPLATE = '    template/{0}\n'
_TOC_ENTRY_LIBRARY = '    lib/{0}/toc'


def _make_toc(title, entry_template, items, header=_TOC_HEADER, footer=_TOC_FOOTER):
    """
    Generate a TOC list for Sphinx.

    :param title: Title of the TOC list (will be rendered as headline).
    :param entry_template: Template for each TOC entry.
    :param items: Items to put into TOC list.
    :param header: The TOC header (default: see `_TOC_HEADER`).
    :param footer: The TOC footer (default: see `_TOC_FOOTER`).

    :return: A text block with the new TOC.
    """
    doc = f'{title}\n' \
          + '=' * len(title) \
          + '\n\n' + header \
          + ''.join(map(entry_template.format, items)) \
          + footer

    return doc


def _extend_doc(template):
    """
    Extend the documentation of the template to include references to templates and libraries.

    :param template: The template module to update.
    """
    template.__py_doc__ = template.__doc__

    if template.templates:
        template_files = []
        # Adjust path (relative to this package)
        for template_file in template.template_files + template.depends:
            filename, _ = os.path.splitext(os.path.basename(template_file))
            if filename.startswith('.'):
                filename = '_' + filename[1:]
            template_file = os.path.join(os.path.dirname(template_file), filename)
            template_files.append(os.path.relpath(template_file, package_dir))

        template.__doc__ += '\n' + _make_toc('Templates', _TOC_ENTRY_TEMPLATE, template_files)

    if template.libraries:
        lib_names = [lib_name for lib_name, _ in template.libraries]
        template.__doc__ += _make_toc('Libraries', _TOC_ENTRY_LIBRARY, lib_names)


def show_templates(out=sys.stdout, full_doc=False):
    """
    Print a nicely formatted list of templates and their documentation.

    :param out: The file to write to.
    """
    import textwrap

    for template in _templates.values():
        docstring = template.__doc__ if (full_doc or not hasattr(template, '__py_doc__')) else template.__py_doc__

        out.write('\n')
        out.write(f'* {template.name} ({template.__package__}) -> {template.templates}\n')

        if template.requires:
            out.write(f'   requires {template.requires}\n\n')

        if template.__py_doc__:
            paragraphs = re.split('\n\n', docstring)

            for para in paragraphs:
                para = ' '.join(para.splitlines())
                para = '\n   '.join(map(str.strip, textwrap.wrap(para)))
                out.write(f'   {para}\n\n')


def get_library_sources(*templates):
    """
    Collect source files for all libraries in the given list of templates.

    :param templates: A list of template names or template modules.
    :return: A list with all library files with full path that are required by the given templates.
    """
    libs = []

    for template in templates:
        # Ensure we have a template module
        if isinstance(template, str):
            template = get_template(template)
            if template is None:
                continue

        for _, lib_info in (template.libraries or []):
            libs += [os.path.join(template.package_dir, source) for source in lib_info['sources']]

    return libs


def _init_templates():
    """
    Initialize registry by scanning this package for templates.
    """
    log.debug(f'scanning {package_dir} for templates...')

    for entry in os.listdir(package_dir):
        if entry.startswith('_') or not os.path.isdir(os.path.join(package_dir, entry)):
            continue

        builtin_template_mod = import_module(f'{__package__}.{entry}', __package__)
        register_template(builtin_template_mod)


def collect_template_data(include=None):
    """
    Collect all data files that are required by any registered template.

    This includes:

    - all template files and their dependencies
    - all library files included in the templates
    - all Python modules included in the templates

    :param include: A set of strings that indicates what to collect. Possible choices are 'templates', 'depends',
                    'libraries', and 'modules'. If nothing is select explicitly, everything will be collected.
    :return: A generator that yields pairs of template and the requested data files.
    """
    if include is None:
        include = {'templates', 'depends', 'libraries', 'modules'}

    for template in _templates.values():
        data = []

        if 'templates' in include:
            data += template.template_files

        if 'depends' in include:
            data += template.depends

        if 'libraries' in include:
            data += get_library_sources(template)

        if 'modules' in include and template.modules is not None:
            data += [os.path.join(template.package_dir, module_file) for module_file in template.modules]

        data = [os.path.relpath(path, template.package_dir) for path in data]
        yield template, data


# Automagically initialize the registry.
if not _templates:
    _init_templates()
