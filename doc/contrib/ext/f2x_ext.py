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
import importlib
import os
import zipfile
from distutils.dep_util import newer_group
from typing import Any, List, Tuple, Union

from docutils import nodes
from docutils.statemachine import ViewList
from docutils.parsers.rst import directives
from sphinx import addnodes
from sphinx.domains import Domain
from sphinx.ext.autosummary import Autosummary, autosummary_table
from sphinx.util.docutils import switch_source_input
from sphinx.util.logging import getLogger

from sphinxcontrib.autojinja import jinja

from F2x import template
from F2x.distutils.strategy.base import BuildStrategy


log = getLogger(__name__)

UNDOCUMENTED = "*not documented yet*"
NO_TEMPLATES = "*no templates*"


class F2xSummaryTable(Autosummary):
    """ Print a nice table. """

    required_arguments = 0
    optional_arguments = 0

    has_content = True

    option_spec = {
        'showheader': directives.flag,
    }

    def get_items(self, names):
        return []

    def f2x_ncols(self):
        # type: () -> int
        """ Get number of columns to render. """
        return 3

    def f2x_get_width(self):
        # type: () -> Tuple[int, int, int]
        """ Get widths of columns to render. """
        return 12, 25, 63

    def f2x_get_title(self):
        return None

    def f2x_get_text(self, *args):
        # type: (*Tuple[str, Any, str]) -> List[str, str, str]
        return ['', '', '']

    def get_table(self, items):
        # type: (List[Tuple[str, str, str, str]]) -> List[Union[addnodes.tabular_col_spec, autosummary_table]]  # NOQA
        """ Build a table. Basically taken from :mod:`sphinx.ext.autosummary.Autosummary`. """

        table_spec, table, body = self._get_table_spec()

        for args in items:
            column_texts = self.f2x_get_text(*args)
            self._append_row(body, column_texts)

        return [table_spec, table]

    def _append_row(self, body, column_texts):
        # type: (Any, str) -> None
        table_row = nodes.row('')
        source, line = self.state_machine.get_source_and_line()

        for text in column_texts:
            node = nodes.paragraph('')

            vl = ViewList()
            vl.append(text, f'{source}:{line}:<{self.name}>')

            with switch_source_input(self.state, vl):
                self.state.nested_parse(vl, 0, node)
                while len(node) > 0 and isinstance(node[0], nodes.paragraph):
                    node = node[0]

                table_row.append(nodes.entry('', node))

        body.append(table_row)

    def _get_table_spec(self):
        ncols = self.f2x_ncols()
        column_titles = self.f2x_get_title()

        table_spec = addnodes.tabular_col_spec()
        table_spec['spec'] = r'\X{1}{2}' * ncols

        table_wrap = autosummary_table('')
        table_layout = nodes.table('', classes=['longtable'])
        table_group = nodes.tgroup('', cols=ncols)
        table_body = nodes.tbody('')

        table_wrap.append(table_layout)
        table_layout.append(table_group)

        for width in self.f2x_get_width():
            table_group.append(nodes.colspec('', colwidth=width))

        if self.options.get('showheader', False) and column_titles is not None:
            table_head = nodes.thead('')
            table_group.append(table_head)
            self._append_row(table_head, column_titles)

        table_group.append(table_body)

        return table_spec, table_wrap, table_body


class TemplateSummary(F2xSummaryTable):
    """ Print a nice table with templates. """

    def get_items(self, names):
        # type: (List[str]) -> List[Tuple[str, str, str]]
        """
        Collect templates by name. The fully-qualified package name is
        assumed.

        :param names: Names of templates to load.
        :return: A list of all loaded templates.
        """

        items = []

        for fullname in names:
            pkg, name = fullname.rsplit('.', 1)
            mod = None

            if pkg == template.__name__:
                # Try to load built-in template
                mod = template.get_template(name)

            if mod is None:
                try:
                    # Try to import template package
                    mod = importlib.import_module(fullname, pkg)
                    template.register_template(mod)

                except ImportError:
                    log.warning(f'Could not load template "{fullname}".')

            items.append((name, mod, fullname))

        return items

    def f2x_get_title(self):
        return ['Name', 'Templates', 'Description']

    def f2x_get_text(self, *args):
        # type: (*Tuple[str, Any, str]) -> List[str, str, str]
        """ Get text to render into column of a row. """

        name, mod, fullname = args
        return [
            f'`{fullname}`' if mod is None else f':mod:`{mod.name} <{fullname}>`',
            NO_TEMPLATES if mod is None else ', '.join([f':code:`{template}`' for template in mod.templates]),
            UNDOCUMENTED if mod is None else (mod.__py_doc__ or UNDOCUMENTED),
        ]


class StrategySummary(F2xSummaryTable):
    """
    Print a nice table with :py:class:`BuildStrategy`s with three columns containing:

    1. Plain name
    2. Templates used
    3. Descriptive text
    """

    def get_items(self, names):
        # type: (List[str]) -> List[Tuple[str, BuildStrategy]]
        """ Collect strategies and return them with name. """
        from F2x.distutils.strategy import get_strategy

        items = []
        for name in names:
            mod = get_strategy(name)
            if mod is None:
                log.warning(f'Could not load strategy "{name}".')

            items.append((name, mod))
        return items

    def f2x_get_title(self):
        return ['Name', 'Templates', 'Description']

    def f2x_get_text(self, *args):
        # type: (*Tuple[str, BuildStrategy]) -> List[str, str, str]
        """ Make texts from strategy: plain name, templates, info. """

        name, mod = args
        if mod is None:
            templates = [NO_TEMPLATES]

        else:
            templates = [f':mod:`{template_name} <{template.__name__}>`'
                         for template, template_name, _, _ in mod.templates]

        return [
            f'{name}' if mod is None else f':mod:`{name} <{mod.__module__}.{mod.__class__.__qualname__}>`',
            f'{", ".join(templates)}',
            UNDOCUMENTED if mod is None else (mod.__doc__ or UNDOCUMENTED),
        ]


class F2xDomain(Domain):
    name = 'f2x'
    label = 'F2x'

    object_types = {}

    directives = {
        'templatesummary': TemplateSummary,
        'strategysummary': StrategySummary,
    }

    roles = {}

    initial_data = {}

    indices = []


def zip_examples(app, config):
    for base_dir, example_dir, archive_name in config.f2x_examples:
        example_files = []

        base_path = os.path.join(app.confdir, base_dir)
        archive_path = os.path.join(base_path, archive_name)
        for curr_dir, _, file_names in os.walk(os.path.join(base_path, example_dir)):
            for file_name in file_names:
                example_files.append(os.path.join(curr_dir, file_name))

        if newer_group(example_files, archive_path, 'newer'):
            log.info(f'Creating example archive {base_dir}{os.path.sep}{archive_name}...')
            with zipfile.ZipFile(archive_path, 'w') as archive:
                for file_path in example_files:
                    arch_file_name = os.path.relpath(file_path, base_path)
                    log.debug(f'... adding {file_path} as {arch_file_name}')
                    archive.write(file_path, arch_file_name)


def setup(app):
    app.add_config_value('f2x_examples', [], '')

    app.add_domain(F2xDomain)

    app.connect('config-inited', zip_examples)
