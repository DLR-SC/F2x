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
from typing import Any, List, Tuple, Union

from docutils import nodes
from docutils.statemachine import ViewList
from sphinx import addnodes
from sphinx.ext.autosummary import Autosummary, autosummary_table
from sphinx.util.docutils import switch_source_input

from F2x import template
from F2x.distutils.strategy.base import BuildStrategy


UNDOCUMENTED = "*not documented yet*"


class TemplateSummary(Autosummary):
    """ Print a nice table with templates. """

    def get_items(self, names):
        # type: (List[str]) -> List[Tuple[str, str, str]]
        """
        Collect templates by name. The fully-qualifed package name is
        assumed.

        :param names: Names of templates to load.
        :return: A list of all loaded templates.
        """

        items = []
        for fullname in names:
            *pkg, name = fullname.split('.')
            pkg = '.'.join(pkg)
            mod = None

            if pkg == template.__name__:
                mod = template.get_template(name)

            if mod is None:
                try:
                    mod = importlib.import_module(fullname, pkg)
                    template.register_template(mod)
                except ImportError:
                    continue

            items.append((name, mod, fullname))
        return items

    def f2x_ncols(self):
        # type: () -> int
        """ Get number of columns to render. """
        return 3

    def f2x_get_width(self):
        # type: () -> Tuple[int, int, int]
        """ Get widths of columns to render. """
        return 12, 25, 63

    def f2x_get_text(self, *args):
        # type: (*Tuple[str, Any, str]) -> List[str, str, str]
        """ Get text to render into column of a row. """

        name, mod, fullname = args
        return [
            f':mod:`{mod.name} <{fullname}>`',
            ', '.join([f':code:`{template}`' for template in mod.templates]),
            mod.__doc__ or UNDOCUMENTED,
        ]

    def get_table(self, items):
        # type: (List[Tuple[str, str, str, str]]) -> List[Union[addnodes.tabular_col_spec, autosummary_table]]  # NOQA
        """ Build a table. Basically taken from :mod:`sphinx.ext.autosummary.Autosummary`. """

        # Flexible cols now.
        cols = self.f2x_ncols()
        table_spec = addnodes.tabular_col_spec()
        table_spec['spec'] = r'\X{1}{2}' * cols

        table = autosummary_table('')
        real_table = nodes.table('', classes=['longtable'])
        table.append(real_table)
        group = nodes.tgroup('', cols=cols)
        real_table.append(group)
        for w in self.f2x_get_width():
            group.append(nodes.colspec('', colwidth=100/cols))
        body = nodes.tbody('')
        group.append(body)

        def append_row(*column_texts):
            # type: (str) -> None
            row = nodes.row('')
            source, line = self.state_machine.get_source_and_line()
            for text in column_texts:
                node = nodes.paragraph('')
                vl = ViewList()
                vl.append(text, '%s:%d:<autosummary>' % (source, line))
                with switch_source_input(self.state, vl):
                    self.state.nested_parse(vl, 0, node)
                    try:
                        if isinstance(node[0], nodes.paragraph):
                            node = node[0]
                    except IndexError:
                        pass
                    row.append(nodes.entry('', node))
            body.append(row)

        for item in items:
            col_text = self.f2x_get_text(*item)
            append_row(*col_text)

        return [table_spec, table]


class StrategySummary(TemplateSummary):
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
                continue

            items.append((name, mod))
        return items

    def f2x_get_text(self, *args):
        # type: (*Tuple[str, BuildStrategy]) -> List[str, str, str]
        """ Make texts from strategy: plain name, templates, info. """

        name, mod = args
        templates = [f':mod:`{template_name} <{template.__name__}>`'
                     for template, template_name, _, _ in mod.templates]
        return [
            f':mod:`{name} <{mod.__module__}.{mod.__class__.__qualname__}>`',
            f'{", ".join(templates)}',
            mod.__doc__ or '*not documented yet*',
        ]


def setup(app):
    app.add_directive('f2x:templatesummary', TemplateSummary)
    app.add_directive('f2x:strategysummary', StrategySummary)
