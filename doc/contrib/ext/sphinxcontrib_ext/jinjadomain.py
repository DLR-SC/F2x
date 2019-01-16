"""
    sphinxcontrib.jinjadomain
    ~~~~~~~~~~~~~~~~~~~~~~~~

    The jinja domain for documenting jinja templates.

    :copyright: Copyright 2012 by Jaka Hudoklin
    :license: BSD, see LICENSE for details.

"""

import re
import os

from sphinx import addnodes
from sphinx.domains import Domain, ObjType, Index
from sphinx.directives import ObjectDescription
from sphinx.util.docfields import GroupedField, TypedField

def jinja_resource_anchor(method, path):
    path = re.sub(r'[<>:/]', '-', path)
    return method.lower() + '-' + path

class JinjaResource(ObjectDescription):

    doc_field_types = [
        TypedField('parameter', label='Parameters',
                   names=('param', 'parameter', 'arg', 'argument'),
                   typerolename='obj', typenames=('paramtype', 'type')),
    ]

    method = "template"

    def handle_signature(self, sig, signode):
        method = self.method.upper() + ' '
        signode += addnodes.desc_name(method, method)
        signode += addnodes.desc_name(sig, sig)

        fullname = "Template" + ' ' + sig
        signode['method'] = self.method
        signode['path'] = sig
        signode['fullname'] = fullname
        return (fullname, self.method, sig)

    def needs_arglist(self):
        return False

    def add_target_and_index(self, name_cls, sig, signode):
        signode['ids'].append(jinja_resource_anchor(*name_cls[1:]))
        self.env.domaindata['jinja'][self.method][sig] = (self.env.docname, '')

    def get_index_text(self, modname, name):
        return ''

class JinjaIndex(Index):

    name = 'jinjatemplates'
    localname = 'templates'
    shortname = 'templates'

    def __init__(self, *args, **kwargs):
        super(JinjaIndex, self).__init__(*args, **kwargs)

    def grouping_prefix(self, path):
        return os.path.split(path)[0]

    def generate(self, docnames=None):
        content = {}
        items = ((method, path, info)
            for method, routes in self.domain.routes.items()
            for path, info in routes.items())
        items = sorted(items, key=lambda item: item[1])
        for method, path, info in items:
            entries = content.setdefault(self.grouping_prefix(path), [])
            entries.append([
                path, 0, info[0],
                jinja_resource_anchor(method, path), '', '', info[1]
            ])
        content = list(content.items())
        content.sort(key=lambda k: k[0])
        return (content, True)

class JinjaDomain(Domain):
    """Jinja domain."""

    name = 'jinja'
    label = 'jinja'

    object_types = {
        'template': ObjType('template', 'template', 'obj'),
    }

    directives = {
        'template': JinjaResource,
    }

    initial_data = {
        'template': {}, # path: (docname, synopsis)
    }

    indices = [JinjaIndex]

    @property
    def routes(self):
        return dict((key, self.data[key]) for key in self.object_types)

    def clear_doc(self, docname):
        for typ, routes in self.routes.items():
            for path, info in list(routes.items()):
                if info[0] == docname:
                    del routes[path]

    def get_objects(self):
        for method, routes in self.routes.items():
            for path, info in routes.items():
                anchor = jinja_resource_anchor(method, path)
                yield (path, path, method, info[0], anchor, 1)

def setup(app):
    app.add_domain(JinjaDomain)
