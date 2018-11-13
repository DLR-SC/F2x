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
from importlib import import_module

from numpy.distutils import log


_REQUIRED_ATTRS = '__package__', '__file__', '__doc__', 'templates', 'depends', 'modules', 'libraries'


package_dir = os.path.dirname(__file__)
builtin_templates = dict()

get = builtin_templates.get


def register_template(template_mod):
    if not all(map(lambda attr: hasattr(template_mod, attr), _REQUIRED_ATTRS)):
        return

    template_mod.name, *body = template_mod.__doc__.strip().splitlines()
    template_mod.description = "\n".join(body[1:])
    template_mod.package_dir = os.path.dirname(template_mod.__file__)

    log.debug(f'register new template "{template_mod.name}"')
    builtin_templates[template_mod.name] = template_mod


def show_templates():
    for template in builtin_templates.values():
        print(f'{template.name} ({template.__package__}) -> {template.templates}')

        if template.depends:
            print(f'    depends on {template.depends}')

        if template.description:
            print(template.description)

        print()


for entry in os.listdir(package_dir):
    if entry.startswith('_') or not os.path.isdir(os.path.join(package_dir, entry)):
        continue

    builtin_template_mod = import_module(f'{__package__}.{entry}', __package__)
    register_template(builtin_template_mod)
