{% import "calls.py.tl" as calls with context %}
{% import "types.py.tl" as types %}

import ctypes
import os

import numpy

from F2x.lib.python.ftype import FType, Field, getter, setter, constructor, destructor, allocator

_lib = '{{ config.get('generate', 'dll') }}'
_path = os.path.join(os.path.dirname(__file__), _lib)

{{ module.name }} = ctypes.cdll.LoadLibrary(_path)

{% for type in module.types %}
{{ types.export_type(module, type) }}
{% endfor %}

########################################################################################################################
# Exported methods.
{%- for method in module.methods %}

# {{ method.name }}
{{ calls.import_method(module, method) }}
{{ calls.export_method(module, method) }}
{%- endfor %}
