{#- Bindings. -#}


{%- macro py_import() -%}
import ctypes
import os

import numpy

    {%- if config.has_section("pyimport") %}
        {%- for imp in config.options("pyimport") %}
from {{ imp }} import {{ config.get("pyimport", imp) }}
        {%- endfor -%}
    {%- endif -%}
{%- endmacro -%}


{%- macro lib_import() %}
library_name = '{{ config.get('generate', 'dll') }}'
library_path = os.path.join(os.path.dirname(__file__), library_name)
library = ctypes.cdll.LoadLibrary(library_path)
{%- endmacro -%}
