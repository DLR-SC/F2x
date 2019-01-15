{#- Methods. -#}

{#- Import helper library. -#}
{%- import "marshal/args.py.tl" as args with context -%}
{%- import "marshal/names.py.tl" as names with context -%}
{%- import "marshal/types.py.tl" as types with context -%}


{%- macro export(method) -%}
{{ declare_c_method(method) }}

def {{ names.py_method_name(method) }}({{ args.dummy_args(method.args) }}):
    {{ args.cast_args(method.args) }}
    {% if method.ret %}{{ names.py_arg_name(method.ret) }}_ct = {% endif -%}
    library.{{ names.c_method_name(method) }}({{ args.call_args(method.args) }})
    {{ args.uncast_args(method.args) }}
    {%- if method.ret %}
    {{ args.uncast_arg(method.ret) }}
    {%- endif %}
{%- endmacro -%}


{%- macro declare_c_method(method) -%}
library.{{ names.c_method_name(method) }}.restype =
    {%- if method.ret %} {{ types.ctypes_type(method.ret) }}
    {%- else %} None
    {%- endif %}
library.{{ names.c_method_name(method) }}.argtypes = [{{ args.ctypes_arg_types(method.args) }}]
{%- endmacro -%}
