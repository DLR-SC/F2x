{%- import "pyx/marshal/convert.pyx.tl" as convert -%}
{%- import "pyx/marshal/names.pyx.tl" as names %}
{%- import "pyx/marshal/types.pyx.tl" as types %}


{% macro export() -%}
{%  for method in module.methods %}

{{ extern_cdef(method) }}
def {{ names.method_name(method) }}({{ types.def_args(method) }}):
    {%- if method.args %}
    {{ convert.input_args(method) }}
    {%- endif %}

    {% if method.ret %}result = {% endif -%}
    {{ names.export_name(module, method) }}({{ types.call_args(method) }})

    {%- if method.has_output_args %}
    {{ convert.output_args(method) }}
    {%- endif %}

    {%- if method.ret %}
    {{ convert.result(method) }}
    return result
        {%- if method.has_output_args -%}
            {{ types.output_args(method) }}
        {%- endif %}
    {%- elif method.has_output_args %}
        return {{ types.output_args(method) }}
    {%- endif %}
{%- endfor %}
{%- endmacro %}


{% macro extern_cdef(method) -%}
cdef extern {{ types.cdef_return_type(method) }} {{ names.export_name(module, method) }}({{ types.cdef_args(method) }})
{%- endmacro %}
