{{ module.name }}
{{ '=' * module.name|length }}

{# Import a method.

   This configures the interface of an imported wrapper routine.

   :param method: The :type SubDef: or :type FuncDef: node that describes the exported method.
#}
{% macro import_method(method) -%}
    {%- if method.ret and method.ret.getter == 'function' %}
        {%- if method.ret.dims %} *{{ method.ret.pytype }}
        {%- else %} {{ method.ret.pytype }}
        {%- endif %}
    {%- else %} void
    {%- endif %} {{ method.name }}(
    {%- for arg in method.args %}
        {%- if arg.dims -%}
            void *{{ arg.name }}
        {%- else %}{{ arg.pytype }} *{{ arg.name }}
        {%- endif %}, {% endfor -%}
    {%- if method.ret and method.ret.getter == 'subroutine' %}{{ method.ret.pytype }} *{{ ret.name }}{% endif -%}
    )
{%- endmacro %}

{% for method in module.methods %}
.. c:function:: {{ import_method(method) }}
{%- endfor %}
