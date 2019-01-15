{#- Sphinx documentation. -#}


{{ module.name }}
{{ '=' * module.name|length }}

.. f:module:: {{ module.name }}

    {{ module.doc }}


{% macro method_args(method) %}
    {%- for arg in method.args -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{% endmacro %}


{# Import a method.

   This configures the interface of an imported wrapper routine.

   :param method: The :type SubDef: or :type FuncDef: node that describes the exported method.
#}
{% macro import_method(method) -%}
    {% if method.ret %}
    .. f:function:: {{ method.ret.type }} {{ method.name }}({{ method_args(method) }})

    {% else %}
    .. f:subroutine:: {{ method.name }}({{ method_args(method) }})

    {% endif %}
{%- endmacro %}

{% for method in module.methods %}
{{ import_method(method) }}
{%- endfor %}
