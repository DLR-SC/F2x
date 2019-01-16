{%- import "bindc/marshal/convert.f90.tl" as convert -%}
{%- import "bindc/marshal/names.f90.tl" as names %}
{%- import "bindc/marshal/types.f90.tl" as types %}


{% macro export() -%}
    {%-  for method in module.methods %}

{% if method.ret -%}
    {{ export_function(method) }}
{% else -%}
    {{ export_subroutine(method) }}
{% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro export_function(method) -%}
    ! {{ method.name }}
    FUNCTION {{ names.wrapper_name(module, method) }}({{ types.bindc_arg_names(method) }}) {{ bindc(method) }}
    {%- if method.args %}
    {{ types.input_arg_types(method) }}
    {% endif -%}
    END FUNCTION
{%- endmacro %}


{% macro export_subroutine(method) -%}
    ! {{ method.name }}
    SUBROUTINE {{ names.wrapper_name(module, method) }}({{ types.bindc_arg_names(method) }}) {{ bindc(method) }}
    {% if method.args -%}
    {{ types.bindc_arg_types(method) }}

    {{ convert.input_args(method) }}
    {%- endif %}
    CALL {{ names.method_name(method) }}({{ types.call_args(method) }})

    {%- if method.has_output_args %}
    {{ convert.output_args(method) }}
    {%- endif %}
    END SUBROUTINE
{%- endmacro %}


{% macro bindc(method) -%}
    BIND(C, name="{{ names.export_name(module, method) }}")
{%- endmacro %}
