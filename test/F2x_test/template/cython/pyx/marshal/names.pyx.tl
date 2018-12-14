{% macro method_name(method) -%}
    {{ method.name|lower }}
{%- endmacro %}


{% macro export_name(module, method) -%}
    {{ module.name|lower }}_f_{{ method.name|lower }}
{%- endmacro %}


{% macro input_arg(var) -%}
    {{ var.name|lower }}
{%- endmacro %}


{% macro output_arg(var) -%}
    {{ var.name|lower }}
{%- endmacro %}
