{% macro module_name(module) -%}
    {{ module.name|upper }}_WRAP
{%- endmacro %}


{% macro method_name(method) -%}
    {{ method.name|upper }}
{%- endmacro %}


{% macro wrapper_name(module, method) -%}
    {{ module.name|upper }}_{{ method.name|upper }}_BINDC
{%- endmacro %}


{% macro export_name(module, method) -%}
    {{ module.name|lower }}_f_{{ method.name|lower }}
{%- endmacro %}


{% macro input_arg(var) -%}
    {{ var.name|upper }}
{%- endmacro %}


{% macro output_arg(var) -%}
    {{ var.name|lower }}
{%- endmacro %}
