{#- Marshalling names.  -#}

{%- macro method_name(method) -%}
    {%- if config.has_option('export', method.name) -%}
        {{ config.get('export', method.name) }}
    {%- else -%}
        {{ module.name|upper }}_{{ method.name|upper }}
    {%- endif -%}
{%- endmacro -%}


{%- macro bindc_export(method) -%}
    {%- if config.has_option('export', method.name) -%}
        {{ config.get('export', method.name)|lower }}
    {%- else -%}
        {{ module.name|lower }}_{{ method.name|lower }}
    {%- endif -%}
{%- endmacro -%}


{%- macro arg_name(arg) -%}
    {{ arg.name|upper }}
{%- endmacro -%}
