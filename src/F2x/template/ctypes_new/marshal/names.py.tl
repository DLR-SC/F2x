{#- Marshal names. -#}


{%- macro py_method_name(method) -%}
    {{ method.name|lower }}
{%- endmacro -%}


{%- macro py_arg_name(arg) -%}
    {{ arg.name|lower }}
    {%- if arg.name|lower in ('is', 'exec') %}_{% endif -%}
{%- endmacro -%}


{%- macro c_method_name(method) -%}
    {%- if config.has_option('export', method.name) -%}
        {{ config.get('export', method.name) }}
    {%- else -%}
        {{ module.name|lower }}_{{ method.name|lower }}
    {%- endif -%}
{%- endmacro -%}
