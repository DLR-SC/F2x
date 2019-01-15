{#- Marshalling types. -#}


{%- macro bindc_type(arg) -%}
    {%- if arg.ftype -%}
        TYPE(C_PTR)
    {%- elif arg.strlen -%}
        {%- if arg.strlen in (':', '*') -%}
        CHARACTER({{ names.arg_name(arg) }}_LENGTH)
        {%- else -%}
        CHARACTER({{ arg.strlen }})
        {%- endif -%}
    {%- else -%}
        {{ arg.type|upper }}(KIND={{ arg.kind or platform.get_default_kind(arg.type) }})
    {%- endif -%}
{%- endmacro -%}


{%- macro fortran_type(arg) -%}
    {%- if arg.ftype -%}
        TYPE({{ arg.ftype }})
    {%- elif arg.strlen -%}
        {%- if arg.strlen in (':', '*') -%}
        CHARACTER({{ names.arg_name(arg) }}_LENGTH)
        {%- else -%}
        CHARACTER({{ arg.strlen }})
        {%- endif -%}
    {%- else -%}
        {{ arg.type|upper }}
    {%- endif -%}
{%- endmacro -%}

