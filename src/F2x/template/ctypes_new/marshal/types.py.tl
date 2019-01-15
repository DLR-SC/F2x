{#- Marshal types. -#}


{%- macro ctypes_type(arg) -%}
    {%- if arg.ftype or arg.strlen -%}
        ctypes.c_void_p
    {%- else -%}
        {%- if arg.type == 'INTEGER' -%}
            ctypes.c_int
            {%- if arg.kind -%}
                {{ arg.kind * 8 }}
            {%- endif -%}
        {%- elif arg.type == 'LOGICAL' -%}
            ctypes.c_int
            {%- if arg.kind -%}
                {{ arg.kind * 8 }}
            {%- endif -%}
        {%- elif arg.type == 'REAL' -%}
            {%- if arg.kind == 8 -%}
                ctypes.c_double
            {%- else -%}
                ctypes.c_float
            {%- endif -%}
        {%- endif -%}
    {%- endif -%}
{%- endmacro -%}
