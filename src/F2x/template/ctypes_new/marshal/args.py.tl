{#- Marshal arguments -#}


{%- import "marshal/types.py.tl" as types with context -%}
{%- import "marshal/names.py.tl" as names with context -%}


{%- macro dummy_args(args) -%}
    {%- for arg in args if arg.intent != 'OUT' -%}
        {{ names.py_arg_name(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}


{%- macro ctypes_arg_types(args) -%}
    {%- for arg in args -%}
        {{ ctypes_arg_type(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}


{%- macro ctypes_arg_type(arg) -%}
    {%- if arg.dims and arg.dims.0 == 0 -%}ctypes.POINTER(ctypes.c_int), {% endif -%}
    {%- if arg.strlen in ('*', ':') %}ctypes.POINTER(ctypes.c_int), {% endif -%}
    {%- if arg.ftype or arg.strlen -%}
        ctypes.c_void_p
    {%- else -%}
        ctypes.POINTER({{ types.ctypes_type(arg) }})
    {%- endif -%}
{%- endmacro -%}


{%- macro cast_args(args) -%}
    {%- for arg in args if arg.intent != 'OUT' -%}
        {{ cast_arg(arg) }}
    {%- endfor -%}
{%- endmacro -%}


{%- macro cast_arg(arg) -%}
    {%- if arg.dims %}
        {%- if arg.ftype %}

        {%- elif arg.strlen %}

        {%- else %}
    if not isinstance({{ names.py_arg_name(arg) }}, numpy.ndarray) or not {{ names.py_arg_name(arg) }}.flags.f_contiguous:
        {{ names.py_arg_name(arg) }} = numpy.array({{ names.py_arg_name(arg) }}, dtype={{ types.ctypes_type(arg) }}, order='F')
    {{ names.py_arg_name(arg) }}_ct = {{ names.py_arg_name(arg) }}.ctypes.data_as(ctypes.POINTER({{ types.ctypes_type(arg) }}))
        {%- endif %}

        {%- if 0 in arg.dims %}
    {{ names.py_arg_name(arg) }}_size = numpy.array({{ names.py_arg_name(arg) }}.shape)
        {%- endif %}
    {% else %}
        {%- if arg.ftype %}

        {%- elif arg.strlen %}
    {{ names.py_arg_name(arg) }}_bytes = {{ names.py_arg_name(arg) }}.encode('{{ config.get() }}')
            {%- if arg.strlen not in (':', '*') %}
    {{ names.py_arg_name(arg) }}_bytes += b' ' * ({{ arg.strlen }} - len({{ names.py_arg_name(arg) }}_bytes))
            {%- endif %}
    {{ names.py_arg_name(arg) }}_ct = ctypes.cast({{ names.py_arg_name(arg) }}_bytes, ctypes.c_void_p)
            {%- if arg.strlen in (':', '*') %}
    {{ names.py_arg_name(arg) }}_length = len({{ names.py_arg_name(arg) }}_bytes)
            {%- endif %}
        {%- else %}
    {{ names.py_arg_name(arg) }}_ct = {{ types.ctypes_type(arg) }}({{ names.py_arg_name(arg) }})
        {%- endif %}
    {%- endif %}
{%- endmacro -%}


{%- macro uncast_args(args) -%}
    {%- for arg in args -%}
        {{ uncast_arg(arg) }}
    {%- endfor -%}
{%- endmacro -%}


{%- macro uncast_arg(arg) -%}
{%- endmacro -%}


{%- macro call_args(args) -%}
    {%- for arg in args -%}
        {{ call_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}


{%- macro call_arg(arg) -%}
    {%- if arg.dims and 0 in arg.dims -%}
        {{ names.py_arg_name(arg) }}_size.ctypes.data_as(ctypes.POINTER(ctypes,c_int)), {% endif -%}

    {%- if arg.strlen and arg.strlen in ('*', ':') -%}
        ctypes.byref({{ names.py_arg_name(arg) }}_length), {% endif -%}

    {%- if arg.ftype -%}

    {%- elif arg.dims or arg.strlen -%}
        {{ names.py_arg_name(arg) }}_ct
    {%- else -%}
        ctypes.byref({{ names.py_arg_name(arg) }}_ct)
    {%- endif -%}
{%- endmacro -%}
