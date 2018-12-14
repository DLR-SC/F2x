{%- import "pyx/marshal/names.pyx.tl" as names %}


{% macro cdef_return_type(method) -%}
    {%- if method.ret -%}
        {{ cdef_primitive(method.ret) }}
    {%- else -%}
        void
    {%- endif -%}
{%- endmacro %}


{% macro cdef_args(method) -%}
    {%- for arg in method.args if arg.intent != 'OUT' -%}
        {{ cdef_type(arg) }} {{ names.input_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro cdef_type(var) -%}
    {% if var.dims -%}
    {% else -%}
        {%- if var.ftype -%}
        {%- elif var.strlen -%}
        {%- else -%}
            {{ cdef_primitive(var) }}
        {%- endif -%}
    {%- endif -%}
{%- endmacro %}


{% macro cdef_primitive(var) -%}
    {%- if var.type in ('INTEGER', 'LOGICAL') -%}
        {%- if var.kind -%}
            {%- if var.kind == '2' -%}
                int16_t
            {%- elif var.kind == '4' -%}
                int32_t
            {%- elif var.kind == '8' -%}
                int64_t
            {%- else -%}
                int
            {%- endif -%}
        {%- else -%}
            int
        {%- endif -%}
    {%- elif var.type == 'REAL' -%}
        {%- if var.kind -%}
            {%- if var.kind == '4' -%}
                float
            {%- elif var.kind == '8' -%}
                double
            {%- else -%}
                double
            {%- endif -%}
        {%- else -%}
            float
        {%- endif -%}
    {%- endif -%}
{%- endmacro %}


{% macro def_args(method) -%}
    {%- for arg in method.args if arg.intent != 'OUT' -%}
        {{ names.input_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro call_args(method) -%}
    {%- for arg in method.args -%}
        {{ names.input_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro output_args(method) -%}
    {%- for arg in method.args if arg.intent != 'IN' -%}
        {{ names.output_arg(arg.name) }}
    {%- endfor -%}
{%- endmacro %}