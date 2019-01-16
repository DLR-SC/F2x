{%- import "bindc/marshal/names.f90.tl" as names %}


{% macro bindc_arg_names(method) -%}
    {%- for arg in method.args -%}
        {{ names.input_arg(arg) }}
        {%- if arg.dims and arg.dims[0] == 0 %}, {{ names.input_arg(arg) }}_SIZE{% endif -%}
        {%- if arg.strlen and arg.strlen in ('*', ':') %}, {{ names.input_arg(arg) }}_LEN{% endif -%}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro bindc_arg_types(method) -%}
    {%- for arg in method.args %}
        {{ arg.type }}
        {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif -%}
        {%- if arg.intent == 'IN' %}, VALUE{% endif %} :: {{ names.input_arg(arg) }}

        {%- if arg.dims and arg.dims[0] == 0 %}
    INTEGER
            {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif -%}
            {%- if arg.intent == 'IN' %}, VALUE{% endif %} :: {{ names.input_arg(arg) }}_SIZE
        {%- endif -%}

        {%- if arg.strlen and arg.strlen in ('*', ':') %}
    INTEGER
            {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %}
            {%- if arg.intent == 'IN' %}, VALUE{% endif %} :: {{ names.input_arg(arg) }}_LEN
        {%- endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro bindc_arg_type(var) -%}
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
    {% if var.type == 'INTEGER' -%}
        {% if var.kind -%}
            {% if var.kind == '2' -%}
                int16_t
            {% elif var.kind == '4' -%}
                int32_t
            {% elif var.kind == '8' -%}
                int64_t
            {% else -%}
                int
            {% endif -%}
        {% else -%}
            int
        {% endif -%}
    {% elif var.type == 'REAL' -%}
        {% if var.kind -%}
            {% if var.kind == '4' -%}
                float
            {% elif var.kind == '8' -%}
                double
            {% else -%}
                double
            {% endif -%}
        {% else -%}
            float
        {% endif -%}
    {% elif var.type == 'LOGICAL' -%}
    {% endif -%}
{%- endmacro %}


{% macro def_args(method) -%}
    {%- for arg in method.args if arg.intent != 'OUT' -%}
        {{ names.input_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro call_args(method) -%}
    {%- for arg in method.args if arg.intent != 'OUT' -%}
        {{ names.input_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{% macro output_args(method) -%}
    {%- for arg in method.args if arg.intent != 'IN' -%}
        {{ names.output_arg(arg.name) }}
    {%- endfor -%}
{%- endmacro %}
