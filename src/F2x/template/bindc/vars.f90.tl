{% macro dummy_args(args) -%}
    {%- for arg in args -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}

{% macro declare_arg(arg) -%}
    {%- if arg.dims -%}
        {{ declare_arg_array(arg) }}
    {%- elif arg.strlen -%}
        {{ declare_arg_str(arg) }}
    {%- elif arg.ftype -%}
        {{ declare_arg_dtype(arg) }}
    {%- else -%}
        {{ declare_arg_basic(arg) }}
    {%- endif -%}
{%- endmacro %}

{% macro declare_arg_array(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
    {%- if arg.strlen %}
        CHARARCTER({{ arg.strlen }}), DIMENSIONS({{ join_dims(arg) }}) :: {{ arg.name }}_INTERN
    {%- elif arg.ftype %}
        TYPE({{ arg.ftype }}), DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ arg.name }}_INTERN
    {%- else %}
        {{ arg.type }}
        {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}, DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ arg.name }}_INTERN
    {%- endif %}
{%- endmacro %}

{% macro declare_arg_str(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
        CHARACTER({{ arg.strlen }}) :: {{ arg.name }}_INTERN
{%- endmacro %}

{% macro declare_arg_dtype(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
        TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
{%- endmacro %}

{% macro declare_arg_basic(arg) -%}
        {{ arg.type }}
    {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}
    {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
{%- endmacro %}

{% macro cast_arg(arg) -%}
    {%- if arg.dims %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN, [{{ join_dims(arg) }}])
    {%- elif arg.strlen %}
        CALL C_F_STRING({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- elif arg.ftype %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- endif %}
{%- endmacro %}

{% macro uncast_arg(arg) -%}
    {%- if arg.dims %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
    {%- elif arg.strlen %}
        {%- if arg.intent == 'OUT' %}
        {{ arg.name }} = C_MALLOC({{ arg.strlen }}_8 + 1)
        {%- endif %}
        CALL F_C_STRING({{ arg.name }}_INTERN, {{ arg.name }})
    {%- elif arg.ftype %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
    {%- endif %}
{%- endmacro %}

{% macro join_dims(arg, c=None) -%}
    {%- for dim in arg.dims -%}
        {{ c or dim }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
