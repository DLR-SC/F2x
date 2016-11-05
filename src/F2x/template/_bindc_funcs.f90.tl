{#####################################################################################################################}
{# F2x-Template library: _bindc_funcs.f90.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bindc_f90.t' template for exported subroutines ans functions.          #}
{#####################################################################################################################}

{#===================================================================================================================#}
{# Top level macros for use in templates                                                                             #}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Simple exported subroutine.                                                                                       #}
{% macro routine(sub, export_name) -%}
{%- set callargs = [] -%}
{%- set outargs = [] -%}
    SUBROUTINE {{ export_name.upper() }}({{ dummy_args(sub.args) }}) BIND(C, name="{{  export_name }}")
        {{ arg_types(sub.args, callargs, outargs) }}

        {{ arg_casts(sub.args) }}
        CALL {{ sub.name }}({{ join_list(callargs) }})
        {{ out_casts(sub.args) }}
    END SUBROUTINE
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Simple exported function.                                                                                         #}
{% macro function(func, export_name) -%}
{%- set callargs = [] -%}
{%- set outargs = [] -%}
    FUNCTION {{ export_name.upper() }}({{ dummy_args(func.args) }}) BIND(C, name="{{  export_name }}")
        {{ arg_types(func.args, callargs, outargs) }}{% if func.ret.dims %}
        TYPE(C_PTR) :: {{  export_name.upper() }}
        {{ type_spec(func.ret) }}, POINTER :: {{ export_name.upper() }}_ARRAY{% else %}
        {{ type_spec(func.ret) }} :: {{ export_name.upper() }}{% if func.ret.ftype %}
        TYPE({{ func.ret.ftype }}), POINTER :: {{ export_name.upper() }}_INTERN{% endif %}{% endif %}

        {{ arg_casts(func.args) }}
    {%- if func.ret.dims %}
        ALLOCATE({{ export_name.upper() }}_ARRAY({{ join_list(func.ret.dims) }}))
        {{ export_name.upper() }}_ARRAY({{ dims_list(func.ret.dims) }}) = {{ func.name }}({{ join_list(callargs) }})
        {{ export_name.upper() }} = C_LOC({{ export_name.upper() }}_ARRAY)
    {%- elif func.ret.ftype %}
        ALLOCATE({{ export_name.upper() }}_INTERN)
        {{ export_name.upper() }}_INTERN = {{ func.name }}({{ join_list(callargs) }})
        {{ export_name.upper() }} = C_LOC({{ export_name.upper() }}_INTERN)
    {%- else %}
        {{ export_name.upper() }} = {{ func.name }}({{ join_list(callargs) }})
    {%- endif %}
        {{ out_casts(func.args) }}
    END FUNCTION
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Exported function with return value as output parameter (arrays and strings).                                     #}
{% macro subfunc(func, export_name) -%}
{%- set callargs = [] -%}
{%- set outargs = [] -%}
    SUBROUTINE {{ export_name.upper() }}({% if func.args %}{{ dummy_args(func.args) }}, {% endif %}RESULT_VALUE) BIND(C, name="{{  export_name }}")
        {{ arg_types(func.args, callargs, outargs) }}{% if func.ret.strlen %}
        {{ type_spec(func.ret) }} :: RESULT_VALUE
        CHARACTER(LEN={{ func.ret.strlen }}) :: RESULT_VALUE_INTERN{% endif %}

        {{ arg_casts(func.args) }}
        {% if func.ret.strlen %}
        RESULT_VALUE_INTERN = {{ func.name }}({{ join_list(callargs) }})
        CALL F_C_STRING(RESULT_VALUE_INTERN, RESULT_VALUE)
        {% endif %}
        {{ out_casts(func.args) }}
    END SUBROUTINE
{%- endmacro %}


{#===================================================================================================================#}
{# Internal helper macros                                                                                            #}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Compile list of comma-separated values.                                                                           #}
{% macro join_list(args) -%}
    {%- for arg in args -%}
    {{ arg }}
    {%- if not loop.last %}, {% endif -%}
    {% endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Create list of dimensions.                                                                                        #}
{% macro dims_list(dims) -%}
    {%- for dim in dims -%}
    :
    {%- if not loop.last %}, {% endif -%}
    {% endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Compile list with dummy args.                                                                                     #}
{% macro dummy_args(args) -%}
{%- for arg in args -%}
{{ arg.name }}{% if not loop.last %}, {% endif %}
{%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type declaration of an argument.                                                                                  #}
{% macro type_spec(arg) -%}
    {{ arg.type }}{% if arg.kind %}(KIND={{ arg.kind }}){% endif %}
{%- if arg.dims %}, DIMENSION({{ dims_list(arg.dims) }})
{%- elif arg.intent %}, INTENT({{ arg.intent }})
{%- endif -%}
{%- if not arg.dims and arg.intent == 'IN' -%}
	, VALUE
{%- endif -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Declare dummy args (and internal shadows where required).                                                         #}
{% macro arg_types(args, callargs, outargs) -%}
{%- for arg in args %}
    {%- if arg.dims %}
    {%- do callargs.append(arg.name + '_INTERN') %}
        TYPE(C_PTR), INTENT({{ arg.intent }}) :: {{ arg.name }}
        {{ type_spec(arg) }}, POINTER :: {{ arg.name }}_INTERN
    {%- else %}
        {{ type_spec(arg) }} :: {{ arg.name }}
        {%- if arg.ftype %}
        {%- do callargs.append(arg.name + '_INTERN') %}
        TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
        {%- elif arg.strlen %}
        {%- do callargs.append(arg.name + '_STR') %}
        CHARACTER(LEN={{ arg.strlen }}) :: {{ arg.name }}_STR
        {%- else %}
        {%- do callargs.append(arg.name) %}
        {%- endif %}
    {%- endif %}

    {%- if arg.intent == 'IN' or arg.intent == 'INOUT' %}{% do outargs.append(arg) %}{% endif -%}
{%- endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Declare dummy args (and internal shadows where required).                                                         #}
{% macro arg_casts(args) -%}
{%- for arg in args -%}
    {%- if arg.dims %}
        {%- if arg.intent == 'OUT' %}
        ALLOCATE({{ arg.name }}_INTERN({{ join_list(arg.dims) }}))
        {%- else %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN, [{{ join_list(arg.dims) }}])
        {%- endif %}
    {%- elif arg.strlen %}
        CALL C_F_STRING({{ arg.name }}, {{ arg.name }}_STR)
    {%- elif arg.ftype %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- endif %}
{%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Declare dummy args (and internal shadows where required).                                                         #}
{% macro out_casts(args) -%}
{%- for arg in args if arg.intent == 'OUT' or arg.intent == 'INOUT' -%}
    {%- if arg.dims %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
    {%- elif arg.strlen %}
        CALL F_C_STRING({{ arg.name }}_STR, {{ arg.name }}, {{ arg.strlen }})
    {%- elif arg.ftype %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
    {%- endif %}
{%- endfor -%}
{%- endmacro %}
