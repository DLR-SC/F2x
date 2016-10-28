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
        CALL {{ sub.name }}({% for arg in callargs %}{{ arg }}{% if not loop.last %}, {% endif %}{% endfor %})
    END SUBROUTINE
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Simple exported function.                                                                                         #}
{% macro function(func, export_name) -%}
{%- set callargs = [] -%}
{%- set outargs = [] -%}
    FUNCTION {{ export_name.upper() }}({{ dummy_args(func.args) }}) BIND(C, name="{{  export_name }}")
        {{ arg_types(func.args, callargs, outargs) }}
        {{ type_spec(func.ret) }} :: {{ export_name.upper() }}{% if func.ret.ftype %}
        TYPE({{ func.ret.ftype }}), POINTER :: {{ export_name.upper() }}_INTERN{% endif %}

        {{ arg_casts(func.args) }}
    {%- if func.ret.ftype %}
        ALLOCATE({{ export_name.upper() }}_INTERN)
        {{ export_name.upper() }}_INTERN = {{ func.name }}({% for arg in callargs %}{{ arg }}{% if not loop.last %}, {% endif %}{% endfor %})
        {{ export_name.upper() }} = C_LOC({{ export_name.upper() }}_INTERN)
    {%- else %}
        {{ export_name.upper() }} = {{ func.name }}({% for arg in callargs %}{{ arg }}{% if not loop.last %}, {% endif %}{% endfor %})
    {%- endif %}
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
        CHARACTER(LEN={{ func.ret.strlen }}) :: RESULT_VALUE_INTERN{% elif func.ret.dims %}
        TYPE(C_PTR), INTENT(IN), VALUE :: RESULT_VALUE
        {{ type_spec(func.ret) }}, POINTER :: RESULT_VALUE_INTERN{% endif %}

        {{ arg_casts(func.args) }}
        {% if func.ret.strlen %}
        RESULT_VALUE_INTERN = {{ func.name }}({% for arg in callargs %}{{ arg }}{% if not loop.last %}, {% endif %}{% endfor %})
        CALL F_C_STRING(RESULT_VALUE_INTERN, RESULT_VALUE)
        {% elif func.ret.dims %}
        CALL C_F_POINTER(RESULT_VALUE, RESULT_VALUE_INTERN, [{% for dim in func.ret.dims %}{{ dim }}{% if not loop.last %}, {% endif %}{% endfor %}])
        RESULT_VALUE_INTERN({% for dim in func.ret.dims %}:{% if not loop.last %}, {% endif %}{% endfor %}) = {{ func.name }}({% for arg in callargs %}{{ arg }}{% if not loop.last %}, {% endif %}{% endfor %})
        {% endif %} 
    END SUBROUTINE
{%- endmacro %}


{#===================================================================================================================#}
{# Internal helper macros                                                                                            #}

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
{%- if arg.dims -%}
, DIMENSION({%- for dim in arg.dims -%}:{%- if not loop.last %}, {% endif -%}{%- endfor -%})
{%- endif -%}
{%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %}
{%- if not arg.dims and arg.intent == 'IN' -%}
	, VALUE
{%- endif -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Declare dummy args (and internal shadows where required).                                                         #}
{% macro arg_types(args, callargs, outargs) -%}
{%- for arg in args -%}
        {{ type_spec(arg) }} :: {{ arg.name }}{% if arg.ftype %}{% do callargs.append(arg.name + '_INTERN') %}
        TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN{% elif arg.strlen %}{% do callargs.append(arg.name + '_STR') %}
        CHARACTER(LEN={{ arg.strlen }}) :: {{ arg.name }}_STR{% else %}{% do callargs.append(arg.name) %}{% endif %}
        {% if arg.intent == 'IN' or arg.intent == 'INOUT' %}{% do outargs.append(arg) %}{% endif -%}
{%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Declare dummy args (and internal shadows where required).                                                         #}
{% macro arg_casts(args) -%}
{%- for arg in args -%}
    {%- if arg.ftype %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- endif %}
{%- endfor -%}
{%- endmacro %}
