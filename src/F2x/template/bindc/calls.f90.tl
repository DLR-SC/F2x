{#-##################################################################################################################-#}
{# F2x template library used by FORTRAN BIND(C) template to export methods (SUBROUTINEs and FUNCTIONs).               #}
{#                                                                                                                    #}
{# This is a library that provides useful macros for the generation of method wrappers.                               #}
{#-##################################################################################################################-#}
{%- import "vars.f90.tl" as vars -%}

{# Entry point: decide which kind of wrapper to generate. #}
{% macro export_method(module, method) -%}
    {% if method.ret -%}
        {{ export_function(module, method) }}
    {%- else -%}
        {{ export_subroutine(module, method) }}
    {%- endif %}
{%- endmacro %}


{#====================================================================================================================#}
{# FUNCTION exports.                                                                                                  #}

{# Export a FUNCTION either as FUNCTION or SUBROUTINE with extra OUT argument. #}
{% macro export_function(module, method) -%}
    ! FUNCTION {{ method.name }}
    {% if method.ret.dims %}
        {#- Array results are casted to C pointers. -#}
        {{ export_function_array(module, method) }}
    {%- elif method.ret.ftype -%}
        {#- TYPE(...) results need to be passed via OUT argument. -#}
        {{ export_function_ftype(module, method) }}
    {%- elif method.ret.strlen -%}
        {#- CHARACTER(*) results need string conversion. -#}
        {{ export_function_str(module, method) }}
    {%- else -%}
        {{ export_function_basic(module, method) }}
    {%- endif %}
{%- endmacro %}

{# Export a FUNCTION that returns an array. Result is returned as TYPE(C_PTR). #}
{% macro export_function_array(module, method) -%}
    {%- set callargs = [] -%}
    FUNCTION {{ method.export_name.upper() }}({{ vars.dummy_args(method.args) }}) BIND(C, name="{{ method.export_name }}")
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        ALLOCATE({{ method.ret.name }}_INTERN({{ vars.join_dims(method.ret) }}))
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END FUNCTION
{%- endmacro %}

{# Export a FUNCTION with TYPE(...) result which dynamically allocated and returned as TYPE(C_PTR). #}
{% macro export_function_ftype(module, method) -%}
    {%- set callargs = [] -%}
    FUNCTION {{ method.export_name.upper() }}({{ vars.dummy_args(method.args) }}) BIND(C, name="{{ method.export_name }}")
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        ALLOCATE({{ method.ret.name }}_INTERN)
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END FUNCTION
{%- endmacro %}

{# Export a FUNCTION with CHARACTER(*) result that is copied to a C string buffer. #}
{% macro export_function_str(module, method) -%}
    {%- set callargs = [] -%}
    SUBROUTINE {{ method.export_name.upper() }}(
            {%- if method.args %}{{ vars.dummy_args(method.args) }}, {% endif -%}
            {{ method.ret.name }}) BIND(C, name="{{ method.export_name }}")
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END SUBROUTINE
{%- endmacro %}

{# Export a basic FUNCTION with simple return type. #}
{% macro export_function_basic(module, method) -%}
    {%- set callargs = [] -%}
    FUNCTION {{ method.export_name.upper() }}({{ vars.dummy_args(method.args) }}) BIND(C, name="{{ method.export_name }}")
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        {{ method.ret.name }} = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
    END FUNCTION
{%- endmacro %}


{#====================================================================================================================#}
{# SUBROUTINE exports.                                                                                                #}

{# Export a SUBROUTINE. #}
{% macro export_subroutine(module, method) -%}
    {%- set callargs = [] -%}
    ! SUBROUTINE {{ method.name }}
    SUBROUTINE {{ method.export_name.upper() }}({{ vars.dummy_args(method.args) }}) BIND(C, name="{{ method.export_name }}")
        {{- declare_args(method) }}
        {{ cast_args(method, callargs) }}
        CALL {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
    END SUBROUTINE
{%- endmacro %}


{#====================================================================================================================#}
{# Common helpers.                                                                                                    #}

{# Add declarations for the dummy arguments. This also creates shadow variables with FORTRAN types where required. #}
{% macro declare_args(method) -%}
    {%- for arg in method.args %}
        {{ vars.declare_arg(arg) }}
    {%- endfor %}
{%- endmacro %}

{# Cast arguments to their FORTRAN type if required. This also gathers the names of the arguments that shdould be
   passed to the callee. #}
{% macro cast_args(method, callargs) -%}
    {%- for arg in method.args -%}
        {%- if arg.dims or arg.strlen or arg.ftype %}
            {%- do callargs.append(arg.name + '_INTERN') -%}
        {{ vars.cast_arg(arg) }}
        {%- else %}
            {%- do callargs.append(arg.name) -%}
        {%- endif %}
    {%- endfor %}
{%- endmacro %}

{% macro uncast_args(method) -%}
    {%- for arg in method.args -%}
        {%- if arg.intent in ("OUT", "INOUT") and (arg.dims or arg.strlen or arg.ftype) -%}
        {{ vars.uncast_arg(arg) }}
        {%- endif %}
    {%- endfor %}
{%- endmacro %}

{# Simply join a list with commas. #}
{% macro join_list(args) -%}
    {%- for arg in args -%}
        {{ arg }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
