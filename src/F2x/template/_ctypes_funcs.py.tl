{#####################################################################################################################}
{# F2x-Template library: _ctypes_funcs.py.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bglue.py.t' template for subprocess and function access.               #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Subroutine call: Translate input parameters and return output parameters.                                         #}
{% macro routine(sub, export_name) -%}
{{ module.name }}.{{ export_name }}.argtypes = []
{{ module.name }}.{{ export_name }}.restype = None
def {{ sub.name }}({{ join_args(sub.args) }}):
    {{ module.name }}.{{ export_name }}()
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Function call: Translate input parameters and return result and output parameters.                                #}
{% macro function(func, export_name) -%}
{{ module.name }}.{{ export_name }}.argtypes = []
{{ module.name }}.{{ export_name }}.restype = {{ func.ret.pytype }}
def {{ func.name }}({{ join_args(func.args) }}):
    return {{ module.name }}.{{ export_name }}()
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Function via subroutine call: Translate input parameters and return output parameters.                            #}
{% macro subfunc(sub, export_name) -%}
{{ module.name }}.{{ export_name }}.argtypes = []
{{ module.name }}.{{ export_name }}.restype = None
def {{ sub.name }}({{ join_args(sub.args) }}):
    {{ module.name }}.{{ export_name }}()
{%- endmacro %}


{#===================================================================================================================#}
{# Helper macros.                                                                                                    #}
{% macro join_args(args) -%}
{%- endmacro %}
