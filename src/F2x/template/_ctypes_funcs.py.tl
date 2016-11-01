{#####################################################################################################################}
{# F2x-Template library: _ctypes_funcs.py.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bglue.py.t' template for subprocess and function access.               #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Subroutine call: Translate input parameters and return output parameters.                                         #}
{% macro routine(sub, export_name) -%}
{%- set callargs = [] -%}
{%- set outargs = [] -%}
{{ module.name }}.{{ export_name }}.argtypes = [{{ join_types(sub.args) }}]
{{ module.name }}.{{ export_name }}.restype = None
def {{ sub.name }}({{ join_args(sub.args) }}):
    {{ prepare_args(sub.args, callargs, outargs) }}
    {{ module.name }}.{{ export_name }}({{ join_list(callargs) }})
    {%- if outargs %}
    return {{ join_list(outargs) }}
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Function call: Translate input parameters and return result and output parameters.                                #}
{% macro function(func, export_name) -%}
{{ module.name }}.{{ export_name }}.argtypes = []
{{ module.name }}.{{ export_name }}.restype = {{ func.ret.pytype }}
def {{ func.name }}({{ join_args(func.args) }}):
    {{ prepare_args(func.args, callargs, outargs) }}
    result = {{ module.name }}.{{ export_name }}({{ join_list(callargs) }})
    return result
    {%- if outargs -%}
    , {{ join_list(outargs) }}
    {%- endif %}
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

{#-------------------------------------------------------------------------------------------------------------------#}
{# Create (input) argument list.                                                                                     #}
{% macro join_args(args) -%}
{%- for arg in args -%}
{%- if not arg.intent == 'OUT' -%}
{{ arg.name }}
{%- if not loop.last %}, {% endif %}
{%- endif -%}
{%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Compile list of comma-separated values.                                                                           #}
{% macro join_list(args) -%}
    {%- for arg in args -%}
    {{ arg }}
    {%- if not loop.last %}, {% endif -%}
    {% endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Create a list of Python/C types to match wrapped library routines.                                                #}
{% macro join_types(args) -%}
{%- for arg in args -%}

{#- Input types are passed 'by value' ... -#}
{%- if not arg.intent or arg.intent == 'IN' -%}

{#- Arrays are special, however dynamic or not does not matter. -#}
{%- if arg.dims and not arg.ftype -%}
ctypes.POINTER({{ arg.pytype }})

{#- Strings have an own pointer type. -#}
{%- elif arg.strlen -%}
ctypes.c_char_p

{#- Other types are transparent pointers. -#}
{%- else -%}
{{ arg.pytype }}

{%- endif -%}

{#- OUT or INOUT variables need to be passed as pointer. -#}
{%- else -%}

{#- Again, arrays are special but this time need one more level of indirection. -#}
{%- if arg.dims and not arg.ftype -%}
ctypes.POINTER(ctypes.POINTER({{ arg.pytype }}))

{#- Also, strings should be char pointer references again. -#}
{%- elif arg.strlen -%}
ctypes.POINTER(ctypes.c_char_p)

{%- else -%}
{#- Rest is kept pretty generic. -#}
ctypes.POINTER({{ arg.pytype }})

{%- endif -%}
{%- endif -%}

{%- if not loop.last %}, {% endif -%}

{%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Convert (input) arguments where needed, prepare output arguments.                                                 #}
{% macro prepare_args(args, callargs, outargs) -%}
    {%- for arg in args %}

    {#- Arrays have to be converted to appropriate pointers... -#}
    {%- if arg.dims %}

    {%- if arg.ftype %}
    {%- do callargs.append(None) %}
    {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(None) -%}{%- endif %}
    # TODO prepare Derived Type
    
    {%- elif arg.dims[0] %}
    {%- do callargs.append(arg.name + '_intern') %}
    {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(arg.name + '_intern[:]') -%}{%- endif %}
    {{ arg.name }}_intern = ({{ arg.pytype }}{% for dim in arg.dims if dim %} * {{ dim }}{% endfor %})({% if not arg.intent == 'OUT' %}*{{ arg.name }}{% endif %})
    
    {%- else %}
    {%- do callargs.append(None) %}
    {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(None) -%}{%- endif %}
    # Cannot handle dynamic arrays
    
    {%- endif %}
    
    {#- ... as do DerivedTypes (which carry their own pointer) ... -#}
    {%- elif arg.ftype %}
    {%- if not arg.intent or arg.intent == 'IN' %}
    {%- do callargs.append(arg.name + '._ptr') %}
    {%- elif arg.intent == 'INOUT' %}
    {%- do callargs.append('ctypes.byref(' + arg.name + '._ptr)') %}
    {%- do outargs.append(arg.name) %}
    {%- else %}
    # TODO Derived Type output variables.
    {%- do callargs.append(None) %}
    {%- do outargs.append(None) %}
    {%- endif %}
    
    {#- Basic types are only special in INOUT/OUT mode. -#}
    {%- else %}
    {%- if not arg.intent or arg.intent == 'IN' %}
    {%- do callargs.append(arg.name) %}
    {%- else %}
    {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') %}
    {%- do outargs.append(arg.name + '_intern.value') %}
    {{ arg.name }}_intern = {{ arg.pytype }}({% if arg.intent == 'INOUT' %}{{ arg.name }}{% endif %})
    {%- endif %}

    {%- endif %}

    {%- endfor %}
{%- endmacro %}
