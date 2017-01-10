{#####################################################################################################################}
{# F2x-Template library: _ctypes_funcs.py.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_glue.py.t' template for subprocess and function access.               #}
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
    {%- set callargs = [] -%}
    {%- set outargs = [] -%}
{{ module.name }}.{{ export_name }}.argtypes = [{{ join_types(func.args) }}]
    {%- if func.ret.dims %}
{{ module.name }}.{{ export_name }}.restype = ctypes.POINTER({{ func.ret.pytype }})
    {%- else %}
{{ module.name }}.{{ export_name }}.restype = {{ func.ret.pytype }}
    {%- endif %}
def {{ func.name }}({{ join_args(func.args) }}):
    {{ prepare_args(func.args, callargs, outargs) }}
    result = {{ module.name }}.{{ export_name }}({{ join_list(callargs) }})

    {%- if func.ret.ftype %}
    return {{ func.ret.ftype }}(result)
    {%- elif func.ret.strlen %}
    return result.value.decode('{{ config.get('parser', 'encoding') }}')
    {%- elif func.ret.dims|length > 1 %}
    return F2{{ func.ret.type }}Array(({{ join_list(func.ret.dims) }}), ptr=result)
    {%- else %}
    return result
        {%- if func.ret.dims %}[:
            {%- for dim in func.ret.dims if dim %}{{ dim }}{% if not loop.last %} * {% endif %}{% endfor %}]
        {%- endif %}
    {%- endif -%}

    {%- if outargs -%}
    , {{ join_list(outargs) }}
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Function via subroutine call: Translate input parameters and return output parameters.                            #}
{% macro subfunc(sub, export_name) -%}
    {%- set callargs = [] -%}
    {%- set outargs = [] -%}
{{ module.name }}.{{ export_name }}.argtypes = [
    {%- if sub.args %}{{ join_types(sub.args) }}, {% endif %}
    {%- if sub.ret.strlen %}ctypes.c_char_p{% else %}ctypes.c_void_p{% endif %}]
{{ module.name }}.{{ export_name }}.restype = None
def {{ sub.name }}({{ join_args(sub.args) }}):
    {%- if sub.ret.strlen -%}
        {%- do outargs.append('result.value.decode("' + config.get('parser', 'encoding') + '").rstrip()') %}
    result = ctypes.create_string_buffer({{ sub.ret.strlen }})
    result_ptr = ctypes.cast(result, ctypes.c_char_p)
    {%- else %}
    # Unsupported return type {{ arg.type }}
    {%- endif %}
    {{ prepare_args(sub.args, callargs, outargs) }}
    {%- if sub.ret.strlen -%}{%- do callargs.append('result_ptr') -%}{%- endif %}
    {{ module.name }}.{{ export_name }}({{ join_list(callargs) }})
    return {{ join_list(outargs) }}
{%- endmacro %}


{#===================================================================================================================#}
{# Helper macros.                                                                                                    #}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Create (input) argument list.                                                                                     #}
{% macro join_args(args) -%}
    {%- for arg in args if not arg.intent == 'OUT' -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif %}
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
                ctypes.POINTER(ctypes.POINTER({{ arg.pytype }}))
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
            {#- Rest is kept pretty generic. -#}
            {%- else -%}
                ctypes.POINTER({{ arg.pytype }})
            {%- endif -%}
        {%- endif -%}

        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Convert (input) arguments where needed, prepare output arguments.                                                 #}
{% macro prepare_args(args, callargs, outargs) -%}
    {%- for arg in args -%}
        {#- Arrays have to be converted to appropriate pointers... -#}
        {%- if arg.dims %}
            {%- if arg.ftype %}
                {%- do callargs.append(None) %}
                {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(None) -%}{% endif %}
    # TODO prepare Derived Type
            {%- elif arg.dims|length == 1 and arg.dims[0] %}
                {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') -%}
                {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}
                    {%- do outargs.append('{0}_intern[:{1}]'.format(arg.name, arg.dims[0])) -%}
                {%- endif %}

                {%- if arg.intent == 'OUT' %}
=======
    {%- elif arg.dims|length == 1 and arg.dims[0] %}
    {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') -%}
    {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(arg.name + '_array[:]') -%}{%- endif %}
    {%- if arg.intent == 'OUT' %}
>>>>>>> cleanup
    {{ arg.name }}_intern = ctypes.POINTER({{ arg.pytype }})()
                {%- else %}
    {{ arg.name }}_array = ({{ arg.pytype }}{% for dim in arg.dims if dim %} * {{ dim }}{% endfor %})(*{{ arg.name }})
    {{ arg.name }}_intern = ctypes.cast({{ arg.name }}_array, ctypes.POINTER({{ arg.pytype }}))
<<<<<<< HEAD
                {%- endif %}
            {%- elif arg.dims|length > 1 %}
                {%- do callargs.append('ctypes.byref(' + arg.name + '.ptr)') -%}
                {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(arg.name) -%}{%- endif %}
                {%- if arg.intent == 'OUT' %}
    {{ arg.name }} = F2{{ arg.type }}Array(({{ join_list(arg.dims) }}))
                {%- endif %}
            {%- else %}
                {%- if arg.intent == 'OUT' %}
                    {%- do callargs.append(None) -%}
                    {%- do outargs.append(None) -%}
=======
    {%- endif %}
    {%- elif arg.dims|length > 1 %}
    {%- do callargs.append('ctypes.byref(' + arg.name + '.ptr)') -%}
    {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}{%- do outargs.append(arg.name) -%}{%- endif %}
    {%- if arg.intent == 'OUT' %}
    {{ arg.name }} = F2{{ arg.type }}Array(({{ join_list(arg.dims) }}))
    {%- endif %}
    {%- else %}
    {%- if arg.intent == 'OUT' %}
    {%- do callargs.append(None) -%}
    {%- do outargs.append(None) -%}
>>>>>>> cleanup
    # Cannot handle dynamic out arrays
                {%- elif arg.intent == 'INOUT' %}
                    {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') -%}
                    {%- do outargs.append(arg.name + '_array[:]') %}
    {{ arg.name }}_array = ({{ arg.pytype }} * len({{ arg.name }}))(*{{ arg.name }})
    {{ arg.name }}_intern = ctypes.cast({{ arg.name }}_array, ctypes.POINTER({{ arg.pytype }}))
                {%- else %}
                    {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') -%}
    {{ arg.name }}_array = ({{ arg.pytype }} * len({{ arg.name }}))(
                    {%- if not arg.intent == 'OUT' %}*{{ arg.name }}{% endif %})
    {{ arg.name }}_intern = ctypes.cast({{ arg.name }}_array, ctypes.POINTER({{ arg.pytype }}))
                {%- endif %}
            {%- endif %}
    
        {#- ... as do DerivedTypes (which carry their own pointer) ... -#}
        {%- elif arg.ftype %}
            {%- if not arg.intent or arg.intent == 'IN' %}
                {%- do callargs.append(arg.name + '._ptr') %}
            {%- elif arg.intent == 'INOUT' %}
                {%- do callargs.append('ctypes.byref(ctypes.cast(' + arg.name + '._ptr, ctypes.c_void_p))') %}
                {%- do outargs.append(arg.name) %}
            {%- else %}
                {%- do callargs.append('ctypes.byref(ctypes.cast(' + arg.name + '_intern._ptr, ctypes.c_void_p))') %}
                {%- do outargs.append(arg.name + '_intern') %}
    {{ arg.name }}_intern = {{ arg.ftype }}(None, False)
            {%- endif %}
    
        {#- ... strings are passed via buffer ...#}
        {%- elif arg.strlen %}
            {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}
                {%- do callargs.append('ctypes.byref(' + arg.name + '_ptr)') %}
                {%- do outargs.append(arg.name + '_intern.value.decode("' + config.get('parser', 'encoding') + '")') -%}
            {%- else %}
                {%- do callargs.append(arg.name + '_ptr') %}
            {%- endif %}
    {{ arg.name }}_intern = ctypes.create_string_buffer(
            {%- if not arg.intent == 'OUT' -%}
                {{ arg.name }}.encode('{{ config.get('parser', 'encoding') }}'), {% endif -%}
            {{ arg.strlen }})
    {{ arg.name }}_ptr = ctypes.cast({{ arg.name }}_intern, ctypes.c_char_p)
    
        {#- ... and basic types are only special in INOUT/OUT mode. -#}
        {%- else %}
            {%- if not arg.intent or arg.intent == 'IN' %}
                {%- do callargs.append(arg.name) %}
            {%- else %}
                {%- do callargs.append('ctypes.byref(' + arg.name + '_intern)') %}
                {%- do outargs.append(arg.name + '_intern.value') %}
    {{ arg.name }}_intern = {{ arg.pytype }}({% if arg.intent == 'INOUT' %}{{ arg.name }}{% endif %})
<<<<<<< HEAD
            {%- endif %}
        {%- endif %}
    {%- endfor -%}
=======
    {%- endif %}

    {%- endif %}

    {%- endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{#- Convert output arguments.                                                                                       -#}
{% macro cleanup_args(args) -%}
    {%- for arg in args if arg.intent != 'IN' -%}
        {%- if arg.dims|length == 1 and arg.intent == 'OUT' %}
    {{ arg.name }}_array = ({{ arg.pytype }}
            {%- for dim in arg.dims %} * {{ dim }}{% endfor -%}
    ).from_address(ctypes.addressof({{ arg.name }}_intern.contents))
        {%- endif %}
    {%- endfor -%}
{%- endmacro %}
