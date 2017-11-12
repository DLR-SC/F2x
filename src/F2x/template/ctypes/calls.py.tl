{#-##################################################################################################################-#}
{#- F2x 'ctypes' template call library.                                                                              -#}
{#-                                                                                                                  -#}
{#- This library provides macros to generate Python methods that handle parameter conversion and calling of ctypes   -#}
{#- function as exported by the 'bindc' template.                                                                    -#}
{#-##################################################################################################################-#}


{# Import a method.

   This configures the interface of an imported wrapper routine.

   :param method: The :type SubDef: or :type FuncDef: node that describes the exported method.
#}
{% macro import_method(method) -%}
library.{{ method.export_name }}.restype =
    {%- if method.ret and method.ret.getter == 'function' %}
        {%- if method.ret.dims %} ctypes.POINTER({{ method.ret.pytype }})
        {%- else %} {{ method.ret.pytype }}
        {%- endif %}
    {%- else %} None
    {%- endif %}
library.{{ method.export_name }}.argtypes = [
    {%- for arg in method.args %}
        {%- if arg.dims %}ctypes.POINTER(ctypes.POINTER({{ arg.pytype }}))
        {%- elif arg.strlen == '*' and arg.pytype == 'ctypes.c_char_p' %} {{arg.pytype}}
        {%- else %}ctypes.POINTER({{ arg.pytype }})
        {%- endif %}, {% endfor -%}
    {%- if method.ret and method.ret.getter == 'subroutine' %}ctypes.POINTER({{ method.ret.pytype }}){% endif -%}
    ]
{%- endmacro %}


{# Generate a method wrapper.

   This provides a Python method that accepts (almost) the same arguments as the exported FORTRAN routine. Pure output
   parameters are omitted. Output parameters are returned along the function result (if any).

   :param method: The :type SubDef: or :type FuncDef: node that defines the exported method.
#}
{% macro export_method(method) -%}
    {%- set callargs = [] -%}
    {%- set retargs = [] -%}
def {{ method.name }}(
    {%- for arg in method.args if not arg.intent == 'OUT' -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}):
    {{ cast_args(method, callargs) }}
    {%- if method.ret and method.ret.getter == 'function' %}
    {{ method.ret.name }} = library.{{ method.export_name }}({{ join_args(callargs) }})
    {%- else %}
        {%- if method.ret %}
            {%- do callargs.append("ctypes.byref(" + method.ret.name + "_INTERN)") %}
    {{ cast_arg(method.ret) }}
        {%- endif %}
    library.{{ method.export_name }}({{ join_args(callargs) }})
    {%- endif %}
    check_error({{ method.name }})
    {%- if method.ret %}
    {{ uncast_ret(method.ret, retargs) }}
    {%- endif %}
    {{ uncast_args(method, retargs) }}
    {%- if retargs %}
    return {{ join_args(retargs) }}
    {%- endif %}
{%- endmacro %}


{# Convert Python paramters to FORTRAN types.

   This macro creates appropriate ctypes instances for each paramter and adds them to the collected call arguements.

   :param method: The :type SubDef: or :type FuncDef: that defines the arugments.
   :param callargs: The list to collect actual call paramters in.
#}
{% macro cast_args(method, callargs) -%}
    {%- for arg in method.args %}
        {%- if arg.strlen == '*' and arg.pytype == 'ctypes.c_char_p' -%}
            {%- do callargs.append(arg.name + "_INTERN") -%}
        {%- else -%}
            {%- do callargs.append("ctypes.byref(" + arg.name + "_INTERN)") -%}
        {%- endif -%}
        {{ cast_arg(arg) }}
    {%- endfor %}
{%- endmacro %}


{# Convert a single Python variable to a FORTRAN representation.
#}
{% macro cast_arg(arg) -%}
    {%- if arg.dims %}
        {%- if arg.ftype or arg.strlen %}
    # pass
        {%- else %}
            {%- if not arg.intent == 'OUT' %}
    {{ arg.name }}_ARRAY = numpy.array({{ arg.name }}, {{ arg.pytype }}, order='F')
            {%- else %}
    {{ arg.name }}_ARRAY = numpy.empty(({% for dim in arg.dims %}{{ dim }}, {% endfor %}), dtype={{ arg.pytype }}, order='F')
            {%- endif %}
    {{ arg.name }}_INTERN = {{ arg.name }}_ARRAY.ctypes.data_as(ctypes.POINTER({{ arg.pytype }}))
        {%- endif %}
    {%- elif arg.strlen %}
        {%- if not arg.intent == 'OUT' %}
    {{ arg.name }}_BUFFER = ctypes.create_string_buffer(
        {{- arg.name }}.encode("{{ config.get("parser", "encoding") }}"){% if arg.strlen != '*' %}, {{ arg.strlen }}{% endif %})
    {{ arg.name }}_INTERN = ctypes.cast({{ arg.name }}_BUFFER, ctypes.c_char_p)
        {%- else %}
    {{ arg.name }}_INTERN = ctypes.c_char_p()
        {%- endif %}
    {%- elif arg.ftype %}
        {%- if arg.intent == 'OUT' %}
    {{ arg.name }} = {{ arg.ftype }}()
        {%- endif %}
    {{ arg.name }}_INTERN = {{ arg.name }}.ptr
    {%- else %}
    {{ arg.name }}_INTERN = {{ arg.pytype }}({% if not arg.intent == 'OUT' %}{{ arg.name }}{% endif %})
    {%- endif %}
{%- endmacro %}


{# Cast output arguments back to their Python types.
#}
{% macro uncast_args(method, retargs) -%}
    {%- for arg in method.args if not arg.intent == 'IN' %}
        {{ uncast_arg(arg, retargs) }}
    {%- endfor %}
{%- endmacro %}


{# Cast function return value to its Python type.
#}
{% macro uncast_ret(arg, retargs) -%}
    {%- if arg.dims %}
        {%- if arg.ftype or arg.strlen %}
    # pass
        {%- else %}
            {%- do retargs.append(arg.name + "_ARRAY") -%}
    {{ arg.name }}_CARRAY = array_from_pointer({{ arg.pytype }}, [{% for dim in arg.dims %}{{ dim }}{% if not loop.last %} * {% endif %}{% endfor %}], {{ arg.name }})
    {{ arg.name }}_ARRAY = numpy.ndarray(({% for dim in arg.dims %}{{ dim }}, {% endfor %}), dtype={{ arg.pytype }}, buffer={{ arg.name }}_CARRAY, order='F')
        {%- endif %}
    {%- elif arg.strlen %}
        {%- do retargs.append(arg.name + '_INTERN.value.decode("' + config.get("parser", "encoding") + '").rstrip()') -%}
    {%- elif arg.ftype %}
        {%- do retargs.append(arg.name + '_OUT') -%}
    {{ arg.name }}_OUT = {{ arg.ftype }}(ctypes.c_void_p({{ arg.name }}), False)
    {%- else %}
        {%- do retargs.append(arg.name) -%}
    {%- endif %}
{%- endmacro %}


{# Cast an output argument back to a Python type.
#}
{% macro uncast_arg(arg, retargs) -%}
    {%- if arg.dims %}
        {%- if arg.ftype or arg.strlen %}
    # pass
        {%- else %}
            {%- do retargs.append(arg.name + '_ARRAY') -%}
        {%- endif %}
    {%- elif arg.strlen %}
        {%- do retargs.append(arg.name + '_INTERN.value.decode("' + config.get("parser", "encoding") + '").rstrip()') -%}
    {%- elif arg.ftype %}
        {%- do retargs.append(arg.name) -%}
    {%- else %}
        {%- do retargs.append(arg.name + '_INTERN.value') -%}
    {%- endif %}
{%- endmacro %}


{# Join a list of strings by comma. #}
{% macro join_args(args) -%}
    {%- for arg in args -%}
        {{ arg }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
