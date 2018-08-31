{#-##################################################################################################################-#}
{#- F2x 'ctypes' template call library.                                                                              -#}
{#-                                                                                                                  -#}
{#- This library provides macros to generate Python methods that handle parameter conversion and calling of ctypes   -#}
{#- function as exported by the 'bindc' template.                                                                    -#}
{#-                                                                                                                  -#}
{#- Copyright 2018 German Aerospace Center (DLR)                                                                     -#}
{#-                                                                                                                  -#}
{#- Licensed under the Apache License, Version 2.0 (the "License");                                                  -#}
{#- you may not use this file except in compliance with the License.                                                 -#}
{#- You may obtain a copy of the License at                                                                          -#}
{#-                                                                                                                  -#}
{#-     http://www.apache.org/licenses/LICENSE-2.0                                                                   -#}
{#-                                                                                                                  -#}
{#- Unless required by applicable law or agreed to in writing, software                                              -#}
{#- distributed under the License is distributed on an "AS IS" BASIS,                                                -#}
{#- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                                         -#}
{#- See the License for the specific language governing permissions and                                              -#}
{#- limitations under the License.                                                                                   -#}
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
        {%- if arg.dims %}
            {%- if arg.strlen == '*' %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_void_p)
            {%- elif arg.strlen %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_void_p)
            {%- else %}ctypes.POINTER(ctypes.POINTER({{ arg.pytype }})){% endif %}
        {%- elif arg.strlen == '*' and arg.pytype == 'ctypes.c_char_p' %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER({{ arg.pytype }})
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
            {%- if arg.dims -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_SIZE)") -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_LENGTH)") -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_PTR)") -%}
            {%- else -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_LENGTH)") -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_INTERN)") -%}
            {%- endif -%}
        {%- elif arg.strlen and arg.pytype == 'ctypes.c_char_p' -%}
            {%- if arg.dims -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_SIZE)") -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_PTR)") -%}
            {%- else %}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_INTERN)") -%}
            {%- endif %}
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
        {%- if arg.strlen %}
    {{ arg.name }}_SIZE = ctypes.c_int({% if not arg.intent == 'OUT' %}len({{ arg.name }}){% endif %})
    {{ arg.name }}_LENGTH = ctypes.c_int({% if arg.strlen == '*' %}max(map(len, {{ arg.name }})) - 1{% else %}{{ arg.strlen }}{% endif %})
            {% if not arg.intent == 'OUT' %}
    {{ arg.name }}_INTERN = numpy.chararray(({{ arg.name }}_SIZE.value, ), itemsize={{ arg.name }}_LENGTH.value)
    {{ arg.name }}_INTERN[:] = {{ arg.name }}
    {{ arg.name }}_PTR = {{ arg.name }}_INTERN.ctypes.data_as(ctypes.c_void_p)
            {% else %}
    {{ arg.name }}_PTR = ctypes.c_void_p()
            {% endif %}
        {%- elif arg.ftype %}
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
            {%- if arg.strlen == '*' %}
    {{ arg.name }}_BUFFER = ctypes.create_string_buffer(
        {{- arg.name }}.encode("{{ config.get("parser", "encoding") }}"))
    {{ arg.name }}_INTERN = ctypes.cast({{ arg.name }}_BUFFER, ctypes.c_char_p)
    {{ arg.name }}_LENGTH = ctypes.c_int(len({{ arg.name }}_BUFFER) - 1)
            {%- else %}
    {{ arg.name }}_PADDED = {{ arg.name }}.ljust({{ arg.strlen }})
    {{ arg.name }}_BUFFER = ctypes.create_string_buffer(
        {{- arg.name }}_PADDED.encode("{{ config.get("parser", "encoding") }}"), {{ arg.strlen }})
    {{ arg.name }}_INTERN = ctypes.cast({{ arg.name }}_BUFFER, ctypes.c_char_p)
            {%- endif %}
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
        {%- else -%}
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
        {%- if arg.strlen and arg.intent == 'INOUT' %}
    {{ arg.name }} = [v.decode("{{ config.get("parser", "encoding") }}") for v in {{ arg.name }}_INTERN.tolist()]
            {%- do retargs.append(arg.name) -%}
        {%- elif arg.strlen and arg.intent == 'OUT' %}
    {{ arg.name }}_CPTR = ctypes.cast({{ arg.name }}_PTR, ctypes.c_char_p)
    {{ arg.name }} = [{{ arg.name }}_CPTR.value[i * {{ arg.name }}_LENGTH.value:(i + 1) * {{ arg.name }}_LENGTH.value].decode("{{ config.get("parser", "encoding") }}") for i in range({{ arg.name }}_SIZE.value)]
            {%- do retargs.append(arg.name) -%}
        {%- elif arg.ftype %}
    # pass
        {%- else %}
            {%- do retargs.append(arg.name + '_ARRAY') -%}
    {%- if arg.intent == 'OUT' %}
    {{ arg.name }}_CARRAY = array_from_pointer({{ arg.pytype }}, [{% for dim in arg.dims %}{{ dim }}{% if not loop.last %} * {% endif %}{% endfor %}], {{ arg.name }}_INTERN)
    {{ arg.name }}_ARRAY = numpy.ndarray(({% for dim in arg.dims %}{{ dim }}, {% endfor %}), dtype={{ arg.pytype }}, buffer={{ arg.name }}_CARRAY, order='F')
    {%- endif %}
        {%- endif -%}
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
