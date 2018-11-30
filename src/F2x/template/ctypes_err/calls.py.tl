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
library.{{ method.export_name }}_cerr.restype =
    {%- if method.ret and method.ret.getter == 'function' %}
        {%- if method.ret.dims %} ctypes.POINTER({{ method.ret.pytype }})
        {%- else %} {{ method.ret.pytype }}
        {%- endif %}
    {%- else %} None
    {%- endif %}
library.{{ method.export_name }}_cerr.argtypes = [
    {%- for arg in method.args %}
        {%- if arg.dims %}
            {%- if arg.strlen in ('*', ':') %}{% if 0 in arg.dims %}ctypes.POINTER(ctypes.c_int), {% endif %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.POINTER(ctypes.c_char)),
            {%- elif arg.strlen %}{% if 0 in arg.dims %}ctypes.POINTER(ctypes.c_int), {% endif %} ctypes.POINTER(ctypes.POINTER(ctypes.c_char),
            {%- else %}
                {%- if 0 in arg.dims %}ctypes.POINTER(ctypes.c_int), {% endif -%}
                ctypes.POINTER(ctypes.POINTER({{ arg.pytype }}))
            {%- endif %}
        {%- elif arg.strlen in ('*', ':') %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.POINTER(ctypes.c_char))
        {%- elif arg.strlen %}ctypes.POINTER(ctypes.POINTER(ctypes.c_char))
        {%- else %}ctypes.POINTER({{ arg.pytype }}){% endif -%}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
    {%- if method.ret and method.ret.getter == 'subroutine' -%}
        , {% if method.ret.dims %}
            {%- if method.ret.strlen in ('*', ':') %}{% if 0 in method.ret.dims %}ctypes.POINTER(ctypes.c_int), {% endif %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_void_p),
            {%- elif method.ret.strlen %}{% if 0 in method.ret.dims %}ctypes.POINTER(ctypes.c_int), {% endif %} ctypes.POINTER(ctypes.c_void_p),
            {%- else %}
                {%- if 0 in method.ret.dims %}ctypes.POINTER(ctypes.c_int), {% endif -%}
                ctypes.POINTER(ctypes.POINTER({{ method.ret.pytype }}))
            {%- endif %}
        {%- elif method.ret.strlen in ('*', ':') %}ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.POINTER(ctypes.c_char))
        {%- else %}ctypes.POINTER({{ method.ret.pytype }}){% endif -%}
    {%- endif -%}
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
    {{ method.ret.name }} = library.{{ method.export_name }}_cerr({{ join_args(callargs) }})
    {%- else %}
        {%- if method.ret %}
            {%- if 0 in method.ret.dims %}{%- do callargs.append(method.ret.name + "_SIZE") -%}{%- endif -%}
            {%- if method.ret.strlen in ('*', ':') -%}{%- do callargs.append("ctypes.byref(" + method.ret.name + "_LENGTH)") -%}{%- endif -%}
            {%- do callargs.append("ctypes.byref(" + method.ret.name + "_INTERN)") -%}
    {{ cast_arg(method.ret) }}
        {%- endif %}
    library.{{ method.export_name }}_cerr({{ join_args(callargs) }})
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
        {%- if arg.strlen in ('*', ':') -%}
            {%- if arg.dims -%}
                {%- if 0 in arg.dims -%}{%- do callargs.append(arg.name + "_SIZE") -%}{%- endif -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_LENGTH)") -%}
                {%- do callargs.append(arg.name + "_INTERN") -%}
            {%- else -%}
                {%- do callargs.append("ctypes.byref(" + arg.name + "_LENGTH)") -%}
                {%- do callargs.append(arg.name + "_INTERN") -%}
            {%- endif -%}
        {%- elif arg.strlen -%}
            {%- if arg.dims -%}
                {%- if 0 in arg.dims -%}{%- do callargs.append(arg.name + "_SIZE") -%}{%- endif -%}
                {%- do callargs.append(arg.name + "_INTERN") -%}
            {%- else %}
                {%- do callargs.append(arg.name + "_INTERN") -%}
            {%- endif %}
        {%- elif arg.dims and 0 in arg.dims %}
            {%- do callargs.append(arg.name + "_SIZE") %}
            {%- do callargs.append("ctypes.byref(" + arg.name + "_INTERN)") -%}
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
        {%- if 0 in arg.dims %}
    {{ arg.name }}_SIZE_ARRAY = ({{ arg.dims|length }} * ctypes.c_int)()
    {{ arg.name }}_SIZE = ctypes.cast({{ arg.name }}_SIZE_ARRAY, ctypes.POINTER({% if arg.strlen %}ctypes.c_char{% else %}{{ arg.pytype }}{% endif %}))
            {%- if not arg.intent == 'OUT' %}
    {{ arg.name }}_SIZE_ARRAY[:] = {{ arg.name }}_ARRAY.shape
            {%- endif %}
        {%- endif %}

        {%- if arg.strlen %}
            {% if not arg.intent == 'OUT' %}
    if isinstance({{ arg.name }}, numpy.ndarray) and {{ arg.name }}.flags.f_contiguous:
        {{ arg.name }}_ARRAY = {{ arg.name }}
    else:
        {{ arg.name }}_ARRAY = numpy.char.array({{ arg.name }}
                {%- if arg.strlen not in ('*', ':') %}, itemsize={{ arg.strlen }}{% endif %}).encode("{{ config.get("parser", "encoding") }}")
    {{ arg.name }}_LENGTH = ctypes.c_int({{ arg.name }}_ARRAY.itemsize)
    {{ arg.name }}_INTERN = {{ arg.name }}_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
            {% else %}
    {{ arg.name }}_INTERN = ctypes.POINTER(ctypes.c_char)
            {% endif %}
        {%- elif arg.ftype %}
    # pass
        {%- else %}
            {%- if not arg.intent == 'OUT' %}
    if isinstance({{ arg.name }}, numpy.ndarray) and {{ arg.name }}.flags.f_contiguous:
        {{ arg.name }}_ARRAY = {{ arg.name }}
    else:
        {{ arg.name }}_ARRAY = numpy.array({{ arg.name }}, dtype={{ arg.pytype }}, order='F')
    {{ arg.name }}_INTERN = {{ arg.name }}_ARRAY.ctypes.data_as(ctypes.POINTER({{ arg.pytype }}))
            {%- endif %}
    {{ arg.name }}_INTERN = ctypes.POINTER({{ arg.pytype }})()
        {%- endif %}
    {%- elif arg.strlen %}
        {%- if not arg.intent == 'OUT' %}
            {%- if arg.strlen in ('*', ':') %}
    {{ arg.name }}_ARRAY = numpy.char.array([{{ arg.name }}]).encode("{{ config.get("parser", "encoding") }}")
    {{ arg.name }}_LENGTH = ctypes.c_int({{ arg.name }}_ARRAY.itemsize)
    {{ arg.name }}_INTERN = {{ arg.name }}_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
            {%- else %}
    {{ arg.name }}_LENGTH = ctypes.c_int({{ arg.strlen }})
    {{ arg.name }}_ARRAY = numpy.char.array([{{ arg.name }}.ljust({{ arg.strlen }})], itemsize={{ arg.strlen }}).encode("{{ config.get("parser", "encoding") }}")
    {{ arg.name }}_INTERN = {{ arg.name }}_ARRAY.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
            {%- endif %}
        {%- else %}
    {{ arg.name }}_LENGTH = ctypes.c_int()
    {{ arg.name }}_INTERN = ctypes.POINTER(ctypes.c_char)
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
    {{ arg.name }}_ARRAY = array_from_pointer({{ arg.pytype }}, {%- if 0 in arg.dims %}{{ arg.name }}_SIZE_ARRAY[:]{% else %}({{ join_args(arg.dims) }}, ){% endif %}, {{ arg.name }}_INTERN
            {%- if arg.free -%}, dealloc=library.{{ arg.free }}_cerr{%- endif -%})
        {%- endif %}
    {%- elif arg.strlen %}
        {%- do retargs.append(arg.name + '_ARRAY[0].decode("' + config.get("parser", "encoding") + '").rstrip()') -%}
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
    {{ arg.name }}_ARRAY = array_from_pointer(ctypes.c_char,
            {%- if 0 in arg.dims %} {{ arg.name }}_SIZE_ARRAY[:]
            {%- else %} ({{ join_args(arg.dims) }}, )
            {%- endif %}, {{ arg.name }}_INTERN, strlen=
            {%- if arg.strlen in ('*', ':') %}{{ arg.name }}_LENGTH.value
            {%- else %}{{ arg.strlen }}
            {%- endif %}{% if arg.free %}, dealloc=library.{{ arg.free }}_cerr{% endif %})
    {{ arg.name }} = [{{ arg.name }}_CPTR.value[i * {{ arg.name }}_LENGTH.value:(i + 1) * {{ arg.name }}_LENGTH.value].decode("{{ config.get("parser", "encoding") }}") for i in range({{ arg.name }}_SIZE.value)]

            {%- if arg.free %}
    library.{{ arg.free }}_cerr(ctypes.byref({{ arg.name }}_SIZE), ctypes.byref({{ arg.name }}_PTR))
            {%- endif %}

            {%- do retargs.append(arg.name) -%}
        {%- elif arg.ftype %}
    # pass
        {%- else %}
            {%- do retargs.append(arg.name + '_ARRAY') -%}
            {%- if arg.intent == 'OUT' %}
    {{ arg.name }}_ARRAY = array_from_pointer({{ arg.pytype }},
                {%- if 0 in arg.dims %} {{ arg.name }}_SIZE_ARRAY[:]
                {%- else %} ({{ join_args(arg.dims) }}, )
                {%- endif %}, {{ arg.name }}_INTERN
                {%- if arg.free %}, dealloc=library.{{ arg.free }}_cerr{% endif %})
    {%- endif %}
        {%- endif -%}
    {%- elif arg.strlen %}
        {%- do retargs.append(arg.name + '_ARRAY[0].decode("' + config.get("parser", "encoding") + '").rstrip()') -%}
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
