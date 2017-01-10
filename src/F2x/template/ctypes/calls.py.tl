{% macro import_method(module, method) -%}
{{ module.name }}.{{ method.export_name }}.restype =
    {%- if method.ret and method.ret.getter == 'function' %}
        {%- if method.ret.dims %} ctypes.POINTER({{ method.ret.pytype }})
        {%- else %} {{ method.ret.pytype }}
        {%- endif %}
    {%- else %} None
    {%- endif %}
{{ module.name }}.{{ method.export_name }}.argtypes = [
    {%- for arg in method.args %}
        {%- if arg.dims %}ctypes.POINTER(ctypes.POINTER({{ arg.pytype }}))
        {%- else %}ctypes.POINTER({{ arg.pytype }})
        {%- endif %}, {% endfor -%}
    {%- if method.ret and method.ret.getter == 'subroutine' %}ctypes.POINTER({{ method.ret.pytype }}){% endif -%}
    ]
{%- endmacro %}

{% macro export_method(module, method) -%}
    {%- set callargs = [] -%}
    {%- set retargs = [] -%}
def {{ method.name }}(
    {%- for arg in method.args if not arg.intent == 'OUT' -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}):
    {{ cast_args(method, callargs) }}
    {%- if method.ret and method.ret.getter == 'function' %}
    {{ method.ret.name }} = {{ module.name }}.{{ method.export_name }}({{ join_args(callargs) }})
    {%- else %}
        {%- if method.ret %}
            {%- do callargs.append("ctypes.byref(" + method.ret.name + "_INTERN)") %}
    {{ cast_arg(method.ret) }}
        {%- endif %}
    {{ module.name }}.{{ method.export_name }}({{ join_args(callargs) }})
    {%- endif %}
    {%- if method.ret %}
    {{ uncast_ret(method.ret, retargs) }}
    {%- endif %}
    {{ uncast_args(method, retargs) }}
    {%- if retargs %}
    return {{ join_args(retargs) }}
    {%- endif %}
{%- endmacro %}

{% macro cast_args(method, callargs) -%}
    {%- for arg in method.args %}
        {%- do callargs.append("ctypes.byref(" + arg.name + "_INTERN)") -%}
        {{ cast_arg(arg) }}
    {%- endfor %}
{%- endmacro %}

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
        {%- if arg.intent == 'IN' %}
    {{ arg.name }}_INTERN = ctypes.c_char_p({{ arg.name }}.encode("{{ config.get("parser", "encoding") }}"))
        {%- elif arg.intent == 'INOUT' %}
    {{ arg.name }}_BUFFER = ctypes.create_string_buffer(
        {{- arg.name }}.encode("{{ config.get("parser", "encoding") }}"), {{ arg.strlen }})
    {{ arg.name }}_INTERN = ctypes.cast({{ arg.name }}_BUFFER, ctypes.c_char_p)
        {%- else %}
    {{ arg.name }}_INTERN = ctypes.c_char_p()
        {%- endif %}
    {%- elif arg.ftype %}
        {%- if arg.intent == 'OUT' %}
    {{ arg.name }} = {{ arg.ftype }}()
        {%- endif %}
    {{ arg.name }}_INTERN = ctypes.c_void_p({{ arg.name }}.ptr)
    {%- else %}
    {{ arg.name }}_INTERN = {{ arg.pytype }}({% if not arg.intent == 'OUT' %}{{ arg.name }}{% endif %})
    {%- endif %}
{%- endmacro %}

{% macro uncast_args(method, retargs) -%}
    {%- for arg in method.args if not arg.intent == 'IN' %}
        {{ uncast_arg(arg, retargs) }}
    {%- endfor %}
{%- endmacro %}

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
        {%- do retargs.append(arg.name + '_INTERN.value.decode("' + config.get("parser", "encoding") + '")') -%}
    {%- elif arg.ftype %}
    # {{ arg.ftype }} {{ arg.name }}
    {%- else %}
        {%- do retargs.append(arg.name) -%}
    {%- endif %}
{%- endmacro %}

{% macro uncast_arg(arg, retargs) -%}
    {%- if arg.dims %}
        {%- if arg.ftype or arg.strlen %}
    # pass
        {%- else %}
            {%- do retargs.append(arg.name + '_ARRAY') -%}
        {%- endif %}
    {%- elif arg.strlen %}
        {%- do retargs.append(arg.name + '_INTERN.value.decode("' + config.get("parser", "encoding") + '")') -%}
    {%- elif arg.ftype %}
    # {{ arg.ftype }} {{ arg.name }}
    {%- else %}
        {%- do retargs.append(arg.name + '_INTERN.value') -%}
    {%- endif %}
{%- endmacro %}

{% macro join_args(args) -%}
    {%- for arg in args -%}
        {{ arg }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
