{#####################################################################################################################}
{# F2x-Template library: _pinvoke_funcs.cs.tl                                                                        #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bindc.cs.t' template for subroutine and function calling.              #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Import subroutine and create calling interface.                                                                   #}
{% macro sub(export_name, sub, lib_name) -%}
    {%- set callargs = [] -%}
        [DllImport("{{ lib_name }}", EntryPoint="{{ export_name }}")]
        private static extern void {{ export_name }}({{ make_pinvoke_args(sub.args) }});
        public static void {{ sub.name }}({{ make_cs_args(sub.args) }}) {
            {{ prepare_args(sub.args, callargs) }}
            {{ module.name }}.{{ export_name }}({{ join_list(callargs) }});
            {{ cleanup_args(sub.args) }}
        }
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Import function and create calling interface.                                                                     #}
{% macro func(export_name, func, lib_name) -%}
<<<<<<< HEAD
    {%- set callargs = [] -%}
        [DllImport("{{ lib_name }}", EntryPoint="{{ export_name }}")]
        private static extern
        {%- if func.ret.dims %} IntPtr
        {%- else %} {{ func.ret.cstype }}
        {%- endif %} {{ export_name }}({{ make_pinvoke_args(func.args) }});
=======
{%- set callargs = [] -%}
        [DllImport("{{ lib_name }}", EntryPoint="{{ export_name }}")]
        private static extern {% if func.ret.dims %}IntPtr{% else %}{{ func.ret.cstype }}{% endif %} {{ export_name }}({{ make_pinvoke_args(func.args) }});
>>>>>>> cleanup
    {%- if func.ret.ftype %}
        public static {{ func.ret.ftype }} {{ func.name }}({{ make_cs_args(func.args) }}) {
            {{ prepare_args(func.args, callargs) }}
            IntPtr ptr = {{ module.name }}.{{ export_name }}({{ join_list(callargs) }});
            {{ cleanup_args(func.args) }}
            return new {{ func.ret.ftype }}(ptr, false);
        }
    {%- else %}
        public static {{ func.ret.cstype }}{% for dim in func.ret.dims %}[]{% endfor %} {{ func.name }}({{ make_cs_args(func.args) }}) {
            {{ prepare_args(func.args, callargs) }}

            {#- Compile result buffer. #}
            {%- if func.ret.dims %}
            {{ func.ret.cstype }}
            {%- for dim in func.ret.dims %}[]
            {%- endfor %} result = new {{ func.ret.cstype }}
            {%- for dim in func.ret.dims %}[{{ dim }}]
            {%- endfor %};
            IntPtr addr = {{ module.name }}.{{ export_name }}({{ join_list(callargs) }});
            Marshal.Copy(addr, result, 0, {{ func.ret.dims[0] }});
            {%- else %}
            {{ func.ret.cstype }} result = {{ module.name }}.{{ export_name }}({{ join_list(callargs) }});
            {%- endif %}
            {{ cleanup_args(func.args) }}
            return result;
        }
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Import function called via subroutine and create calling interface.                                               #}
{% macro subfunc(export_name, func, lib_name) -%}
<<<<<<< HEAD
    {%- set callargs = [] -%}
=======
{%- set callargs = [] -%}
>>>>>>> cleanup
        [DllImport("{{ lib_name }}", EntryPoint="{{ export_name }}")]
        private static extern void {{ export_name }}(
            {%- if func.args %}{{ make_pinvoke_args(func.args) }}, {% endif -%}
            {% if func.ret.strlen %}StringBuilder{% else %}ref {{ func.ret.cstype }}{% endif %} result);
        public static {% if func.ret.ftype %}{{ func.ret.ftype }}{% else %}{{ func.ret.cstype }}{% endif %} {{ func.name }}({{ make_cs_args(func.args) }}) {
            {{ prepare_args(func.args, callargs) }}
        {%- if func.ret.strlen %}
            StringBuilder result = new StringBuilder({{ func.ret.strlen }});
            {{ module.name }}.{{ export_name }}({% if callargs %}{{ join_list(callargs) }}, {% endif %}result);
            {{ cleanup_args(func.args) }}
            return result.ToString();
        {%- else %}
            {{ func.ret.cstype }} result = {{ func.ret.csinit }};
            {{ module.name }}.{{ export_name }}({% if callargs %}{{ join_list(callargs) }}, {% endif %}ref result);
            {{ cleanup_args(func.args) }}
            return result;
        {%- endif %}
        }
{%- endmacro %}

{#===================================================================================================================#}
{# Helper macros.                                                                                                    #}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Compile list of arguments for native interface call.                                                              #}
{% macro make_pinvoke_args(args) -%}
<<<<<<< HEAD
    {%- for arg in args -%}
        {%- if arg.dims -%}
            [In{% if arg.intent != 'IN' %}, Out{% endif %}] ref {{ arg.cstype }}
            {%- for dim in arg.dims %}[]
            {%- endfor %} {{ arg.name }}
        {%- elif arg.strlen and not arg.intent == 'IN' -%}
            ref StringBuilder {{ arg.name }}
        {%- else -%}
            {%- if not arg.intent == 'IN' %}ref {% endif -%}
            {{ arg.cstype }} {{ arg.name }}
        {%- endif -%}

        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
=======
{%- for arg in args -%}
    {%- if arg.dims -%}
    [In{% if arg.intent != 'IN' %}, Out{% endif %}] ref {{ arg.cstype }}{% for dim in arg.dims %}[]{% endfor %} {{ arg.name }}
    {%- elif arg.strlen and not arg.intent == 'IN' -%}
    ref StringBuilder {{ arg.name }}
    {%- else -%}
    {%- if not arg.intent == 'IN' %}ref {% endif -%}
    {{ arg.cstype }} {{ arg.name }}
    {%- endif -%}
    {%- if not loop.last %}, {% endif -%}
{%- endfor -%}
>>>>>>> cleanup
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Compile list of arguments for C# interface routine.                                                               #}
{% macro make_cs_args(args) -%}
    {%- for arg in args -%}
        {%- if arg.intent == 'OUT' or arg.intent == 'INOUT' %}ref {% endif -%}
        {%- if arg.ftype -%}
            {{ arg.ftype }}
        {%- else -%}
            {{ arg.cstype }}
            {%- for dim in arg.dims %}[]{% endfor -%}
        {%- endif %} {{ arg.name }}

        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Prepare arguments for P/Invoke (convert from C# to C).                                                            #}
{% macro prepare_args(args, callargs) -%}
    {%- for arg in args -%}
        {%- if arg.strlen and not arg.intent == 'IN' -%}
            {%- do callargs.append('ref ' + arg.name + '_ref') -%}
            StringBuilder {{ arg.name }}_ref = new StringBuilder(
            {%- if arg.intent == 'INOUT' %}{{ arg.name }}, {% endif -%}
        {{ arg.strlen }});
        {%- elif arg.ftype -%}
            {%- if arg.intent == 'IN' -%}
                {%- do callargs.append(arg.name + '.Ptr') -%}
            {%- else -%}
                {%- do callargs.append('ref ' + arg.name + '_ptr') -%}
                {%- if arg.intent == 'OUT' -%}
            {{ arg.name }} = new {{ arg.ftype }}();
                {%- endif %}
            IntPtr {{ arg.name }}_ptr = {{ arg.name }}.Ptr;
            {%- endif %}
        {%- elif arg.dims or not arg.intent == 'IN' -%}
            {%- do callargs.append('ref ' + arg.name) -%}
        {%- else -%}
            {%- do callargs.append(arg.name) -%}
        {%- endif -%}
    {%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Cleanup output arguments.                                                                                         #}
{% macro cleanup_args(args) -%}
    {%- for arg in args -%}
        {%- if arg.strlen and not arg.intent == 'IN' -%}
        {{ arg.name }} = {{ arg.name }}_ref.ToString();
        {%- elif arg.ftype and not arg.intent == 'IN' -%}
        {{ arg.name }}.UpdatePtr({{ arg.name }}_ptr);
        {%- endif %}
    {%- endfor -%}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Join a list of values by comma.                                                                                   #}
{% macro join_list(args) -%}
    {%- for arg in args -%}
        {{ arg }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor %}
{%- endmacro %}
