{#-##################################################################################################################-#}
{#- F2x 'bindc' template call library.                                                                               -#}
{#-                                                                                                                  -#}
{#- This library contains macros to export FORTRAN `FUNCTION`s and `SUBROUTINE`s to C. It produces `BIND(C)`         -#}
{#- wrappers that handle parameter conversion.                                                                       -#}
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


{# Import helper library. #}
{%- import "vars.f90.tl" as vars -%}


{# Export a C API for a method.

   This is the main entry point and only selects whether a `FUNCTION` or a `SUBROUTINE` is wrapped.

   :param method: A :type SubDef: or :type FuncDef: node that should be exported.
#}
{% macro export_method(method) -%}
    {% if method.ret -%}
        {{ export_function(method) }}
    {%- else -%}
        {{ export_subroutine(method) }}
    {%- endif %}
{%- endmacro %}


{# Export a `FUNCTION`.

   Depending on the return type provides a `FUNCTION` or `SUBROUTINE` with the given export name using `BIND(C)`.

   :param method: A :type FuncDef: node that defines the `FUNCTION` to be exported.
#}
{% macro export_function(method) -%}
    ! FUNCTION {{ method.name }}
    {% if method.ret.dims %}
        {#- Array results are casted to C pointers. -#}
        {{ export_function_array(method) }}
    {%- elif method.ret.ftype -%}
        {#- TYPE(...) results need to be passed via OUT argument. -#}
        {{ export_function_ftype(method) }}
    {%- elif method.ret.strlen -%}
        {#- CHARACTER(*) results need string conversion. -#}
        {{ export_function_str(method) }}
    {%- else -%}
        {{ export_function_basic(method) }}
    {%- endif %}
{%- endmacro %}


{# Export a `FUNCTION` that returns an array.

   This generates a `FUNCTION` returning the result as `TYPE(C_PTR)`.

   :param method: The :type FuncDef: node that specifies the exported `FUNCTIOǸ`.
#}
{% macro export_function_array(method) -%}
    {%- set callargs = [] -%}
    {{ vars.func_decl(method, True) }}
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        ALLOCATE({{ method.ret.name }}_INTERN({{ vars.join_dims(method.ret) }}))
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END FUNCTION
{%- endmacro %}


{# Export a `FUNCTION` that returns a derived type.

   A `FUNCTION` is generated that returns the address of the result as `TYPE(C_PTR)`.

   :param method: The :type FuncDef: node specifying the the `FUNCTION`.
#}
{% macro export_function_ftype(method) -%}
    {%- set callargs = [] -%}
    {{ vars.func_decl(method, True) }}
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        ALLOCATE({{ method.ret.name }}_INTERN)
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END FUNCTION
{%- endmacro %}


{# Exprort a `FUNCTION` that returns a string.

   A `SUBROUTINE` is generated that returns the result in an additional output argument that contains the string in a
   newly allocated C string buffer.

   :param method: The :type FuncDef: node that specifies the `FUNCITON`.
#}
{% macro export_function_str(method) -%}
    {%- set callargs = [] -%}
    {{ vars.sub_decl(method, True) }}
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        {{ method.ret.name }}_INTERN = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
        {{ vars.uncast_arg(method.ret) }}
    END SUBROUTINE
{%- endmacro %}


{# Export a `FUNCTION`.

   The result is return by the generated `FUNCTION` as is.

   :param method: The :type FuncDef: node that specifies the simple `FUNCTION`.
#}
{% macro export_function_basic(method) -%}
    {%- set callargs = [] -%}
    {{ vars.func_decl(method, True) }}
        {{- declare_args(method) }}
        {{ vars.declare_arg(method.ret) }}
        {{ cast_args(method, callargs) }}
        {{ method.ret.name }} = {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
    END FUNCTION
{%- endmacro %}


{# Export a `SUBROUTINE`.

   A `SUBROUTINE` with `BIND(C)` interface with the given export name is generated.

   :param method: The `SubDef` node specifying the exported `SUBROUTINE`.
#}
{% macro export_subroutine(method) -%}
    {%- set callargs = [] -%}
    ! SUBROUTINE {{ method.name }}
    {{ vars.sub_decl(method, True) }}
        {{- declare_args(method) }}
        {{ cast_args(method, callargs) }}
        CALL {{ method.name }}({{ join_list(callargs) }})
        {{ uncast_args(method) }}
    END SUBROUTINE
{%- endmacro %}


{# Declare dummy arguments and required shadows.

   :param method: The :type SubDef: or :type FuncDef: node for which the dummy arguments and shadow variables should be
                  declared for.
#}
{% macro declare_args(method) -%}
    {%- for arg in method.args %}
        {{ vars.declare_arg(arg) }}
    {%- endfor %}
{%- endmacro %}


{# Cast dummy arguments to internal FORTRAN shadow variables.

   This applies the appropriate conversion where required and adds the name of the actual call argument to the output
   list.

   :param method: The :type SubDef: or :type FuncDef: node for which casting should be applied.
   :param callargs: A list that collects the names of the acutal argument names to be used.
#}
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


{# Cast output arguments back to C types.

   For output arguments the appropriate conversion is applied where required.

   :param method: The :type SubDef: or :type FuncDef: node for which the output arguments should be converted.
#}
{% macro uncast_args(method) -%}
    {%- for arg in method.args -%}
        {%- if arg.intent in ("OUT", "INOUT") and (arg.dims or arg.strlen or arg.ftype) -%}
        {{ vars.uncast_arg(arg) }}
        {%- endif %}
    {%- endfor %}
{%- endmacro %}


{# Simply join a list with commas.

   :param args: The list to be joined.
#}
{% macro join_list(args) -%}
    {%- for arg in args -%}
        {{ arg }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
