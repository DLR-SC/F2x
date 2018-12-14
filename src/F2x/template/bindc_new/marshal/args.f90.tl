{#-##################################################################################################################-#}
{#- F2x 'bindc' argument helper library.                                                                             -#}
{#-                                                                                                                  -#}
{#- This library contains macros to assemble argument lists.                                                         -#}
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


{#- Import helper library. -#}
{%- import "marshal/names.f90.tl" as names with context -%}
{%- import "marshal/types.f90.tl" as types with context -%}


{#- Assemble list with dummy args.

    :param args: The list of `VarDecl` nodes to assemble.
-#}
{%- macro dummy_args(args) -%}
    {%- for arg in args -%}
        {{ dummy_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}


{#- Add dummy arg to list of args.

    This also adds possibly required additional arguments to determine the size of dynamic arrays.

    :param arg: A `VarDecl` node to add to dummy argument list.
-#}
{%- macro dummy_arg(arg) -%}
    {%- if arg.dims and 0 in arg.dims -%}{{ names.arg_name(arg) }}_SIZE, {% endif -%}
    {%- if arg.strlen in ('*', ':') %}{{ names.arg_name(arg) }}_LENGTH, {% endif -%}
    {{ names.arg_name(arg) }}
{%- endmacro -%}


{#- Declare argument types.

    :param args: The list of `VarDecl` to generate.
-#}
{%- macro declare_args(args) -%}
    {% for arg in args -%}
        {{ declare_arg(arg) }}
    {% endfor -%}
{%- endmacro -%}


{#- Declare a single argument type.

    This also generates possibly required helpers e.g., for dynamic sized arrays.

    :param arg: The `VarDecl` node to generate type for.
-#}
{%- macro declare_arg(arg) -%}
    {%- if arg.dims %}
        {%- if 0 in arg.dims %}
        INTEGER(KIND={{ platform.default_type_kind('INTEGER') }}){{ arg_intent(arg, 'IN') }}, DIMENSION({{ arg.dims|length }}) :: {{ names.arg_name(arg) }}_SIZE
        {%- endif %}
        {%- if arg.ftype %}
        {{ types.fortran_type(arg) }}{{ arg_intent(arg) }}, DIMENSION({{ arg_dims(arg) }}), POINTER :: {{ names.arg_name(arg) }}_INTERN
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }} :: {{ names.arg_name(arg) }}
        {%- elif arg.strlen %}
            {%- if arg.strlen in ('*', ':') %}
        INTEGER(KIND={{ platform.default_type_kind('INTEGER') }}){{ arg_intent(arg, 'IN') }} :: {{ names.arg_name(arg) }}_LENGTH
        {{ types.fortran_type(arg) }}, DIMENSION({{ arg_dims(arg) }}), POINTER :: {{ names.arg_name(arg) }}_INTERN
            {%- else %}
        {{ types.fortran_type(arg) }}, DIMENSION({{ arg_dims(arg) }}), POINTER :: {{ names.arg_name(arg) }}_INTERN
            {%- endif %}
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }}, DIMENSION({{ arg_dims(arg) }}) :: {{ names.arg_name(arg) }}
        {%- else %}
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }}, DIMENSION({{ arg_dims(arg) }}) :: {{ names.arg_name(arg) }}
        {%- endif %}
    {% else %}
        {%- if arg.ftype %}
        {{ types.fortran_type(arg) }}, DIMENSION({{ arg_dims(arg) }}), POINTER :: {{ names.arg_name(arg) }}_INTERN
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }} :: {{ names.arg_name(arg) }}
        {%- elif arg.strlen %}
            {%- if arg.strlen in ('*', ':') %}
        INTEGER(KIND={{ platform.default_type_kind('INTEGER') }}){{ arg_intent(arg, 'IN') }} :: {{ names.arg_name(arg) }}_LENGTH
        {{ types.fortran_type(arg) }}, POINTER :: {{ names.arg_name(arg) }}_INTERN
            {%- else %}
        {{ types.fortran_type(arg) }}, POINTER :: {{ names.arg_name(arg) }}_INTERN
            {%- endif %}
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }} :: {{ names.arg_name(arg) }}
        {%- else %}
        {{ types.bindc_type(arg) }}{{ arg_intent(arg) }} :: {{ names.arg_name(arg) }}
        {%- endif %}
    {%- endif %}
{%- endmacro -%}


{#- Cast args from C to Fortran.

    :param args: The list of `VarDecl` nodes to cast.
-#}
{%- macro cast_args(args) -%}
    {%- for arg in args if arg.intent != 'OUT' -%}
        {{ cast_arg(arg) }}
    {%- endfor -%}
{%- endmacro -%}


{%- macro cast_arg(arg) -%}
    {%- if arg.dims %}
        {%- if arg.ftype %}
        CALL C_F_POINTER({{ names.arg_name(arg) }}, {{ names.arg_name(arg) }}_INTERN, [{{ arg_dims(arg) }}])
        {%- elif arg.strlen %}
        CALL C_F_POINTER({{ names.arg_name(arg) }}, {{ names.arg_name(arg) }}_INTERN, [{{ arg_dims(arg) }}])
        {%- endif %}
    {% else %}
        {%- if arg.ftype %}
        CALL C_F_POINTER({{ names.arg_name(arg) }}, {{ names.arg_name(arg) }}_INTERN)
        {%- elif arg.strlen %}
        CALL C_F_POINTER({{ names.arg_name(arg) }}, {{ names.arg_name(arg) }}_INTERN)
        {%- endif %}
    {%- endif %}
{%- endmacro -%}


{%- macro uncast_args(args) -%}
    {%- for arg in args -%}
        {{ uncast_arg(arg) }}
    {%- endfor -%}
{%- endmacro -%}


{%- macro uncast_arg(arg) -%}
{%- endmacro -%}


{%- macro call_args(args) -%}
    {%- for arg in args -%}
        {{ call_arg(arg) }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}


{%- macro call_arg(arg) -%}
    {{ names.arg_name(arg) }}
    {%- if arg.ftype or arg.strlen -%}
        _INTERN
    {%- endif -%}
{%- endmacro -%}


{%- macro arg_intent(arg, default=None) -%}
    {%- if arg.intent or default -%}
    , INTENT({{ arg.intent or default }})
    {%- endif -%}
{%- endmacro -%}


{%- macro arg_dims(arg) -%}
    {%- for dim in arg.dims -%}
        {%- if dim == 0 -%}
        {{ names.arg_name(arg) }}_SIZE({{ loop.index }})
        {%- else -%}
        {{ dim }}
        {%- endif -%}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro -%}
