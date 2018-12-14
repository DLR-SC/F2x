{#-##################################################################################################################-#}
{#- F2x 'bindc' template method library.                                                                             -#}
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


{#- Import helper library. -#}
{%- import "marshal/names.f90.tl" as names with context -%}
{%- import "marshal/args.f90.tl" as args with context -%}


{#- Export a C API for a method.

   This is the main entry point and only selects whether a `FUNCTION` or a `SUBROUTINE` is wrapped.

   :param method: A :type SubDef: or :type FuncDef: node that should be exported.
-#}
{%- macro export(method) -%}
    {% if method.ret -%}
        {{ export_function(method) }}
    {%- else -%}
        {{ export_subroutine(method) }}
    {%- endif %}
{%- endmacro -%}


{#- Export a `FUNCTION`.

   Depending on the return type provides a `FUNCTION` or `SUBROUTINE` with the given export name using `BIND(C)`.

   :param method: A :type FuncDef: node that defines the `FUNCTION` to be exported.
-#}
{%- macro export_function(method) -%}
    ! FUNCTION {{ method.name }}
    {%- if method.ret.getter == 'subroutine' %}
    {{ export_function_sub(method) }}
    {%- else %}
    {{ export_function_func(method) }}
    {%- endif %}
{%- endmacro -%}


{#- Export a `FUNCTION` as `FUNCTION`

   A `FUNCTION` with a `BIND(C)` interface is generated.

   :param method: The `FuncDef` node specifying the exported `FUNCTION`.
-#}
{%- macro export_function_func(method) -%}
    FUNCTION {{ names.method_name(method) }}({{ args.dummy_args(method.args) }}) RESULT({{ args.dummy_arg(method.ret) }}) {{ bindc(method) }}
        {{ args.declare_args(method.args) }}
        {{ args.declare_arg(method.ret) }}

        {{ args.cast_args(method.args) }}
        {{ args.call_arg(method.ret) }} = {{ method.name }}({{ args.call_args(method.args) }})
        {{ args.uncast_args(method.args) }}
        {{ args.uncast_arg(method.ret) }}
    END FUNCTION
{%- endmacro -%}


{#- Export a `FUNCTION` as `SUBROUTINE`

   A `SUBROUTINE` with a `BIND(C)` interface and an additional output arguement for the return value is generated.

   :param method: The `FuncDef` node specifying the exported `FUNCTION`.
-#}
{%- macro export_function_sub(method) -%}
    SUBROUTINE {{ names.method_name(method) }}({{ args.dummy_args(method.args) }}, {{ args.dummy_arg(method.ret) }}) {{ bindc(method) }}
        {{ args.declare_args(method.args) }}
        {{ args.declare_arg(method.ret) }}

        {{ args.cast_args(method.args) }}
        {{ args.call_arg(method.ret) }} = {{ method.name }}({{ args.call_args(method.args) }})
        {{ args.uncast_args(method.args) }}
        {{ args.uncast_arg(method.ret) }}
    END SUBROUTINE
{%- endmacro -%}


{#- Export a `SUBROUTINE`.

   A `SUBROUTINE` with `BIND(C)` interface is generated.

   :param method: The `SubDef` node specifying the exported `SUBROUTINE`.
-#}
{%- macro export_subroutine(method) -%}
    ! SUBROUTINE {{ method.name }}
    SUBROUTINE {{ names.method_name(method) }}({{ args.dummy_args(method.args) }}) {{ bindc(method) }}
        {{ args.declare_args(method.args) }}

        {{ args.cast_args(method.args) }}
        CALL {{ method.name }}({{ args.call_args(method.args) }})
        {{ args.uncast_args(method.args) }}
    END SUBROUTINE
{%- endmacro -%}


{#- Generate a `BIND(C)` statement.

    :param method: The method that should be exported.
-#}
{%- macro bindc(method) -%}
    BIND(C, name="{{ names.bindc_export(method) }}")
{%- endmacro -%}
