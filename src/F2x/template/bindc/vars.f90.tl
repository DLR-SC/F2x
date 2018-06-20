{#-##################################################################################################################-#}
{#- F2x 'bindc' template variable helpers.                                                                           -#}
{#-                                                                                                                  -#}
{#- This module provides macros to handle FORTRAN variables. This contains variable definitions and casting them     -#}
{#- from and to their C representation.                                                                              -#}
{#-##################################################################################################################-#}


{# Output a function declaration.

   Declare a function header complete with dummy args, BIND(C) and exports.

   :param method: The :type FuncDef: node to generate the header for.
#}
{% macro func_decl(method, ifort_dll=False) -%}
    FUNCTION {{ method.export_name.upper() }}({{ dummy_args(method.args) }}) BIND(C, name="{{ method.export_name }}")
	{%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {{ method.export_name }}
	{%- endif %}
{%- endmacro %}


{# Output a subroutine declaration.

   Declare a subroutine header complete with dummy args, BIND(C) and exports.

   :param method: The :type SubDef: node to generate the header for.
#}
{% macro sub_decl(method, ifort_dll=False) -%}
    SUBROUTINE {{ method.export_name.upper() }}(
            {%- if method.args %}{{ dummy_args(method.args) }}{% if method.ret %}, {% endif %}{% endif -%}
            {% if method.ret %}{{ method.ret.name }}{% endif %}) BIND(C, name="{{ method.export_name }}")
	{%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {{ method.export_name }}
	{%- endif %}
{%- endmacro %}


{# Output a comma-seperated list with dummy arguments.

   Join the names of the arguments passed as parameter.

   :param args: A list with :type VarDecl: nodes.
#}
{% macro dummy_args(args) -%}
    {%- for arg in args -%}
        {%- if arg.dims and arg.dims.0 == '0' %}{{ arg.name }}_SIZE, {% endif -%}
        {%- if arg.strlen == '*' -%}{{ arg.name }}_LENGTH, {% endif -%}
        {{ arg.name }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}


{# Declare a variable.

   Produce a variable declaration with a type that can be used by the C API. If required, a shadow variable with the
   suffix '_INTERN' and the corresponding FORTRAN type is declared, too.

   The macro basically decides which concrete macro should be rendered (see below).

   :param arg: A :type VarDecl: node that should be declared.
#}
{% macro declare_arg(arg, name=None) -%}
    {%- if not name %}{% set name=arg.name %}{% endif %}
    {%- if arg.dims -%}
        {{ declare_arg_array(arg, name) }}
    {%- elif arg.strlen -%}
        {{ declare_arg_str(arg, name) }}
    {%- elif arg.ftype -%}
        {{ declare_arg_dtype(arg, name) }}
    {%- else -%}
        {{ declare_arg_basic(arg, name) }}
    {%- endif -%}
{%- endmacro %}


{# Declare an array variable.

   Create a `TYPE(C_PTR)` variable and a pointer variable with the associated FORTRAN type as internal shadow variable.

   :param arg: A :type VarDecl: node specifiying the variable to be declared.
#}
{% macro declare_arg_array(arg, name) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ name }}
    {%- if arg.strlen == '*' %}
        INTEGER, INTENT({{ arg.intent }}) :: {{ name }}_SIZE, {{ name }}_LENGTH
        CHARACTER({{ name }}_LENGTH), DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ name }}_INTERN
    {%- elif arg.strlen %}
        INTEGER, INTENT({{ arg.intent }}) :: {{ name }}_SIZE
        CHARACTER({{ arg.strlen }}), DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ name }}_INTERN
    {%- elif arg.ftype %}
        TYPE({{ arg.ftype }}), DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ name }}_INTERN
    {%- else %}
        {{ arg.type }}
        {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}, DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ name }}_INTERN
    {%- endif %}
{%- endmacro %}


{# Declare a string variable.

   Creates a `TYPE(C_PTR)` variable pointing at a C string buffer and an internal shadow variable of type
   `CHARACTER(...)` that is used in FORTRAN.

   :param arg: A :type VarDecl: node that specifies the variable to be declared.
#}
{% macro declare_arg_str(arg, name) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ name }}
    {%- if arg.strlen != '*' %}
        CHARACTER({{ arg.strlen }}) :: {{ name }}_INTERN
    {%- else %}
        INTEGER{%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ name }}_LENGTH
        CHARACTER({{ name }}_LENGTH), POINTER :: {{ name }}_INTERN
    {%- endif %}
{%- endmacro %}


{# Declare a derived type variable.

   Next to the `TYPE(C_PTR)` variable a pointer variable with the corresponding FORTRAN `TYPE(...)` is created as
   internal shadow variable.

   :param arg: The :type VarDecl: node specifiying the derived type variable.
#}
{% macro declare_arg_dtype(arg, name) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ name }}
        TYPE({{ arg.ftype }}), POINTER :: {{ name }}_INTERN
{%- endmacro %}


{# Declare a basic variable.

   No shadow variable is required.

   :param arg: The :type VarDecl: node specifing the variable.
#}
{% macro declare_arg_basic(arg, name) -%}
        {{ arg.type }}
    {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}
    {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ name }}
{%- endmacro %}


{# Cast an input variable to it internal shadow.

   Depending on the variable's type the appropriate conversion is applied. Afterwards, the `_INTERN` shadow variable can
   be used in FORTRAN.

   :param arg: The :type VarDecl: node that should be casted.
#}
{% macro cast_arg(arg, name=None) -%}
    {%- if arg.dims %}
        {%- if arg.strlen and not arg.intent == 'OUT' %}
        CALL C_F_POINTER({{ name or arg.name }}, {{ name or arg.name }}_INTERN, [{{ name or arg.name }}_SIZE])
        {%- elif not arg.strlen %}
        CALL C_F_POINTER({{ name or arg.name }}, {{ name or arg.name }}_INTERN, [{{ join_dims(arg) }}])
        {%- endif %}
    {%- elif arg.strlen == '*' %}
        CALL C_F_POINTER({{ name or arg.name }}, {{ name or arg.name }}_INTERN)
    {%- elif arg.strlen %}
        CALL C_F_STRING({{ name or arg.name }}, {{ name or arg.name }}_INTERN)
    {%- elif arg.ftype %}
        CALL C_F_POINTER({{ name or arg.name }}, {{ name or arg.name }}_INTERN)
    {%- endif %}
{%- endmacro %}


{# Cast an internal variable to an output variable.

   The appropriate conversion method is apped that the value of the `_INTERN` shadow variable can be returend to C.

   :param arg: A :type VarDecl: node that defines the output variable to be uncasted.
#}
{% macro uncast_arg(arg, name=None) -%}
    {%- if arg.dims %}
        {%- if arg.intent == 'OUT' and arg.dims.0 == '0' %}
        {{ name or arg.name }}_SIZE = SIZE({{ name or arg.name }}_INTERN)
        {%- endif %}
        {{ name or arg.name }} = C_LOC({{ name or arg.name }}_INTERN)
    {%- elif arg.strlen %}
        {%- if arg.intent == 'OUT' %}
        {{ name or arg.name }} = C_MALLOC({{ arg.strlen }}_C_SIZE_T + 1)
        {%- endif %}
        CALL F_C_STRING({{ name or arg.name }}_INTERN, {{ name or arg.name }})
    {%- elif arg.ftype %}
        {{ name or arg.name }} = C_LOC({{ name or arg.name }}_INTERN)
    {%- endif %}
{%- endmacro %}


{# Create a list with array dimensions of a variable.

   A comma-seperated list of the variables dimensions is created. Instead of using the dimensions, an alternate
   character can be supplied that will be joined.

   :param arg: The :type VarDecl: node that contains the dimensions to be joined.
   :param c: An optional alternative character to be used instead of the dimension size.
#}
{% macro join_dims(arg, c=None) -%}
    {%- for dim in arg.dims -%}
        {{ c or dim }}
        {%- if not loop.last %}, {% endif -%}
    {%- endfor -%}
{%- endmacro %}
