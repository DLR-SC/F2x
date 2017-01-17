{#-##################################################################################################################-#}
{#- F2x 'bindc' template variable helpers.                                                                           -#}
{#-                                                                                                                  -#}
{#- This module provides macros to handle FORTRAN variables. This contains variable definitions and casting them     -#}
{#- from and to their C representation.                                                                              -#}
{#-##################################################################################################################-#}


{# Output a comma-seperated list with dummy arguments.

   Join the names of the arguments passed as parameter.

   :param args: A list with :type VarDecl: nodes.
#}
{% macro dummy_args(args) -%}
    {%- for arg in args -%}
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
{% macro declare_arg(arg) -%}
    {%- if arg.dims -%}
        {{ declare_arg_array(arg) }}
    {%- elif arg.strlen -%}
        {{ declare_arg_str(arg) }}
    {%- elif arg.ftype -%}
        {{ declare_arg_dtype(arg) }}
    {%- else -%}
        {{ declare_arg_basic(arg) }}
    {%- endif -%}
{%- endmacro %}


{# Declare an array variable.

   Create a `TYPE(C_PTR)` variable and a pointer variable with the associated FORTRAN type as internal shadow variable.

   :param arg: A :type VarDecl: node specifiying the variable to be declared.
#}
{% macro declare_arg_array(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
    {%- if arg.strlen %}
        CHARACTER({{ arg.strlen }}), DIMENSION({{ join_dims(arg) }}) :: {{ arg.name }}_INTERN
    {%- elif arg.ftype %}
        TYPE({{ arg.ftype }}), DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ arg.name }}_INTERN
    {%- else %}
        {{ arg.type }}
        {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}, DIMENSION({{ join_dims(arg, ':') }}), POINTER :: {{ arg.name }}_INTERN
    {%- endif %}
{%- endmacro %}


{# Declare a string variable.

   Creates a `TYPE(C_PTR)` variable pointing at a C string buffer and an internal shadow variable of type
   `CHARACTER(...)` that is used in FORTRAN.

   :param arg: A :type VarDecl: node that specifies the variable to be declared.
#}
{% macro declare_arg_str(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
        CHARACTER({{ arg.strlen }}) :: {{ arg.name }}_INTERN
{%- endmacro %}


{# Declare a derived type variable.

   Next to the `TYPE(C_PTR)` variable a pointer variable with the corresponding FORTRAN `TYPE(...)` is created as
   internal shadow variable.

   :param arg: The :type VarDecl: node specifiying the derived type variable.
#}
{% macro declare_arg_dtype(arg) -%}
        TYPE(C_PTR){%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
        TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
{%- endmacro %}


{# Declare a basic variable.

   No shadow variable is required.

   :param arg: The :type VarDecl: node specifing the variable.
#}
{% macro declare_arg_basic(arg) -%}
        {{ arg.type }}
    {%- if arg.kind %}(KIND={{ arg.kind }}){% endif %}
    {%- if arg.intent %}, INTENT({{ arg.intent }}){% endif %} :: {{ arg.name }}
{%- endmacro %}


{# Cast an input variable to it internal shadow.

   Depending on the variable's type the appropriate conversion is applied. Afterwards, the `_INTERN` shadow variable can
   be used in FORTRAN.

   :param arg: The :type VarDecl: node that should be casted.
#}
{% macro cast_arg(arg) -%}
    {%- if arg.dims %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN, [{{ join_dims(arg) }}])
    {%- elif arg.strlen %}
        CALL C_F_STRING({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- elif arg.ftype %}
        CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
    {%- endif %}
{%- endmacro %}


{# Cast an internal variable to an output variable.

   The appropriate conversion method is apped that the value of the `_INTERN` shadow variable can be returend to C.

   :param arg: A :type VarDecl: node that defines the output variable to be uncasted.
#}
{% macro uncast_arg(arg) -%}
    {%- if arg.dims %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
    {%- elif arg.strlen %}
        {%- if arg.intent == 'OUT' %}
        {{ arg.name }} = C_MALLOC({{ arg.strlen }}_8 + 1)
        {%- endif %}
        CALL F_C_STRING({{ arg.name }}_INTERN, {{ arg.name }})
    {%- elif arg.ftype %}
        {{ arg.name }} = C_LOC({{ arg.name }}_INTERN)
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
