{#-##################################################################################################################-#}
{#- F2x 'bindc' template type library.                                                                               -#}
{#-                                                                                                                  -#}
{#- This library contains macros to export FORTRAN derived types. It creates constructors, destructors and accessors -#}
{#- with `BIND(C)` interface.                                                                                        -#}
{#-                                                                                                                  -#}
{#- For applied naming conventions, please refer to :ref templates/bindc/naming.adoc:.                               -#}
{#-##################################################################################################################-#}


{# Import helper library. #}
{% import "vars.f90.tl" as vars %}


{# Export a derived type.

   Create constructor, destructor and accessors for each field.

   :param type: A :type TypeDef: node that describes the type to be exported.
#}
{% macro export_type(type) -%}
    !===================================================================================================================
    ! TYPE({{ type.name }})
    {{ constructor(type) }}

    {{ destructor(type) }}

    {%- for field in type.fields %}
    {{ export_field(type, field) }}
    {%- endfor %}
{%- endmacro %}


{# Generate a constructor.

   This creates a `FUNCTION` that returns a new instance of the type and as a `TYPE(C_PTR)` that references the
   allocated memory.

   :param type: The :type TypeDef: node the constructor should be generated for.
#}
{% macro constructor(type) -%}
    FUNCTION {{ type.name }}_NEW() RESULT(PTR) BIND(C, name="{{ type.name }}_new")
        TYPE(C_PTR) :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE

        ALLOCATE(INSTANCE)
        PTR = C_LOC(INSTANCE)
    END FUNCTION
{%- endmacro %}


{# Generate a destructor.

   The generates a `SUBROUTINE` that releases the memory that is referenced by the `TYPE(C)` pointer passed as only
   argument. It checks for pointer validity to prevent double-frees and sets the pointer to NULL.

   :param type: The :type TypeDef: node of the type that should be freed.
#}
{%  macro destructor(type) -%}
    SUBROUTINE {{ type.name }}_FREE(PTR) BIND(C, name="{{ type.name }}_free")
        TYPE(C_PTR), INTENT(INOUT) :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE

        IF (.NOT. C_ASSOCIATED(PTR)) RETURN
        CALL C_F_POINTER(PTR, INSTANCE)
        IF (ASSOCIATED(INSTANCE)) DEALLOCATE(INSTANCE)
        PTR = C_NULL_PTR
    END SUBROUTINE
{%- endmacro %}


{# Generate accessors for a type field.

   This generates a getter and if applicable a setter for the field. For fields that are `ALLOCATABLE` or `POINTER`,
   an allocator is provided, too.

   :param type: The :type TypeDef: node of the derived type containing the field.
   :param field: A :type VarDecl: node that specifies the field.
#}
{% macro export_field(type, field) -%}
    ! {{ type.name }}%{{ field.name }}
    {%- if field.dynamic %}
    {{ allocator(type, field) }}
    {%- endif %}
    {{ getter(type, field) }}
    {%- if field.setter == 'subroutine' %}

    {{ setter(type, field) }}
    {%- endif %}
{%- endmacro %}


{# Generate a field allocator.

   The generated `SUBROUTINE` allocates dynamic fields. It accepts a `TYPE(C_PTR)` that references the containing
   instance and for array fields an array with the sizes to be allocated for each dimension. Basic precautions are met
   to avoid accessing null pointers or do double allocations.

   :param type: The :type TypeDef: node for the type containing the field.
   :param field: A :type VarDecl: node that specifies the dynamic field.

   :todo: Maybe split this macro.
#}
{% macro allocator(type, field) -%}
    SUBROUTINE {{ type.name }}_ALLOC_{{ field.name }}(PTR{% if field.dims %}, SIZES{% endif %}) BIND(C, name="{{ type.name }}_alloc_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {%- if field.dims %}
        TYPE(C_PTR), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:), POINTER :: SIZE_ARRAY
    {%- endif %}

        IF (.NOT. C_ASSOCIATED(PTR){% if field.dims %} .OR. .NOT. C_ASSOCIATED(SIZES){% endif %}) RETURN
        CALL C_F_POINTER(PTR, INSTANCE)
    {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (ALLOCATED(INSTANCE%{{ field.name }})) RETURN
    {%- elif field.dynamic == 'POINTER' %}
        IF (ASSOCIATED(INSTANCE%{{ field.name }})) RETURN
    {%- endif %}
    {%- if field.dims %}
        CALL C_F_POINTER(SIZES, SIZE_ARRAY, [{{ field.dims|length }}])
        ALLOCATE(INSTANCE%{{ field.name }}(
        {%- for _ in field.dims -%}
            SIZE_ARRAY({{ loop.index }}){% if not loop.last %}, {% endif %}
        {%- endfor -%}
        ))
    {%- else %}
        ALLOCATE(INSTANCE%{{ field.name }})
    {%- endif %}
    END SUBROUTINE
{%- endmacro %}


{# Generate a field getter.

   Generate a `FUNCTION` or `SUBROUTINE` to access a derived types field value from C. Depending on the field type, the
   appropriate generator macro is instantiated.

   :param type: The :type TypeDef: node of the type containing the field.
   :param field: A :type VarDecl: node that specifies the field that should be accessed.
#}
{% macro getter(type, field) -%}
    {%- if field.dims %}
    {{ getter_array(type, field) }}
    {%- elif field.getter == 'function' %}
    {{ getter_func(type, field) }}
    {%- else %}
    {{ getter_sub(type, field) }}
    {%- endif %}
{%- endmacro %}


{# Generate a getter for an array field.

   A `SUBROUTINE` accepting a `TYPE(C_PTR)` referencing the type instance to access. An output argument of `TYPE(C_PTR)`
   is used to return the address of the array.

   :param type: The :type TypeDef: node of the type containing the field.
   :param field: A :type VarDecl: node that specifies the array field.
#}
{% macro getter_array(type, field) -%}
    SUBROUTINE {{ type.name }}_GET_{{ field.name }}(PTR, {{ field.name }}) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
        {{ vars.declare_arg(field.with_intent("OUT")) }}

        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
        {{ field.name }}_INTERN => INSTANCE%{{ field.name }}
        {{ vars.uncast_arg(field) }}
    END SUBROUTINE
{%- endmacro %}


{# Generate a field getter `FUNCTION`.

   The `FUNCTION` takes an `TYPE(C_PTR)` that references the type instance. The field value is returned.

   :param type: The :type TypeDef: node that contains the field.
   :param field: A :type VarDecl: node specifying the field.
#}
{% macro getter_func(type, field) -%}
    FUNCTION {{ type.name }}_GET_{{ field.name }}(PTR) RESULT({{ field.name }}) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
        {{ vars.declare_arg(field) }}

        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
        {%- if field.ftype %}
        {{ field.name }}_INTERN => INSTANCE%{{ field.name }}
        {{ field.name }} = C_LOC({{ field.name }}_INTERN)
        {%- else %}
        {{ field.name }} = INSTANCE%{{ field.name }}
        {%- endif %}
    END FUNCTION
{%- endmacro %}


{# Generate a field getter `SUBROUTINE`.

   This is used to return field value using an additional output argument. The first argument is a `TYPE(C_PTR)` that
   references the type instance.

   :param type: The :type TypeDef: node that contains the field.
   :param field: A :type VarDecl: that specifies the field.
#}
{% macro getter_sub(type, field) -%}
    SUBROUTINE {{ type.name }}_GET_{{ field.name }}(PTR, {{ field.name }}) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
        {{ vars.declare_arg(field.with_intent("OUT")) }}

        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
        {%- if field.strlen %}
        {{ field.name }} = C_MALLOC({{ field.strlen }}_C_SIZE_T + 1)
        CALL F_C_STRING(INSTANCE%{{ field.name }}, {{ field.name }})
        {%- endif %}
    END SUBROUTINE
{%- endmacro %}


{# Generate a field setter.

   A `SUBROUTINE` is generated that takes a `TYPE(C_PTR)` referencing the type instance and the new value.

   :param type: The :type TypeDef: node containing the field.
   :param field: A :type VarDecl: node that specifies the settable field.
#}
{% macro setter(type, field) -%}
    SUBROUTINE {{ type.name }}_SET_{{ field.name }}(PTR, {{ field.name }}) BIND(C, name="{{ type.name }}_set_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
        {{ vars.declare_arg(field.with_intent('IN')) }}

        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
        {%- if field.strlen %}
        CALL C_F_STRING({{ field.name }}, INSTANCE%{{ field.name }})
        {%- else %}
        INSTANCE%{{ field.name }} = {{ field.name }}
        {%- endif %}
    END SUBROUTINE
{%- endmacro %}
