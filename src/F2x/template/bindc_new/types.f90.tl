{#-##################################################################################################################-#}
{#- F2x 'bindc' template type library.                                                                               -#}
{#-                                                                                                                  -#}
{#- This library contains macros to export FORTRAN derived types. It creates constructors, destructors and accessors -#}
{#- with `BIND(C)` interface.                                                                                        -#}
{#-                                                                                                                  -#}
{#- For applied naming conventions, please refer to :ref templates/bindc/naming.adoc:.                               -#}
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
{% import "types_vars.f90.tl" as vars %}


{# Export a derived type.

   Create constructor, destructor and accessors for each field.

   :param type: A :type TypeDef: node that describes the type to be exported.
#}
{% macro export(type) -%}
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
    FUNCTION {{ type.name|upper }}_NEW() RESULT(INSTANCE_PTR) BIND(C, name="{{ module.name|lower }}_{{ type.name|lower }}_new")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {{ type.name }}_new
    {%- endif %}
        TYPE(C_PTR) :: INSTANCE_PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
        INTEGER :: STATUS

        ALLOCATE(INSTANCE, STAT=STATUS)
        IF (STATUS == 0) THEN
            INSTANCE_PTR = C_LOC(INSTANCE)
        ELSE
            CALL F2X_SET_ERROR(STATUS)
            INSTANCE_PTR = C_NULL_PTR
        END IF
    END FUNCTION
{% endmacro %}


{# Generate a destructor.

   The generates a `SUBROUTINE` that releases the memory that is referenced by the `TYPE(C)` pointer passed as only
   argument. It checks for pointer validity to prevent double-frees and sets the pointer to NULL.

   :param type: The :type TypeDef: node of the type that should be freed.
#}
{%  macro destructor(type) -%}
    SUBROUTINE {{ type.name|upper }}_FREE(INSTANCE_PTR) BIND(C, name="{{ module.name|lower }}_{{ type.name|lower }}_free")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {{ type.name }}_free
    {%- endif %}
        TYPE(C_PTR), INTENT(INOUT) :: INSTANCE_PTR
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
    {%- if type %}
    ! {{ type.name }}%{{ field.name }}
    {%- else %}
    ! GLOBAL {{ field.name }}
    {%- endif %}
    {%- if field.dynamic or (type == None and field.dims) %}
    {{ allocator(type, field) }}
    {%- endif %}
    {{ getter(type, field) }}
    {%- if field.setter == 'subroutine' %}
        {%- if field.dims and field.strlen %}
    {{ setter_strarray(type, field) }}
        {%- else %}
    {{ setter(type, field) }}
        {%- endif %}
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
    SUBROUTINE {% if type %}{{ type.name }}_{% endif %}ALLOC_{{ field.name }}(
    {%- if type %}PTR{% if field.dims %}, {% endif %}{% endif %}
    {%- if field.dims %}SIZES{% endif %}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}alloc_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}ALLOC_{{ field.name }}
    {%- endif %}
    {%- if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {%- endif %}
    {%- if field.dims %}
        TYPE(C_PTR), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:), POINTER :: SIZE_ARRAY
    {%- endif %}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR){% if field.dims %} .OR. .NOT. C_ASSOCIATED(SIZES){% endif %}) RETURN
        CALL C_F_POINTER(PTR, INSTANCE)
    {%- endif %}
    {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (ALLOCATED({% if type %}INSTANCE%{% endif %}{{ field.name }})) RETURN
    {%- elif field.dynamic == 'POINTER' %}
        IF (ASSOCIATED({% if type %}INSTANCE%{% endif %}{{ field.name }})) RETURN
    {%- endif %}
    {%- if field.dims %}
        CALL C_F_POINTER(SIZES, SIZE_ARRAY, [{{ field.dims|length }}])
        ALLOCATE({% if type %}INSTANCE%{% endif %}{{ field.name }}(
        {%- for _ in field.dims -%}
            SIZE_ARRAY({{ loop.index }}){% if not loop.last %}, {% endif %}
        {%- endfor -%}
        ))
    {%- else %}
        ALLOCATE({% if type %}INSTANCE%{% endif %}{{ field.name }})
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
        {%- if field.ftype %}
    {{ getter_farray(type, field) }}
        {%- elif field.strlen %}
    {{ getter_strarray(type, field) }}
        {%- else %}
    {{ getter_array(type, field) }}
        {%- endif %}
    {%- elif field.getter == 'function' %}
    {{ getter_func(type, field) }}
    {%- else %}
    {{ getter_sub(type, field) }}
    {%- endif %}
{%- endmacro %}


{# Generate a getter for a derived type array field.
#}
{% macro getter_farray(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    FUNCTION {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}({% if type %}PTR, {% endif %}INDICES) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        TYPE(C_PTR), INTENT(IN) :: INDICES
        INTEGER, DIMENSION(:), POINTER :: INDEX_ARRAY
        TYPE({{ field.ftype }}), POINTER :: {{ name }}_INTERN
        TYPE(C_PTR) :: {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        CALL C_F_POINTER(INDICES, INDEX_ARRAY, [{{ field.dims|length }}])
        {{ name }}_INTERN => {% if type %}INSTANCE%{% endif %}{{ field.name }}(
        {%- for _ in field.dims -%}
            INDEX_ARRAY({{ loop.index }}) + 1{% if not loop.last %}, {% endif %}
        {%- endfor -%})
        {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }} = C_LOC({{ name }}_INTERN)
    END FUNCTION
{%- endmacro %}


{% macro getter_strarray(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    SUBROUTINE {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}(PTR, INDICES, {{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        TYPE(C_PTR), INTENT(IN) :: INDICES
        INTEGER, DIMENSION(:), POINTER :: INDEX_ARRAY
        CHARACTER({{ field.strlen }}), POINTER :: {{ name }}_INTERN
        TYPE(C_PTR), INTENT(OUT) :: {{ name }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        CALL C_F_POINTER(INDICES, INDEX_ARRAY, [{{ field.dims|length }}])
        {{ name }}_INTERN => {% if type %}INSTANCE%{% endif %}{{ field.name }}(
        {%- for _ in field.dims -%}
            INDEX_ARRAY({{ loop.index }}) + 1{% if not loop.last %}, {% endif %}
        {%- endfor -%})
        {{ name }} = C_MALLOC({{ field.strlen }}_C_SIZE_T + 1)
        CALL F_C_STRING({{ name }}_INTERN, {{ name }})
    END SUBROUTINE
{%- endmacro %}


{# Generate a getter for an array field.

   A `SUBROUTINE` accepting a `TYPE(C_PTR)` referencing the type instance to access. An output argument of `TYPE(C_PTR)`
   is used to return the address of the array.

   :param type: The :type TypeDef: node of the type containing the field.
   :param field: A :type VarDecl: node that specifies the array field.
#}
{% macro getter_array(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    SUBROUTINE {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}({% if type %}PTR, {% endif %}{% if field.dims.0 == 0 %}{{ name }}_SIZE, {% endif %}{{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        {{ vars.declare_arg(field.with_intent("OUT"), name) }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        {{ name }}_INTERN => {% if type %}INSTANCE%{% endif %}{{ field.name }}
        {{ vars.uncast_arg(field) }}
    END SUBROUTINE
{%- endmacro %}


{# Generate a field getter `FUNCTION`.

   The `FUNCTION` takes an `TYPE(C_PTR)` that references the type instance. The field value is returned.

   :param type: The :type TypeDef: node that contains the field.
   :param field: A :type VarDecl: node specifying the field.
#}
{% macro getter_func(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    FUNCTION {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}({% if type %}PTR{% endif %}) RESULT({{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        {{ vars.declare_arg(field.with_intent(None), name) }}


    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        {%- if field.ftype %}
        {{ name }}_INTERN => {% if type %}INSTANCE%{% endif %}{{ field.name }}
        {{ name }} = C_LOC({{ field.name }}_INTERN)
        {%- else %}
        {{ name }} = {% if type %}INSTANCE%{% endif %}{{ field.name }}
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
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    SUBROUTINE {% if type %}{{ type.name }}_{% endif %}GET_{{ field.name }}({% if type %}PTR, {% endif %}{{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}get_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        {{ vars.declare_arg(field.with_intent("OUT"), name) }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        {%- if field.strlen %}
        {{ name }} = C_MALLOC({{ field.strlen }}_C_SIZE_T + 1)
        CALL F_C_STRING({% if type %}INSTANCE%{% endif %}{{ field.name }}, {{ name }})
        {%- endif %}
    END SUBROUTINE
{%- endmacro %}


{# Generate a field setter.

   A `SUBROUTINE` is generated that takes a `TYPE(C_PTR)` referencing the type instance and the new value.

   :param type: The :type TypeDef: node containing the field.
   :param field: A :type VarDecl: node that specifies the settable field.
#}
{% macro setter(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    SUBROUTINE {% if type %}{{ type.name }}_{%  endif %}SET_{{ field.name }}({% if type %}PTR, {% endif %}{{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}set_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}set_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        {{ vars.declare_arg(field.with_intent('IN'), name) }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        {%- if field.strlen %}
        CALL C_F_STRING({{ name }}, {% if type %}INSTANCE%{% endif %}{{ field.name }})
        {%- else %}
        {% if type %}INSTANCE%{% endif %}{{ field.name }} = {{ name }}
        {%- endif %}
    END SUBROUTINE
{%- endmacro %}


{% macro setter_strarray(type, field) -%}
{% if type %}{% set  name  = field.name %}{% else %}{% set name = "VALUE" %}{% endif %}
    SUBROUTINE {% if type %}{{ type.name }}_{% endif %}SET_{{ field.name }}(PTR, INDICES, {{ name }}) BIND(C, name="{% if type %}{{ type.name }}_{% endif %}set_{{ field.name }}")
    {%- if ifort_dll %}
!DEC$ ATTRIBUTES DLLEXPORT :: {% if type %}{{ type.name }}_{% endif %}set_{{ field.name }}
    {%- endif %}
    {% if type %}
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE
    {% endif %}
        TYPE(C_PTR), INTENT(IN) :: INDICES
        INTEGER, DIMENSION(:), POINTER :: INDEX_ARRAY
        CHARACTER({{ field.strlen }}) :: {{ name }}_INTERN
        TYPE(C_PTR), INTENT(IN) :: {{ name }}

    {% if type %}
        IF (.NOT. C_ASSOCIATED(PTR)) THEN
            WRITE (*,*) "Cannot access {{ field.name }} from null pointer."
            RETURN
        END IF
        CALL C_F_POINTER(PTR, INSTANCE)
    {% endif %}
        CALL C_F_POINTER(INDICES, INDEX_ARRAY, [{{ field.dims|length }}])
        CALL C_F_STRING({{ name }}, {{ name }}_INTERN)
        {% if type %}INSTANCE%{% endif %}{{ field.name }}(
        {%- for _ in field.dims -%}
            INDEX_ARRAY({{ loop.index }}) + 1{% if not loop.last %}, {% endif %}
        {%- endfor -%}) = {{ name }}_INTERN
    END SUBROUTINE
{%- endmacro %}

