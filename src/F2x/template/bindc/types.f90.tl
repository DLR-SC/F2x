{% import "vars.f90.tl" as vars %}

{% macro export_type(module, type) -%}
    !===================================================================================================================
    ! TYPE({{ type.name }})
    {{ constructor(type) }}

    {{ destructor(type) }}

    {%- for field in type.fields %}
    {{ export_field(type, field) }}
    {%- endfor %}
{%- endmacro %}

{% macro constructor(type) -%}
    FUNCTION {{ type.name }}_NEW() RESULT(PTR) BIND(C, name="{{ type.name }}_new")
        TYPE(C_PTR) :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE

        ALLOCATE(INSTANCE)
        PTR = C_LOC(INSTANCE)
    END FUNCTION
{%- endmacro %}

{%  macro destructor(type) -%}
    SUBROUTINE {{ type.name }}_FREE(PTR) BIND(C, name="{{ type.name }}_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        TYPE({{ type.name }}), POINTER :: INSTANCE

        IF (.NOT. C_ASSOCIATED(PTR)) RETURN
        CALL C_F_POINTER(PTR, INSTANCE)
        IF (ASSOCIATED(INSTANCE)) DEALLOCATE(INSTANCE)
    END SUBROUTINE
{%- endmacro %}

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

{% macro getter(type, field) -%}
    {%- if field.dims %}
    {{ getter_array(type, field) }}
    {%- elif field.getter == 'function' %}
    {{ getter_func(type, field) }}
    {%- else %}
    {{ getter_sub(type, field) }}
    {%- endif %}
{%- endmacro %}

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
        {{ field.name }} = C_MALLOC({{ field.strlen }}_8 + 1)
        CALL F_C_STRING(INSTANCE%{{ field.name }}, {{ field.name }})
        {%- endif %}
    END SUBROUTINE
{%- endmacro %}

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
