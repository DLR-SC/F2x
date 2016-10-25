{#####################################################################################################################}
{# F2x-Template library: _bindc_types.f90.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bindc_f90.t' template for derived type handling.                       #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type constructor: Allocates a POINTER of the derived type and returns it as C pointer.                            #}
{% macro constructor(type) -%}
    FUNCTION {{ type.name }}_NEW() BIND(C, name="{{ type.name }}_new")
        TYPE(C_PTR) :: {{ type.name }}_NEW
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        
        ALLOCATE({{ type.name }}_INTERN)
        {{ type.name }}_NEW = C_LOC({{ type.name }}_INTERN)
    END FUNCTION
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type destructor: Takes a C pointer reference to a POINTER of the derived type and deallocates if needed.          #}
{% macro destructor(type) -%}
    SUBROUTINE {{ type.name }}_FREE({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        
        IF (C_ASSOCIATED({{ type.name }}_PTR)) THEN
            CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        END IF
        
        IF (ASSOCIATED({{ type.name }}_INTERN)) THEN
            DEALLOCATE({{ type.name }}_INTERN)
        END IF
    END SUBROUTINE
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field allocator: Resizes allocatable array fields to the given size.                                         #}
{% macro allocator(type, field) -%}
    {%- if field.dims and field.dynamic == 'ALLOCATABLE' %}
    SUBROUTINE {{ type.name }}_INIT_{{ field.name }}({{ type.name }}_PTR, SIZE) BIND(C, name="{{ type.name }}_init_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        INTEGER, VALUE :: SIZE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        IF (.NOT. ALLOCATED({{ type.name }}_INTERN%{{ field.name }})) ALLOCATE({{ type.name }}_INTERN%{{ field.name }}(SIZE))
    END SUBROUTINE
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field getter: Takes a C pointer reference to a POINTER of the derived type and returns a fields value.       #}
{% macro getter(type, field) -%}
    {%- if field.dims %}
    FUNCTION {{ type.name }}_GET_{{ field.name }}_ITEM({{ type.name }}_PTR, INDEX) BIND(C, name="{{ type.name }}_get_{{ field.name }}_item")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        INTEGER, VALUE :: INDEX
        {{ field.type }} :: {{ type.name }}_GET_{{ field.name }}_ITEM
        {%- if field.ftype %}
        TYPE({{ field.ftype }}), POINTER :: {{ field.name }}_INTERN
        {%- endif %}

        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.ftype %}
        {{ field.name }}_INTERN => {{ type.name }}_INTERN%{{ field.name }}(INDEX)
        {{ type.name }}_GET_{{ field.name }}_ITEM = C_LOC({{ field.name }}_INTERN)
        {%- else %}
        {{ type.name }}_GET_{{ field.name }}_ITEM = {{ type.name }}_INTERN%{{ field.name }}(INDEX)
        {%- endif %}
    END FUNCTION
    {%- elif field.getter == 'function' %}
    FUNCTION {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        {{ field.type }} :: {{ type.name }}_GET_{{ field.name }}
        {%- if field.ftype %}
        TYPE({{ field.ftype }}), POINTER :: {{ field.name }}_INTERN
        {%- endif %}
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.ftype %}
        {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (.NOT. ALLOCATED({{ type.name }}_INTERN%{{ field.name }})) ALLOCATE({{ type.name }}_INTERN%{{ field.name }})
        {%- elif field.dynamic == 'POINTER' %}
        IF (.NOT. ASSOCIATED({{ type.name }}_INTERN%{{ field.name }})) ALLOCATE({{ type.name }}_INTERN%{{ field.name }})
        {%- endif %}
        {{ field.name }}_INTERN => {{ type.name }}_INTERN%{{ field.name }}
        {{ type.name }}_GET_{{ field.name }} = C_LOC({{ field.name }}_INTERN)
        {%- else %}
        {{ type.name }}_GET_{{ field.name }} = {{ type.name }}_INTERN%{{ field.name }}
        {%- endif %}
    END FUNCTION
    {%- elif field.getter == 'subroutine' %}
    SUBROUTINE {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_VALUE) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        {{ field.type }}, INTENT(OUT) :: {{ field.name }}_VALUE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {{ field.name }}_VALUE = {{ type.name }}_INTERN%{{ field.name }}
    END SUBROUTINE
    {%- else %}
    ! No read access to {{ type.name }}%{{ field.name }}
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field setter: Takes a C pointer reference to a POINTER of the derived type and and a value to be set.        #}
{% macro setter(type, field) -%}
    {%- if field.dims %}
    ! No write access to arrays (yet)
    {%- elif field.setter == 'subroutine' %}
    SUBROUTINE {{ type.name }}_SET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_VALUE) BIND(C, name="{{ type.name }}_set_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN
        {{ field.type }}, INTENT(IN), VALUE :: {{ field.name }}_VALUE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {{ type.name }}_INTERN%{{ field.name }} = {{ field.name }}_VALUE
    END SUBROUTINE
    {%- else %}
    ! No write access to {{ type.name }}%{{ field.name }}
    {%- endif %}
{%- endmacro %}
