{#####################################################################################################################}
{# F2x-Template library: _bindc_types.f90.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bindc.f90.t' template for derived type handling.                       #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type constructor: Allocates a POINTER of the derived type and returns it as C pointer.                            #}
{% macro constructor(type) -%}
    FUNCTION {{ type.name }}_NEW() BIND(C, name="{{ type.name }}_new")
        TYPE(C_PTR) :: {{ type.name }}_NEW
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        
        ALLOCATE({{ type.name }}_INTERN)
        {{ type.name }}_NEW = C_LOC({{ type.name }}_INTERN)
    END FUNCTION
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type destructor: Takes a C pointer reference to a POINTER of the derived type and deallocates if needed.          #}
{% macro destructor(type) -%}
    SUBROUTINE {{ type.name }}_FREE({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        
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
    {%- if field.dims and field.dynamic %}
    SUBROUTINE {{ type.name }}_{{ field.name }}_INIT({{ type.name }}_PTR, {{ field.name }}_SIZE) BIND(C, name="{{ type.name }}_{{ field.name }}_init")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        INTEGER, VALUE :: {{ field.name }}_SIZE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (.NOT. ALLOCATED({{ type.name }}_INTERN%{{ field.name }})) ALLOCATE({{ type.name }}_INTERN%{{ field.name }}({{ field.name }}_SIZE))
        {%- elif field.dynamic == 'POINTER' %}
        IF (.NOT. ASSOCIATED({{ type.name }}_INTERN%{{ field.name }})) ALLOCATE({{ type.name }}_INTERN%{{ field.name }}({{ field.name }}_SIZE))
        {%- endif %}
    END SUBROUTINE

    SUBROUTINE {{ type.name }}_{{ field.name }}_CLEAR({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_{{ field.name }}_clear")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()

        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (ALLOCATED({{ type.name }}_INTERN%{{ field.name }})) DEALLOCATE({{ type.name }}_INTERN%{{ field.name }})
        {%- elif field.dynamic == 'POINTER' %}
        IF (ASSOCIATED({{ type.name }}_INTERN%{{ field.name }})) DEALLOCATE({{ type.name }}_INTERN%{{ field.name }})
        {%- endif %}
    END SUBROUTINE

    FUNCTION {{ type.name }}_{{ field.name }}_SIZE({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_{{ field.name }}_size")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        INTEGER :: {{ type.name }}_{{ field.name }}_SIZE

        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.dynamic == 'ALLOCATABLE' %}
        IF (ALLOCATED({{ type.name }}_INTERN%{{ field.name }})) THEN
        {%- elif field.dynamic == 'POINTER' %}
        IF (ASSOCIATED({{ type.name }}_INTERN%{{ field.name }})) THEN
        {%- endif %}
            {{ type.name }}_{{ field.name }}_SIZE = SIZE({{ type.name }}_INTERN%{{ field.name }})
        ELSE
            {{ type.name }}_{{ field.name }}_SIZE = 0
        END IF
    END FUNCTION
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field getter: Takes a C pointer reference to a POINTER of the derived type and returns a fields value.       #}
{% macro getter(type, field) -%}
    {#- Arrays need special treatment... #}
    {%- if field.dims %}

    {%- if field.ftype %}
    {#- ... those of derived types should be allocated by Fortran and accessed item-wise ... #}
    FUNCTION {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_INDEX) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        INTEGER, VALUE :: {{ field.name }}_INDEX
        {{ field.type }}{% if field.kind %}(KIND={{ field.kind }}){% endif %} :: {{ type.name }}_{{ field.name }}_ITEM
        {%- if field.ftype %}
        TYPE({{ field.ftype }}), POINTER :: {{ field.name }}_INTERN => NULL()
        {%- endif %}

        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {%- if field.ftype %}
        {{ field.name }}_INTERN => {{ type.name }}_INTERN%{{ field.name }}({{ field.name }}_INDEX)
        {{ type.name }}_{{ field.name }}_ITEM = C_LOC({{ field.name }}_INTERN)
        {%- else %}
        {{ type.name }}_{{ field.name }}_ITEM = {{ type.name }}_INTERN%{{ field.name }}({{ field.name }}_INDEX)
        {%- endif %}
    END FUNCTION
    
    {%- else %}
    {#- ... all other are returned as native arrays. #}
    FUNCTION {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        TYPE(C_PTR) :: {{ type.name }}_GET_{{ field.name }}
        {{ field.type }}{% if field.kind %}(KIND={{ field.kind }}){% endif %}, POINTER :: {{ field.name }}_INTERN({% for dim in field.dims %}:{% if not loop.last %}, {% endif %}{% endfor %}) => NULL()
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {{ field.name }}_INTERN => {{ type.name }}_INTERN%{{ field.name }}
        {{ type.name }}_GET_{{ field.name }} = C_LOC({{ field.name }}_INTERN)
    END FUNCTION
    {%- endif %}

    {#- Subroutine as getter only allowed for strings (so far). #}
    {%- elif field.getter == 'subroutine' and field.strlen %}
    SUBROUTINE {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_VALUE) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ field.name }}_VALUE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        CALL F_C_STRING({{ type.name }}_INTERN%{{ field.name }}, {{ field.name }}_VALUE, {{ field.strlen }})
    END SUBROUTINE

	{#- Other types are (more ore less) straight forward. #}
    {%- elif field.getter == 'function' %}
    FUNCTION {{ type.name }}_GET_{{ field.name }}({{ type.name }}_PTR) BIND(C, name="{{ type.name }}_get_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        {{ field.type }}{% if field.kind %}(KIND={{ field.kind }}){% endif %} :: {{ type.name }}_GET_{{ field.name }}
        {%- if field.ftype %}
        TYPE({{ field.ftype }}), POINTER :: {{ field.name }}_INTERN => NULL()
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

    {%- else %}
    ! No read access to {{ type.name }}%{{ field.name }}
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field setter: Takes a C pointer reference to a POINTER of the derived type and and a value to be set.        #}
{% macro setter(type, field) -%}
    {#- Arrays cannot be set, they should be accessed using the C pointer. #}
    {%- if field.dims %}
    ! No write access to array {{ type.name }}%{{ field.name }}.
    
    {#- String need special treatment. #}
    {%- elif field.strlen %}
    SUBROUTINE {{ type.name }}_SET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_VALUE) BIND(C, name="{{ type.name }}_set_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ field.name }}_VALUE
    
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        CALL C_F_STRING({{ field.name }}_VALUE, {{ type.name }}_INTERN%{{ field.name }})
    END SUBROUTINE
    
    {#- The rest is straight forward. #}
    {%- elif field.setter == 'subroutine' %}
    SUBROUTINE {{ type.name }}_SET_{{ field.name }}({{ type.name }}_PTR, {{ field.name }}_VALUE) BIND(C, name="{{ type.name }}_set_{{ field.name }}")
        TYPE(C_PTR), INTENT(IN), VALUE :: {{ type.name }}_PTR
        TYPE({{ type.name }}), POINTER :: {{ type.name }}_INTERN => NULL()
        {{ field.type }}{% if field.kind %}(KIND={{ field.kind }}){% endif %}, INTENT(IN), VALUE :: {{ field.name }}_VALUE
        
        CALL C_F_POINTER({{ type.name }}_PTR, {{ type.name }}_INTERN)
        {{ type.name }}_INTERN%{{ field.name }} = {{ field.name }}_VALUE
    END SUBROUTINE
    
    {%- else %}
    ! No write access to {{ type.name }}%{{ field.name }}.
    {%- endif %}
{%- endmacro %}
