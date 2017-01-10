{#-##################################################################################################################-#}
{# F2x template to generate FORTRAN BIND(C) wrapper around exported modules.                                          #}
{#-##################################################################################################################-#}
{%- import "calls.f90.tl" as calls -%}
{%- import "types.f90.tl" as types %}

MODULE {{ module.name }}_WRAP
    USE C_INTERFACE_MODULE
    USE {{ module.name }}

    IMPLICIT NONE
    PRIVATE

CONTAINS
{%- for type in module.types  %}
    {{ types.export_type(module, type) }}
{% endfor %}

{%- if module.methods %}
    !===================================================================================================================
    ! Exported subroutines and functions
    {%- for method in module.methods %}

    {{ calls.export_method(module, method) }}
    {% endfor %}
{%- endif %}
END
