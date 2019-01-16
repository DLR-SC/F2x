{%- macro export() -%}
    {%- for type in module.types %}

    ! TYPE({{ type.ftype }}) }}
    {%- endfor %}
{%- endmacro -%}
