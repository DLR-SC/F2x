{%- macro export() -%}
    {%- for type in module.types %}

class {{ type.name }}(FType):
        pass
    {%- endfor %}
{%- endmacro -%}
