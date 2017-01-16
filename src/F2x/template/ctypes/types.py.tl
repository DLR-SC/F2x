{#-##################################################################################################################-#}
{#- F2x 'ctypes' template call library.                                                                              -#}
{#-                                                                                                                  -#}
{#- This library provides macros to generate Python methods that handle parameter conversion and calling of ctypes   -#}
{#- function as exported by the 'bindc' template.                                                                    -#}
{#-##################################################################################################################-#}


{# Generate a derived type wrapper.
#}
{% macro export_type(type) -%}
class {{ type.name }}(FType):
    _new = constructor(library.{{ type.name }}_new)
    _free = destructor(library.{{ type.name }}_free)

    {%- for field in type.fields %}
        {{ export_field(type, field) }}

    {%- endfor %}
{%- endmacro %}


{% macro export_field(type, field) -%}
    {%- if field.dims %}
    {{ field.name }} = ArrayField(
        "{{ field.name }}",
        {{ field.ftype or field.pytype }},
        [{{ join_dims(field.dims) }}],
        library.{{ type.name }}_get_{{ field.name }}
        {%- if field.dynamic %},
        library.{{ type.name }}_alloc_{{ field.name }}
        {%- endif %}
        {%- if field.strlen %},
        strlen={{ field.strlen }}
        {%- endif %}
    )
    {%- else %}
    {{ field.name }} = Field(
        {{ field.ftype or field.pytype }},
        library.{{ type.name }}_get_{{ field.name }},
        {%- if field.setter %}
        library.{{ type.name }}_set_{{ field.name }}
        {%- else %}
        None
        {%- endif %}
        {%- if field.dynamic %},
        library.{{ type.name }}_alloc_{{ field.name }}
        {%- endif %}
    )
    {%- endif %}
{%- endmacro %}


{% macro join_dims(dims) -%}
    {% for dim in dims %}{{ dim }}{% if not loop.last %}, {% endif %}{% endfor %}
{%- endmacro %}
