{% macro export_type(module, type) -%}
class {{ type.name }}(FType):
    _new = constructor({{ module.name }}.{{ type.name }}_new)
    _free = destructor({{ module.name }}.{{ type.name }}_free)

    {%- for field in type.fields %}
        {%- if field.dims %}
    {{ field.name }}_dims = [{{ join_dims(field.dims) }}]
        {%- endif %}
    {{ field.name }} = Field(
        {%- if field.dims %}
        getter({{ field.pytype }}, {{ module.name }}.{{ type.name }}_get_{{ field.name }}, dims={{ field.name }}_dims),
        {%- elif field.ftype %}
        getter({{ field.ftype }}, {{ module.name }}.{{ type.name }}_get_{{ field.name }}),
        {%- else %}
        getter({{ field.pytype }}, {{ module.name }}.{{ type.name }}_get_{{ field.name }}),
        {%- endif %}
        {%- if field.setter == 'subroutine' %}
        setter({{ field.pytype }}, {{ module.name }}.{{ type.name }}_set_{{ field.name }})
        {%- elif field.ftype and not field.dims %}
        setter({{ field.ftype }}, {{ module.name }}.{{ type.name }}_get_{{ field.name }}),
        {%- elif field.dims %}
        setter({{ field.pytype }}, {{ module.name }}.{{ type.name }}_get_{{ field.name }}, dims={{ field.name }}_dims),
        {%- else %}
        None,
        {%- endif %}
        {%- if field.dynamic %}
        allocator({{ module.name }}.{{ type.name }}_alloc_{{ field.name }}
            {%- if field.dims %}, dims={{ field.name }}_dims{% endif -%}
        )
        {%- endif %}
    )
    {%- endfor %}
{%- endmacro %}

{% macro join_dims(dims) -%}
    {% for dim in dims %}{{ dim }}{% if not loop.last %}, {% endif %}{% endfor %}
{%- endmacro %}
