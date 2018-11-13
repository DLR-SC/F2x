{#-##################################################################################################################-#}
{#- F2x 'ctypes' template call library.                                                                              -#}
{#-                                                                                                                  -#}
{#- This library provides macros to generate Python methods that handle parameter conversion and calling of ctypes   -#}
{#- function as exported by the 'bindc' template.                                                                    -#}
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
	{%- if field.dims and field.strlen %}
    {{ field.name }} = ArrayField(
        "{{ field.name }}",
        {{ field.ftype or field.pytype }},
        [{{ join_dims(field.dims) }}],
        library.{{ type.name }}_get_{{ field.name }},
        library.{{ type.name }}_set_{{ field.name }}
        {%- if field.dynamic %},
        library.{{ type.name }}_alloc_{{ field.name }}
        {%- else %}
        None
        {%- endif %},
        {{ field.strlen }}
    )
    {%- elif field.dims %}
    {{ field.name }} = ArrayField(
        "{{ field.name }}",
        {{ field.ftype or field.pytype }},
        [{{ join_dims(field.dims) }}],
        library.{{ type.name }}_get_{{ field.name }},
        None
        {%- if field.dynamic %},
        library.{{ type.name }}_alloc_{{ field.name }}
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
