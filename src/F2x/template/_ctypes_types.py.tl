{#####################################################################################################################}
{# F2x-Template library: _ctypes_types.f90.tl                                                                        #}
{#                                                                                                                   #}
{# This library provides macros used by the '_glue.py.t' template for derived type handling.                         #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type imports: Define interface to imported C routines.                                                            #}
{% macro import(type) -%}
{{ module.name }}.{{ type.name }}_new.argtypes = []
{{ module.name }}.{{ type.name }}_new.restype = ctypes.c_void_p
{{ module.name }}.{{ type.name }}_free.argtypes = [ctypes.c_void_p]
{{ module.name }}.{{ type.name }}_free.restype = None
{%- for field in type.fields %}

{#- Arrays need special treatment ... #}
{%- if field.dims %}

{#- ... providing dynamic allocation ... #}
{%- if field.dynamic %}
{{ module.name }}.{{ type.name }}_{{ field.name }}_init.argtypes = [ctypes.c_void_p, ctypes.c_int]
{{ module.name }}.{{ type.name }}_{{ field.name }}_init.restype = None
{{ module.name }}.{{ type.name }}_{{ field.name }}_clear.argtypes = [ctypes.c_void_p]
{{ module.name }}.{{ type.name }}_{{ field.name }}_clear.restype = None
{{ module.name }}.{{ type.name }}_{{ field.name }}_size.argtypes = [ctypes.c_void_p]
{{ module.name }}.{{ type.name }}_{{ field.name }}_size.restype = ctypes.c_int
{%- endif %}

{#- ... item access for derived type arrays ...#}
{%- if field.ftype %}
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.argtypes = [ctypes.c_void_p, ctypes.c_int]
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.restype = ctypes.c_void_p
{#- ... and normal access to others. #}
{%- else %}
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.argtypes = [ctypes.c_void_p]
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.restype = ctypes.c_void_p
{%- endif %}

{#- Subroutines getter only for strings. #}
{%- elif field.getter == 'subroutine' and field.strlen %}
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.restype = None

{#- All other getters are straight forward. #}
{%- elif field.getter == 'function' %}
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.argtypes = [ctypes.c_void_p]
{{ module.name }}.{{ type.name }}_get_{{ field.name }}.restype = {{ field.pytype }}
{%- endif %}

{#- All setter share the same interface (if available). #}
{%- if field.setter == 'subroutine' %}
{{ module.name }}.{{ type.name }}_set_{{ field.name }}.argtypes = [ctypes.c_void_p, {{ field.pytype }}]
{{ module.name }}.{{ type.name }}_set_{{ field.name }}.restype = None
{%- endif %}
{%- endfor %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field getter: Reads the field value from Fortran derived type behind void pointer. Automatically converts to #}
{# Python types.                                                                                                     #}
{% macro getter(type, field, prop) -%}
    {#- Arrays need special treatment... #}
    {%- if field.dims %}

    {%- if not field.ftype %}
    {#- ... arrays of derived types are handled seperately, all other are returned as native arrays. #}

    {#- Implied-size arrays are only allocatable at runtime. #}
    {%- if not field.dynamic %}
    {%- do prop.append("_get_" + field.name) %}
    _{{ field.name }}_TYPE = {{ field.pytype }}{% for dim in field.dims %} * {{ dim }}{% endfor %}
    def _get_{{ field.name }}(self):
        if not hasattr(self, '_{{ field.name }}'):
            address = {{ module.name }}.{{ type.name }}_get_{{ field.name }}(self._ptr)
            self._{{ field.name }} = self._{{ field.name }}_TYPE.from_address(address)
        return self._{{ field.name }}
    {%- endif %}
    {%- endif %}

    {#- Subroutine as getter only allowed for strings (so far). #}
    {%- elif field.getter == 'subroutine' and field.strlen %}
    {%- do prop.append("_get_" + field.name) %}
    def _get_{{ field.name }}(self):
        buffer = ctypes.create_string_buffer({{ field.strlen + 1 }})
        {{ module.name }}.{{ type.name }}_get_{{ field.name }}(self._ptr, buffer)
        return buffer.value

	{#- Other types are (more ore less) straight forward. #}
    {%- elif field.getter == 'function' %}
    {%- do prop.append("_get_" + field.name) %}
    def _get_{{ field.name }}(self):
        {%- if field.ftype %}
        {#- Derived types are automatically wrapped. #}
        return {{ field.ftype }}({{ module.name }}.{{ type.name }}_get_{{ field.name }}(self._ptr))
        {%- else %}
        return {{ module.name }}.{{ type.name }}_get_{{ field.name }}(self._ptr)
    	{%- endif %}
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field setter: Assigns the given value to the derived type's field automatically converting Python types.     #}
{% macro setter(type, field, prop) -%}
    {%- if field.dims and not field.dynamic and not field.ftype %}
    {%- do prop.append("_set_" + field.name) %}
    def _set_{{ field.name }}(self, value):
        self.{{ field.name }}[:] = value

    {%- elif field.setter == 'subroutine' %}
    {%- do prop.append("_set_" + field.name) %}
    def _set_{{ field.name }}(self, value):
        {{ module.name }}.{{ type.name }}_set_{{ field.name }}(self._ptr, value)
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Type field property: Combine getter and setter to a nice property.                                                #}
{% macro property(type, field, prop) -%}
    {#- Arrays of derived types have their own access method. #}
    {%- if field.dims and field.ftype %}
    {{ field.name }} = DerivedTypeArray(
        {{ field.ftype }},
        {{ module.name }}.{{ type.name }}_{{ field.name }}_init,
        {{ module.name }}.{{ type.name }}_{{ field.name }}_clear,
        {{ module.name }}.{{ type.name }}_{{ field.name }}_size,
        {{ module.name }}.{{ type.name }}_get_{{ field.name }},
    )
    
    {#- Dynamically allocated arrays also have their own access method. #}
    {%- elif field.dims and field.dynamic %}
    {{ field.name }} = DynamicArray(
        {{ field.pytype }},
        {{ module.name }}.{{ type.name }}_{{ field.name }}_init,
        {{ module.name }}.{{ type.name }}_{{ field.name }}_clear,
        {{ module.name }}.{{ type.name }}_{{ field.name }}_size,
        {{ module.name }}.{{ type.name }}_get_{{ field.name }},
    )
    
    {#- Otherwise, create a normal property. #}
    {%- else %}
    {{ field.name }} = property({% for name in prop %}{{ name }}{% if not loop.last %}, {% endif %}{% endfor %})
    {%- endif %}
{% endmacro -%}
