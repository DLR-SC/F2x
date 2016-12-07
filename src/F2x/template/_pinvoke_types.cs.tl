{#####################################################################################################################}
{# F2x-Template library: _pinvoke_types.cs.tl                                                                         #}
{#                                                                                                                   #}
{# This library provides macros used by the '_bindc.cs.t' template for derived type handling.                        #}
{#####################################################################################################################}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Import the accessor routines for a field.                                                                         #}
{% macro import(type, field, lib_name) -%}
    {%- if field.dims %}

        {%- if field.dynamic %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_{{ field.name }}_init")]
        private extern static void Init{{ field.name }}(IntPtr ptr, Int32 size);
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_{{ field.name }}_clear")]
        private extern static void Clear{{ field.name }}(IntPtr ptr);
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_{{ field.name }}_size")]
        private extern static Int32 SizeOf{{ field.name }}(IntPtr ptr);
        {%- endif %}

        {%- if field.ftype %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_get_{{ field.name }}")]
        private extern static IntPtr Get{{ field.name }}(IntPtr ptr, Int32 index);
        {%- else %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_get_{{ field.name }}")]
        private extern static IntPtr Get{{ field.name }}(IntPtr ptr);
        {%- endif %}

        {%- elif field.getter == "subroutine" and field.strlen %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_get_{{ field.name }}", CharSet=CharSet.Auto)]
        private extern static void Get{{ field.name }}(IntPtr ptr, StringBuilder value);

    {%- elif field.getter == "function" %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_get_{{ field.name }}")]
        private extern static {{ field.cstype }} Get{{ field.name }}(IntPtr ptr);
    {%- endif %}

    {%- if field.setter == "subroutine" %}
        [DllImport("{{ lib_name }}", EntryPoint="{{ type.name }}_set_{{ field.name }}")]
        private extern static void Set{{ field.name }}(IntPtr ptr, {{ field.cstype }} value);
    {%- endif %}
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Create property to access field.                                                                                  #}
{% macro property(type, field) -%}
    {%- if field.dims %}
        public
            {%- if field.dynamic %} FDynamicArrayField
            {%- else %} FArrayField
            {%- endif %}<
            {%- if field.ftype %}{{ field.ftype }}
            {%- else %}{{ field.cstype }}
            {%- endif %}> {{ field.name }} {
    {%- elif field.ftype %}
        public {{ field.ftype }} {{ field.name }} {
    {%- else %}
        public {{ field.cstype }} {{ field.name }} {
    {%- endif %}
            {{ getter(type, field) }}
        {%- if field.setter == "subroutine" %}
            {{ setter(type, field) }}
        {%- endif %}
        }
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Generate property getter.                                                                                         #}
{% macro getter(type, field) -%}
            get {
    {%- if field.dims %}
        {%- if field.ftype %}
                return new FTypeArrayField<{{ field.ftype }}>(
                    this.ptr,
                    {{ type.name }}.Init{{ field.name }},
                    {{ type.name }}.Clear{{ field.name }},
                    {{ type.name }}.SizeOf{{ field.name }},
                    {{ type.name }}.Get{{ field.name }}
                );
        {%- elif field.dynamic %}
                return new FDynamicArrayField{{ field.cstype }}(
                    this.ptr,
                    {{ type.name }}.Init{{ field.name }},
                    {{ type.name }}.Clear{{ field.name }},
                    {{ type.name }}.SizeOf{{ field.name }},
                    {{ type.name }}.Get{{ field.name }}
                );
        {%- else %}
                return new F{{ field.dims|length }}DArrayField{{ field.cstype }}(
                    this.ptr, new Int32[] { {% for dim in field.dims %}{{ dim }}{% if not loop.last %}, {% endif %} }{% endfor %},
                    {{ type.name }}.Get{{ field.name }}
                );
        {%- endif %}
    {%- elif field.getter == "subroutine" and field.strlen %}
                StringBuilder result = new StringBuilder({{ field.strlen + 1 }});
                {{ type.name }}.Get{{ field.name }}(this.ptr, result);
                return result.ToString();
    {%- elif field.ftype %}
                IntPtr ptr = {{ type.name }}.Get{{ field.name }}(this.ptr);
                return new {{ field.ftype }}(ptr);
    {%- else %}
                return {{ type.name }}.Get{{ field.name }}(this.ptr);
    {%- endif %}
            }
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Generate property setter.                                                                                         #}
{% macro setter(type, field) -%}
            set {
                {{ type.name }}.Set{{ field.name }}(this.ptr, value);
            }
{%- endmacro %}

{#-------------------------------------------------------------------------------------------------------------------#}
{# Generate CopyFrom method for derived types.                                                                       #}
{% macro copy(type) -%}
        public override void CopyFrom(FType other) {
            {{ type.name }} from = ({{ type.name }})other;
    {%- for field in type.fields %}
            {%- if field.setter %}
            this.{{ field.name }} = from.{{ field.name }};
            {%- elif field.dims %}
            {%- if field.dynamic %}
            this.{{ field.name }}.Init(from.{{ field.name }}.Size);
            {%- endif %}
            {%- if field.ftype %}
            for (Int32 i = 0; i < from.{{ field.name }}.Size; i++) {
                this.{{ field.name }}[i].CopyFrom(from.{{ field.name }}[i]);
            }
            {%- else %}
            {{ field.cstype }}[] {{ field.name }}_buffer = new {{ field.cstype }}[from.{{ field.name }}.Size];
            from.{{ field.name }}.CopyTo({{ field.name }}_buffer);
            this.{{ field.name }}.CopyFrom({{ field.name }}_buffer);
            {%- endif %}
            {%- elif field.ftype %}
            this.{{ field.name }}.CopyFrom(from.{{ field.name }});
            {%- endif %}
    {%- endfor %}
        }
{%- endmacro %}
