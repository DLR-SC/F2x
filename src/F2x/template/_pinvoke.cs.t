/* PInvoke-based wrapper for {{ context.filename }}
 * This file was generated by F2x. Please do not edit directly.
 */
using System;
using System.Runtime.InteropServices;
{%- for typ in module.types %}

/* Interface for {{ typ.name }} from {{ module.name }}. */
public class {{ typ.name }} {
	private IntPtr c_ptr = null;
	
	/* Constructor: Allocate Fortran object. */
	public {{ typ.name }}() {
		this.c_ptr = {{ typ.name }}._new();
	}
	
	/* Constructor: Reuse Fortran object. */
	public {{ typ.name }}(IntPtr c_ptr) {
		this.c_ptr = c_ptr;
	}
	
	/* Destructor: Release Fortran object. */
	~{{ typ.name }}() {
		{{ typ.name }}._free(this.c_ptr);
	}
	
	/* Getter/setter for attributes. */
{%- for field in typ.fields %}
	public {{ field.cstype }} {{ field.name }} {
{%- if field.getter == 'function' %}
		get { return {{ typ.name }}.get_{{ field.name }}(this.c_ptr); }
{%- elif field.getter == 'subroutine' %}
		get {
			{{ field.cstype }} value;
			{{ typ.name }}.get_{{ field.name }}(this.c_ptr, value);
			return value;
		}
{%- endif %}
{%- if field.setter %}
		set { {{ typ.name }}.set_{{ field.name }}(this.c_ptr, value); }
{%- endif %}
	}
{%- endfor %}

	/* Exported C interface. */
{%- for field in typ.fields %}
{%- if field.getter == 'function' %}
	[DllImport("{{ config.get('generate', 'dll') }}", EntryPoint="{{ typ.name }}_get_{{ field.name }}")]
	public static extern {{ field.cstype }} get_{{ field.name }}(IntPtr c_ptr);
{%- elif field.getter == 'subroutine' %}
	[DllImport("{{ config.get('generate', 'dll') }}", EntryPoint="{{ typ.name }}_get_{{ field.name }}")]
	public static extern void get_{{ field.name }}(IntPtr c_ptr, ref {{ field.cstype }} value);
{%- endif %}
{%- if field.setter %}
	[DllImport("{{ config.get('generate', 'dll') }}", EntryPoint="{{ typ.name }}_set_{{ field.name }}")]
	public static extern void get_{{ field.name }}(IntPtr c_ptr, {{ field.cstype }} value);
{%- endif %}
{%- endfor %}
}
{%- endfor %}


{%- if config.has_section('export') %}
{%- set exports = config.options('export') %}
/* Exported functions and subroutines of {{ module.name }}. */
public static class {{ module.name }} {
{%- for function in module.functions %}
{%- if function.name.lower() in exports %}
{%- set export_name = config.get('export', function.name.lower()) %}
{%- set call_args = [] %}
{%- if function.ret.getter == 'function' %}
	[DllImport("{{ config.get('generate', 'dll') }}", EntryPoint="{{ export_name }}")]
	public static {{ function.ret.cstype }} {{ export_name }}({% for arg in function.args %}{{ arg.cstype }} {{ arg.name }}{% if not loop.last %}, {% endif %}{% endfor %});
{%- elif function.ret.getter == 'subroutine' %}
	[DllImport("{{ config.get('generate', 'dll') }}", EntryPoint="{{ export_name }}")]
	public static void {{ export_name }}({% for arg in function.args %}{{ arg.cstype }} {{ arg.name }}, {% endfor %}ref {{ function.ret.cstype }} value);
{%- endif %}
{%- endif %}
{%- endfor %}
}
{%- endif %}
