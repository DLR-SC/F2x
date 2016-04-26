! BIND(C)-based wrapper for {{ context.filename }}
! This file was generated by F2x. Please do not edit directly.
MODULE {{ module.name }}_BINDC
	USE, INTRINSIC :: ISO_C_BINDING
	USE WRAP_UTIL
{%- for use in module.uses %}
	USE {{ use }}
{%- endfor %}
	USE {{ module.name }}
	
	IMPLICIT NONE
	
	INTEGER, PARAMETER :: LF = 8 ! TODO: Move to configuration

CONTAINS
{%- for typ in module.types %}
	
	!--------------------------------------------------------------------------
	! Interface for {{ typ.name }} from {{ module.name }}.
	!
	FUNCTION {{ typ.name }}_NEW() BIND(C, NAME="{{ typ.name }}_new")
		TYPE(C_PTR) :: {{ typ.name }}_NEW
		TYPE({{ typ.name }}), POINTER :: {{ typ.name }}_INTERN
		
		ALLOCATE({{ typ.name}}_INTERN)
		{{ typ.name }}_NEW = C_LOC({{ typ.name }}_INTERN)
	END FUNCTION
	
	SUBROUTINE {{ typ.name }}_FREE({{ typ.name }}_INST) BIND(C, NAME="{{ typ.name }}_free")
		TYPE(C_PTR), VALUE, INTENT(IN) :: {{ typ.name }}_INST
		TYPE({{ typ.name }}), POINTER :: {{ typ.name }}_INTERN
		
		CALL C_F_POINTER({{ typ.name }}_INST, {{ typ.name }}_INTERN)
		DEALLOCATE({{ typ.name }}_INTERN)
	END SUBROUTINE
{%- for field in typ.fields %}
{%- if field.getter == 'function' %}

	FUNCTION {{ typ.name }}_GET_{{ field.name }}({{ typ.name }}_INST) BIND(C, NAME="{{ typ.name }}_get_{{ field.name }}")
		{{ field.type }} :: {{ typ.name }}_GET_{{ field.name }}
		TYPE(C_PTR), VALUE, INTENT(IN) :: {{ typ.name }}_INST
		TYPE({{ typ.name }}), POINTER :: {{ typ.name }}_INTERN
		{%- if field.ftype %}
		TYPE({{ field.ftype }}), POINTER :: {{ field.name }}_INTERN
		{%- endif %}
		
		CALL C_F_POINTER({{ typ.name }}_INST, {{ typ.name }}_INTERN)
		{%- if field.ftype %}
		{{ field.name }}_INTERN => {{ typ.name }}_INTERN%{{ field.name }}
		{{ typ.name }}_GET_{{ field.name }} = C_LOC({{ field.name }}_INTERN)
		{%- else %}
		{{ typ.name }}_GET_{{ field.name }} = {{ typ.name }}_INTERN%{{ field.name }}
		{%- endif %}
	END FUNCTION
{%- elif field.getter == 'subroutine' %}

	SUBROUTINE {{ typ.name }}_GET_{{ field.name }}({{ typ.name }}_INST, {{ field.name }}_VALUE) BIND(C, NAME="{{ typ.name }}_get_{{ field.name }}")
		TYPE(C_PTR), VALUE, INTENT(IN) :: {{ typ.name }}_INST
		{{ field.type }}, VALUE, INTENT(IN) :: {{ field.name }}_VALUE
		TYPE({{ typ.name }}), POINTER :: {{ typ.name }}_INTERN

		CALL C_F_POINTER({{ typ.name }}_INST, {{ typ.name }}_INTERN)
		{%- if field.strlen %}
		CALL F_C_STRING({{ typ.name }}_INTERN%{{ field.name }}, {{ field.strlen }}, {{ field.name }}_VALUE)
		{%- endif %}
	END SUBROUTINE
{%- endif %}
{%- if field.setter %}

	SUBROUTINE {{ typ.name }}_SET_{{ field.name }}({{ typ.name }}_INST, {{ field.name }}_VALUE) BIND(C, NAME="{{ typ.name }}_set_{{ field.name }}")
		TYPE(C_PTR), VALUE, INTENT(IN) :: {{ typ.name }}_INST
		{{ field.type }}, VALUE, INTENT(IN) :: {{ field.name }}_VALUE
		TYPE({{ typ.name }}), POINTER :: {{ typ.name }}_INTERN

		CALL C_F_POINTER({{ typ.name }}_INST, {{ typ.name }}_INTERN)
		{%- if field.strlen %}
		CALL C_F_STRING({{ field.name }}_VALUE, {{ field.strlen }}, {{ typ.name }}_INTERN%{{ field.name }})
		{%- else %}
		{{ typ.name }}_INTERN%{{ field.name }} = {{ field.name }}_VALUE
		{%- endif %}
	END SUBROUTINE
{%- endif %}
{%- endfor %}
{%- endfor %}
{%- if config.has_section('export') %}
{%- set exports = config.options('export') %}

	!------------------------------------------------------------
	! Exported functions and subroutines
	!
{%- for function in module.functions %}
{%- if function.name.lower() in exports %}
{%- set export_name = config.get('export', function.name.lower()) %}
{%- set call_args = [] %}
{%- if function.ret.getter == 'function' %}

	FUNCTION {{ export_name.upper() }}({% for arg in function.args %}{{ arg.name }}{% if not loop.last %}, {% endif %}{% endfor %}) BIND(C, NAME="{{ export_name }}")
		{{ function.ret.type }} :: {{ export_name.upper() }}
	{%- for arg in function.args %}
	{%- if arg.dims %}
		{{ arg.type }}, INTENT({{ arg.intent }}) :: {{ arg.name }}({{ ', '.join(arg.dims) }})
		{%- do call_args.append(arg.name) %}
	{%- else %}
		{{ arg.type }}, {% if arg.intent == 'IN' %}VALUE, {% endif %}INTENT({{ arg.intent }}) :: {{ arg.name }}
		{%- if arg.strlen %}
		CHARACTER(LEN={{ arg.strlen }}) :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- elif arg.ftype %}
		TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- else %}
		{%- do call_args.append(arg.name) %}
		{%- endif %}
	{%- endif %}
	{%- endfor %}
	{%- if function.ret.ftype %}
		TYPE({{ function.ret.ftype }}), POINTER :: {{ function.name.upper() }}_INTERN
	{%- endif %}
	{% for arg in function.args %}
	{%- if arg.strlen %}
		CALL C_F_STRING({{ arg.name }}, {{ arg.strlen }}, {{ arg.name }}_INTERN)
	{%- elif arg.ftype %}
		CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
	{%- endif %}
	{%- endfor %}
	{%- if function.ret.ftype %}
		ALLOCATE({{ function.name.upper() }}_INTERN)
		{{ function.name.upper() }}_INTERN = {{ function.name }}({{ ', '.join(call_args) }})
		{{ export_name.upper() }} = C_LOC({{ function.name.upper() }}_INTERN)
	{%- else %}
		{{ export_name.upper() }} = {{ function.name }}({{ ', '.join(call_args) }})
	{%- endif %}
	END FUNCTION
{%- elif function.ret.getter == 'subroutine' %}

	SUBROUTINE {{ export_name.upper() }}({% for arg in function.args %}{{ arg.name }}, {% endfor %}{{ export_name.upper() }}_VALUE) BIND(C, NAME="{{ export_name }}")
	{%- for arg in function.args %}
	{%- if arg.dims %}
		{{ arg.type }}, INTENT({{ arg.intent }}) :: {{ arg.name }}({{ ', '.join(arg.dims) }})
		{%- do call_args.append(arg.name) %}
	{%- else %}
		{{ arg.type }}, {% if arg.intent == 'IN' %}VALUE, {% endif %}INTENT({{ arg.intent }}) :: {{ arg.name }}
		{%- if arg.strlen %}
		CHARACTER(LEN={{ arg.strlen }}) :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- elif arg.ftype %}
		TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- else %}
		{%- do call_args.append(arg.name) %}
		{%- endif %}
	{%- endif %}
	{%- endfor %}
		{%- if function.ret.strlen %}
		{{ function.ret.type }}, VALUE, INTENT(IN) :: {{ export_name.upper() }}_VALUE{% if function.ret.dims %}({{ ', '.join(function.ret.dims) }}){% endif %}
		CHARACTER(LEN={{ function.ret.strlen }}) :: {{ export_name.upper() }}_INTERN
		{%- else %}
		{{ function.ret.type }}, INTENT(INOUT) :: {{ export_name.upper() }}_VALUE{% if function.ret.dims %}({{ ', '.join(function.ret.dims) }}){% endif %}
		{%- endif %}
	{% for arg in function.args %}
	{%- if arg.strlen %}
		CALL C_F_STRING({{ arg.name }}, {{ arg.strlen }}, {{ arg.name }}_INTERN)
	{%- elif arg.ftype %}
		CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
	{%- endif %}
	{%- endfor %}
	{%- if function.ret.strlen %}
		{{ export_name.upper() }}_INTERN = {{ function.name }}({{ ', '.join(call_args) }})
		CALL F_C_STRING({{ export_name.upper() }}_INTERN, {{ function.ret.strlen }}, {{ export_name.upper() }}_VALUE)
	{%- else %}
		{{ export_name.upper() }}_VALUE{% if function.ret.dims %}({{ ', '.join([':'] * function.ret.dims|length) }}){% endif %} = {{ function.name }}({{ ', '.join(call_args) }})
	{%- endif %}
	END SUBROUTINE
{%- endif %}
{%- endif %}
{%- endfor %}
{%- for subroutine in module.subroutines %}
{%- if subroutine.name.lower() in exports %}
{%- set export_name = config.get('export', subroutine.name.lower()) %}
{%- set call_args = [] %}

	SUBROUTINE {{ export_name.upper() }}({% for arg in subroutine.args %}{{ arg.name }}{% if not loop.last %}, {% endif %}{% endfor %}) BIND(C, NAME="{{ export_name }}")
	{%- for arg in subroutine.args %}
	{%- if arg.dims %}
		{{ arg.type }}, INTENT({{ arg.intent }}) :: {{ arg.name }}({{ ', '.join(arg.dims) }})
		{%- do call_args.append(arg.name) %}
	{%- else %}
		{{ arg.type }}, {% if arg.intent == 'IN' %}VALUE, {% endif %}INTENT({{ arg.intent }}) :: {{ arg.name }}
		{%- if arg.strlen %}
		CHARACTER(LEN={{ arg.strlen }}) :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- elif arg.ftype %}
		TYPE({{ arg.ftype }}), POINTER :: {{ arg.name }}_INTERN
		{%- do call_args.append(arg.name + '_INTERN') %}
		{%- else %}
		{%- do call_args.append(arg.name) %}
		{%- endif %}
	{%- endif %}
	{%- endfor %}
	{% for arg in subroutine.args %}
	{%- if arg.strlen %}
		CALL C_F_STRING({{ arg.name }}, {{ arg.strlen }}, {{ arg.name }}_INTERN)
	{%- elif arg.ftype %}
		CALL C_F_POINTER({{ arg.name }}, {{ arg.name }}_INTERN)
	{%- endif %}
	{%- endfor %}
		CALL {{ subroutine.name }}({{ ', '.join(call_args) }})
	END SUBROUTINE
{%- endif %}
{%- endfor %}
{%- endif %}
END
