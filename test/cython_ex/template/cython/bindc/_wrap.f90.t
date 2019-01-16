{%- import "bindc/methods.f90.tl" as methods with context -%}
{%- import "bindc/types.f90.tl" as types with context -%}
{%- import "bindc/marshal/names.f90.tl" as names with context -%}


MODULE {{ names.module_name(module) }}
    USE {{ module.name|upper }}

CONTAINS

{%- if module.types -%}
    {{ types.export() }}
{%- endif -%}


{%- if module.methods -%}
    {{ methods.export() }}
{%- endif -%}

END