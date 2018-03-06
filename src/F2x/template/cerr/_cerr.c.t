{#-##################################################################################################################-#}
{#- F2x 'cerr' main template.                                                                                        -#}
{#-                                                                                                                  -#}
{#- This template generates a small C wrapper to allow longjmp-based error handling.                                 -#}
{#-##################################################################################################################-#}
#include <setjmp.h>

void f2x_err_reset();

{% macro export_function(method) -%}
    {% if method.ret.dims %}
        {#- Array results are casted to C pointers. -#}
        {{ export_function_ret(method) }}
    {%- elif method.ret.ftype -%}
        {#- TYPE(...) results need to be passed via OUT argument. -#}
        {{ export_function_ret(method) }}
    {%- elif method.ret.strlen -%}
        {#- CHARACTER(*) results need string conversion. -#}
        {{ export_function_out(method) }}
    {%- else -%}
        {{ export_function_ret(method) }}
    {%- endif %}
{%- endmacro %}


{% macro export_function_ret(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void *{{ method.export_name }}({% for arg in method.args %}void *{% if not loop.last %}, {% endif %}{% endfor %});
void *{{ method.export_name }}_cerr({% for arg in method.args %}void *arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %}) {
    static jmp_buf buf;

    if (setjmp(buf) == 0) {
        f2x_err_reset();
        return {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
    } else {
        return 0;
    }
}
{%- endmacro %}


{% macro export_function_out(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}void *, {% endfor %}void *);
void {{ method.export_name }}_cerr({% for arg in method.args %}void *arg{{ loop.index0 }}, {% endfor %}, void *out) {
    static jmp_buf buf;

    if (setjmp(buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}, {% endfor %}out);
    }
}
{%- endmacro %}


{% macro export_subroutine(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}void *{% if not loop.last %}, {% endif %}{% endfor %});
void {{ method.export_name }}_cerr({% for arg in method.args %}void *arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %}) {
    static jmp_buf buf;

    if (setjmp(buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
    }
}
{%- endmacro %}


{%- if module.methods %}
    {%- for method in module.methods %}
        {% if method.ret -%}
            {{ export_function(method) }}
        {%- else -%}
            {{ export_subroutine(method) }}
        {%- endif %}
    {% endfor %}
{%- endif %}
