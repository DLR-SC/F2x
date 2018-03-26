{#-##################################################################################################################-#}
{#- F2x 'cerr' main template.                                                                                        -#}
{#-                                                                                                                  -#}
{#- This template generates a small C wrapper to allow longjmp-based error handling.                                 -#}
{#-##################################################################################################################-#}
#include <setjmp.h>

void f2x_err_reset();
jmp_buf *f2x_prepare_jmp_buffer();
void f2x_clear_jmp_buffer();

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
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();
    void *result = 0;

    if (_jmp_buf == 0) {
        return 0;
    }

    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        result = {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
        f2x_clear_jmp_buffer();
    }

    f2x_clear_jmp_buffer();
    return result;
}
{%- endmacro %}


{% macro export_function_out(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}void *, {% endfor %}void *);
void {{ method.export_name }}_cerr({% for arg in method.args %}void *arg{{ loop.index0 }}, {% endfor %}, void *out) {
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();

    if (_jmp_buf != 0) {
        return;
    }

    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}, {% endfor %}out);
    }

    f2x_clear_jmp_buffer();
}
{%- endmacro %}


{% macro export_subroutine(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}void *{% if not loop.last %}, {% endif %}{% endfor %});
void {{ method.export_name }}_cerr({% for arg in method.args %}void *arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %}) {
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();

    if (_jmp_buf == 0) {
        return;
    }


    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
    }

    f2x_clear_jmp_buffer();
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
