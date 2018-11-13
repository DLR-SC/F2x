{#-##################################################################################################################-#}
{#- F2x 'cerr' main template.                                                                                        -#}
{#-                                                                                                                  -#}
{#- This template generates a small C wrapper to allow longjmp-based error handling.                                 -#}
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
void *{{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}void *, {% endif %}{% if arg.strlen == '*' %}void *, {% endif %}void *{% if not loop.last %}, {% endif %}{% endfor %});
void *{{ method.export_name }}_cerr({% for arg in method.args %}{% if arg.dims %}void *arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}void *arg{{ loop.index0 }}_length, {% endif %}void *arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %}) {
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();
    void *result = 0;

    if (_jmp_buf == 0) {
        return 0;
    }

    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        result = {{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}arg{{ loop.index0 }}_length, {% endif %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
        f2x_clear_jmp_buffer();
    }

    f2x_clear_jmp_buffer();
    return result;
}
{%- endmacro %}


{% macro export_function_out(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}void *, {% endif %}{% if arg.strlen == '*' %}void *, {% endif %}void *, {% endfor %}void *);
void {{ method.export_name }}_cerr({% for arg in method.args %}{% if arg.dims %}void *arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}void *arg{{ loop.index0 }}_length, {% endif %}void *arg{{ loop.index0 }}, {% endfor %}void *out) {
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();

    if (_jmp_buf == 0) {
        return;
    }

    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}arg{{ loop.index0 }}_length, {% endif %}arg{{ loop.index0 }}, {% endfor %}out);
    }

    f2x_clear_jmp_buffer();
}
{%- endmacro %}


{% macro export_subroutine(method) -%}
/* Prototype for BIND(C) routine {{ method.name }} */
void {{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}void *, {% endif %}{% if arg.strlen == '*' %}void *, {% endif %}void *{% if not loop.last %}, {% endif %}{% endfor %});
void {{ method.export_name }}_cerr({% for arg in method.args %}{% if arg.dims %}void *arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}void *arg{{ loop.index0 }}_length, {% endif %}void *arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %}) {
    jmp_buf *_jmp_buf = f2x_prepare_jmp_buffer();

    if (_jmp_buf == 0) {
        return;
    }


    if (setjmp(*_jmp_buf) == 0) {
        f2x_err_reset();
        {{ method.export_name }}({% for arg in method.args %}{% if arg.dims %}arg{{ loop.index0 }}_size, {% endif %}{% if arg.strlen == '*' %}arg{{ loop.index0 }}_length, {% endif %}arg{{ loop.index0 }}{% if not loop.last %}, {% endif %}{% endfor %});
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
