/* This is a small helper libray to allow error handling using setjmp/longjmp from Fortran.

   Copyright 2018 German Aerospace Center (DLR)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
#include <stdio.h>
#include <setjmp.h>
#include <stdbool.h>

static jmp_buf f2x_err_jmp_buf;
static bool f2x_err_active = false;
static int f2x_err_code;

/* Return address of f2x_err_jmp_buf if not already in use. */
jmp_buf *f2x_prepare_jmp_buffer() {
    if (f2x_err_active == true) {
        f2x_err_code = -1;
        return 0;
    } else {
        f2x_err_active = true;
        return &f2x_err_jmp_buf;
    }
}

/* Reset f2x_err_jmp_buf usage. */
void f2x_clear_jmp_buffer() {
    f2x_err_active = false;
}

/* Handle error. Should be called, when an error occured. */
void f2x_err_handle(int code) {
    if (f2x_err_active == true) {
        f2x_err_code = code;
        f2x_err_active = false;
        longjmp(f2x_err_jmp_buf, 1);
    } else {
        fprintf(stderr, "WARNING: f2x_err_handle called with %d but no error context enabled.", code);
    }
}


/* Set error code. */
void f2x_err_reset() {
    f2x_err_code = 0;
}


/* Read error code. */
int f2x_err_get() {
    return f2x_err_code;
}
