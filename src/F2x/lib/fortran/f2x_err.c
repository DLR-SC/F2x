/* This is a small helper libray to allow error handling using setjmp/longjmp from Fortran. */
#include <stdio.h>
#include <setjmp.h>

static jmp_buf f2x_err_jmp_buf;
static int f2x_err_code;

/* Prepare error handling. Should be called in wrapper function before FORTRAN code is executed. */
int f2x_err_prepare() {
    if (setjmp(f2x_err_jmp_buf) == 0) {
        f2x_err_code = 0;
        return 0;
    } else {
        return f2x_err_code;
    }
}

/* Handle error. Should be called, when an error occured. */
void f2x_err_handle(int code) {
    f2x_err_code = code;
    longjmp(f2x_err_jmp_buf, 1);
}


/* Set error code. */
void f2x_err_reset() {
    f2x_err_code = 0;
}


/* Read error code. */
int f2x_err_get() {
    return f2x_err_code;
}
