! This module provides generic array accessors.
MODULE F2X_GLUE
    USE, INTRINSIC :: ISO_C_BINDING

CONTAINS

    !------------------------------------------------------------------------------------------------------------------
    ! Two-dimensional INTEGER array access
    FUNCTION F2X_INT2D_ALLOC(SIZES) BIND(C, NAME="F2x_int2d_alloc")
        TYPE(C_PTR) :: F2X_INT2D_ALLOC
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:, :), POINTER :: ARRAY

        ALLOCATE(ARRAY(SIZES(1), SIZES(2)))
        F2X_INT2D_ALLOC = C_LOC(ARRAY)
    END FUNCTION

    FUNCTION F2X_INT2D_GETITEM(PTR, SIZES, IDX) BIND(C, NAME="F2x_int2d_getitem")
        INTEGER :: F2X_INT2D_GETITEM
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(2), INTENT(IN) :: IDX
        INTEGER, DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        F2X_INT2D_GETITEM = ARRAY(IDX(1) + 1, IDX(2) + 1)
    END FUNCTION

    SUBROUTINE F2X_INT2D_SETITEM(PTR, SIZES, IDX, VALUE) BIND(C, NAME="F2x_int2d_setitem")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(2), INTENT(IN) :: IDX
        INTEGER, INTENT(IN), VALUE :: VALUE
        INTEGER, DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        ARRAY(IDX(1) + 1, IDX(2) + 1) = VALUE
    END SUBROUTINE

    SUBROUTINE F2X_INT2D_FREE(PTR, SIZES) BIND(C, NAME="F2x_int2d_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        DEALLOCATE(ARRAY)
    END SUBROUTINE
    !------------------------------------------------------------------------------------------------------------------


    !------------------------------------------------------------------------------------------------------------------
    ! Three-dimensional INTEGER array access
    FUNCTION F2X_INT3D_ALLOC(SIZES) BIND(C, NAME="F2x_int3d_alloc")
        TYPE(C_PTR) :: F2X_INT3D_ALLOC
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:, :, :), POINTER :: ARRAY

        ALLOCATE(ARRAY(SIZES(1), SIZES(2), SIZES(3)))
        F2X_INT3D_ALLOC = C_LOC(ARRAY)
    END FUNCTION

    FUNCTION F2X_INT3D_GETITEM(PTR, SIZES, IDX) BIND(C, NAME="F2x_int3d_getitem")
        INTEGER :: F2X_INT3D_GETITEM
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(3), INTENT(IN) :: IDX
        INTEGER, DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        F2X_INT3D_GETITEM = ARRAY(IDX(1) + 1, IDX(2) + 1, IDX(3) + 1)
    END FUNCTION

    SUBROUTINE F2X_INT3D_SETITEM(PTR, SIZES, IDX, VALUE) BIND(C, NAME="F2x_int3d_setitem")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(3), INTENT(IN) :: IDX
        INTEGER, INTENT(IN), VALUE :: VALUE
        INTEGER, DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        ARRAY(IDX(1) + 1, IDX(2) + 1, IDX(3) + 1) = VALUE
    END SUBROUTINE

    SUBROUTINE F2X_INT3D_FREE(PTR, SIZES) BIND(C, NAME="F2x_int3d_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        DEALLOCATE(ARRAY)
    END SUBROUTINE
    !------------------------------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------------------------------
    ! Two-dimensional REAL(8) array access
    FUNCTION F2X_REAL2D_ALLOC(SIZES) BIND(C, NAME="F2x_real2d_alloc")
        TYPE(C_PTR) :: F2X_REAL2D_ALLOC
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        REAL(KIND=8), DIMENSION(:, :), POINTER :: ARRAY

        ALLOCATE(ARRAY(SIZES(1), SIZES(2)))
        F2X_REAL2D_ALLOC = C_LOC(ARRAY)
    END FUNCTION

    FUNCTION F2X_REAL2D_GETITEM(PTR, SIZES, IDX) BIND(C, NAME="F2x_real2d_getitem")
        REAL(KIND=8) :: F2X_REAL2D_GETITEM
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(2), INTENT(IN) :: IDX
        REAL(KIND=8), DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        F2X_REAL2D_GETITEM = ARRAY(IDX(1) + 1, IDX(2) + 1)
    END FUNCTION

    SUBROUTINE F2X_REAL2D_SETITEM(PTR, SIZES, IDX, VALUE) BIND(C, NAME="F2x_real2d_setitem")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(2), INTENT(IN) :: IDX
        REAL(KIND=8), INTENT(IN), VALUE :: VALUE
        REAL(KIND=8), DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        ARRAY(IDX(1) + 1, IDX(2) + 1) = VALUE
    END SUBROUTINE

    SUBROUTINE F2X_REAL2D_FREE(PTR, SIZES) BIND(C, NAME="F2x_real2d_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(2), INTENT(IN) :: SIZES
        REAL(KIND=8), DIMENSION(:, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES(:))
        DEALLOCATE(ARRAY)
    END SUBROUTINE
    !------------------------------------------------------------------------------------------------------------------


    !------------------------------------------------------------------------------------------------------------------
    ! Three-dimensional REAL(8) array access
    FUNCTION F2X_REAL3D_ALLOC(SIZES) BIND(C, NAME="F2x_real3d_alloc")
        TYPE(C_PTR) :: F2X_REAL3D_ALLOC
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        REAL(KIND=8), DIMENSION(:, :, :), POINTER :: ARRAY

        ALLOCATE(ARRAY(SIZES(1), SIZES(2), SIZES(3)))
        F2X_REAL3D_ALLOC = C_LOC(ARRAY)
    END FUNCTION

    FUNCTION F2X_REAL3D_GETITEM(PTR, SIZES, IDX) BIND(C, NAME="F2x_real3d_getitem")
        REAL(KIND=8) :: F2X_REAL3D_GETITEM
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(3), INTENT(IN) :: IDX
        REAL(KIND=8), DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        F2X_REAL3D_GETITEM = ARRAY(IDX(1) + 1, IDX(2) + 1, IDX(3) + 1)
    END FUNCTION

    SUBROUTINE F2X_REAL3D_SETITEM(PTR, SIZES, IDX, VALUE) BIND(C, NAME="F2x_real3d_setitem")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        INTEGER, DIMENSION(3), INTENT(IN) :: IDX
        REAL(KIND=8), INTENT(IN), VALUE :: VALUE
        REAL(KIND=8), DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        ARRAY(IDX(1) + 1, IDX(2) + 1, IDX(3) + 1) = VALUE
    END SUBROUTINE

    SUBROUTINE F2X_REAL3D_FREE(PTR, SIZES) BIND(C, NAME="F2x_real3d_free")
        TYPE(C_PTR), INTENT(IN), VALUE :: PTR
        INTEGER, DIMENSION(3), INTENT(IN) :: SIZES
        REAL(KIND=8), DIMENSION(:, :, :), POINTER :: ARRAY

        CALL C_F_POINTER(PTR, ARRAY, SIZES)
        DEALLOCATE(ARRAY)
    END SUBROUTINE
    !------------------------------------------------------------------------------------------------------------------


END
