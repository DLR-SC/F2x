! This module is part of the F2x test suites. It provides some types and routines that reflect the supported spectrum.
MODULE SOURCE

    ! BASIC_TYPE has some different fields all of which are supported built-in types.
    TYPE :: BASIC_TYPE
        INTEGER :: INTFIELD
        REAL(8) :: REALFIELD
        LOGICAL :: LOGICALFIELD

        CHARACTER(32) :: CHARFIELD

        INTEGER                        :: INTARRAY(3)
        REAL(8), ALLOCATABLE           :: REALARRAY(:)
        LOGICAL, POINTER, DIMENSION(:) :: LOGICALARRAY => NULL()
    END TYPE

    ! COMPOUND_TYPE aggregates BASIC_TYPE in fields and arrays.
    TYPE :: COMPOUND_TYPE
        TYPE(BASIC_TYPE) :: BASICFIELD
        TYPE(BASIC_TYPE), ALLOCATABLE :: ALLOCATEFIELD
        TYPE(BASIC_TYPE), POINTER :: POINTERFIELD => NULL()

        TYPE(BASIC_TYPE), ALLOCATABLE :: BASICARRAY(:)
    END TYPE

CONTAINS

    ! Routine to test supported args as input. All values are printed.
    SUBROUTINE BASIC_ARGS_IN(ININT, INREAL, INLOGICAL)
        INTEGER, INTENT(IN) :: ININT
        REAL(8), INTENT(IN) :: INREAL
        LOGICAL, INTENT(IN) :: INLOGICAL

        WRITE (*,*) ININT, INREAL, INLOGICAL
    END SUBROUTINE

    ! Routine to test supported args as output values. It sets the arguments to 1, 2.3 and .TRUE.
    SUBROUTINE BASIC_ARGS_OUT(OUTINT, OUTREAL, OUTLOGICAL)
        INTEGER, INTENT(OUT) :: OUTINT
        REAL(8), INTENT(OUT) :: OUTREAL
        LOGICAL, INTENT(OUT) :: OUTLOGICAL

        OUTINT = 1
        OUTREAL = 2.3
        OUTLOGICAL = .TRUE.
    END SUBROUTINE

    ! Routine to test supported args as inout values. Manipulates the values (see body).
    SUBROUTINE BASIC_ARGS_INOUT(INOUTINT, INOUTREAL, INOUTLOGICAL)
        INTEGER, INTENT(INOUT) :: INOUTINT
        REAL(8), INTENT(INOUT) :: INOUTREAL
        LOGICAL, INTENT(INOUT) :: INOUTLOGICAL

        INOUTINT = INOUTINT * 2
        INOUTREAL = INOUTREAL / 2
        INOUTLOGICAL = .NOT. INOUTLOGICAL
    END SUBROUTINE

    ! Test arrays as in, out and inout. Needs to have fixed sizes overwritten in wrapper config.
    SUBROUTINE BASIC_ARGS_ARRAY(INARRAY, OUTARRAY, INOUTARRAY)
        INTEGER, INTENT(IN) :: INARRAY(:)
        INTEGER, INTENT(OUT) :: OUTARRAY(:)
        INTEGER, INTENT(INOUT) :: INOUTARRAY(:)

        WRITE (*,*) INARRAY(:), INOUTARRAY(:)
        OUTARRAY = [4, 5, 6]
        INOUTARRAY(1) = INOUTARRAY(2)
    END SUBROUTINE

    ! Test multi-dimensional array parameters.
    SUBROUTINE BASIC_ARGS_NDARRAY(INARRAY2D, OUTARRAY3D, INOUTARRAY2D)
        INTEGER, INTENT(IN) :: INARRAY2D(2, 3)
        REAL(8), INTENT(OUT) :: OUTARRAY3D(1, 2, 3)
        INTEGER, INTENT(INOUT) :: INOUTARRAY2D(1, 2)
    END SUBROUTINE

    ! Strings in all directions.
    SUBROUTINE STRING_ARGS(INSTR, OUTSTR, INOUTSTR)
        CHARACTER(32), INTENT(IN) :: INSTR
        CHARACTER(32), INTENT(OUT) :: OUTSTR
        CHARACTER(32), INTENT(INOUT) :: INOUTSTR

        WRITE (*,*) INSTR, INOUTSTR
        OUTSTR = INOUTSTR
        INOUTSTR = INSTR
    END SUBROUTINE

    ! TODO implement support for this kind of dummy arguments
    SUBROUTINE STRING_ARGS_ARRAY(INARRAY, OUTARRAY, INOUTARRAY)
        CHARACTER(32), INTENT(IN) :: INARRAY(:)
        CHARACTER(32), INTENT(OUT) :: OUTARRAY(:)
        CHARACTER(32), INTENT(INOUT) :: INOUTARRAY(:)
    END SUBROUTINE

    ! Derived types as arguments.
    SUBROUTINE DERIVED_TYPE_ARGS(INTYPE, OUTTYPE, INOUTTYPE)
        TYPE(BASIC_TYPE), INTENT(IN) :: INTYPE
        TYPE(BASIC_TYPE), INTENT(OUT) :: OUTTYPE
        TYPE(BASIC_TYPE), INTENT(INOUT) :: INOUTTYPE

        INOUTTYPE%INTFIELD = INTYPE%INTFIELD
        OUTTYPE%REALFIELD = INOUTTYPE%REALFIELD
    END SUBROUTINE

    ! Supported type return value.
    FUNCTION BASIC_RETURN_VALUE()
        INTEGER :: BASIC_RETURN_VALUE

        BASIC_RETURN_VALUE = 123
    END FUNCTION

    ! Derived type return value.
    FUNCTION DERIVED_TYPE_RETURN_VALUE()
        TYPE(BASIC_TYPE) :: DERIVED_TYPE_RETURN_VALUE
        TYPE(BASIC_TYPE), ALLOCATABLE :: DERIVED_TYPE_INTERN

        ALLOCATE(DERIVED_TYPE_INTERN)
        DERIVED_TYPE_INTERN%REALARRAY = [1.2, 3.4]
        DERIVED_TYPE_RETURN_VALUE = DERIVED_TYPE_INTERN
    END FUNCTION

    ! String as return value.
    FUNCTION STRING_RETURN_VALUE()
        CHARACTER(32) :: STRING_RETURN_VALUE

        STRING_RETURN_VALUE = "Foo Bar"
    END FUNCTION

    ! Array as return value.
    FUNCTION ARRAY_RETURN_VALUE()
        INTEGER, DIMENSION(3) :: ARRAY_RETURN_VALUE

        ARRAY_RETURN_VALUE = [1, 2, 3]
    END FUNCTION

END
