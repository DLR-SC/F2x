! This module is part of the F2x test suites. It provides some types and routines that reflect the supported spectrum.
MODULE SOURCE
    ! BASIC_TYPE has some different fields all of which are supported built-in types.
    TYPE, PUBLIC :: BASIC_TYPE
        INTEGER :: INTFIELD
        REAL(8) :: REALFIELD
        LOGICAL :: LOGICALFIELD

        CHARACTER(32) :: CHARFIELD

        INTEGER                        :: INTARRAY(3)
        REAL(8), ALLOCATABLE           :: REALARRAY(:)
        LOGICAL, POINTER, DIMENSION(:) :: LOGICALARRAY => NULL()
        
        CHARACTER(32), DIMENSION(:), ALLOCATABLE :: STRINGARRAY
    END TYPE

    ! COMPOUND_TYPE aggregates BASIC_TYPE in fields and arrays.
    TYPE, PUBLIC :: COMPOUND_TYPE
        TYPE(BASIC_TYPE) :: BASICFIELD
        TYPE(BASIC_TYPE), ALLOCATABLE :: ALLOCATEFIELD
        TYPE(BASIC_TYPE), POINTER :: POINTERFIELD => NULL()

        TYPE(BASIC_TYPE), ALLOCATABLE :: BASICARRAY(:)
    END TYPE
    
    TYPE(BASIC_TYPE), PUBLIC, ALLOCATABLE, TARGET :: BASICS(:)
    LOGICAL, PUBLIC :: BASICS_READY = .FALSE.

    INTERFACE
    subroutine F2x_handle_error(code) bind(C,name="f2x_err_handle")
      USE ISO_C_BINDING
      integer(C_int), value, intent(in) :: code
    end subroutine
    END Interface

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
        INTEGER, INTENT(INOUT) :: INOUTARRAY2D(2, 2)

        WRITE (*,*) INARRAY2D(1,:)
        WRITE (*,*) INARRAY2D(2,:)
        WRITE (*,*) INOUTARRAY2D(1,:)
        WRITE (*,*) INOUTARRAY2D(2,:)

        OUTARRAY3D(:,:,:) = 0.0_8
        OUTARRAY3D(1, 1, 2) = 4.3_8
        INOUTARRAY2D(1,1) = 3
        INOUTARRAY2D(1,2) = 2
    END SUBROUTINE

    ! Strings in all directions.
    SUBROUTINE STRING_ARGS(INSTR, OUTSTR, INOUTSTR)
        CHARACTER(*), INTENT(IN) :: INSTR
        CHARACTER(32), INTENT(OUT) :: OUTSTR
        CHARACTER(32), INTENT(INOUT) :: INOUTSTR

        WRITE (*,*) 'IN', LEN(INSTR), INSTR
        WRITE (*,*) 'INOUT', INOUTSTR
        OUTSTR = INOUTSTR
        INOUTSTR = INSTR
    END SUBROUTINE

    ! TODO implement support for this kind of dummy arguments
    SUBROUTINE STRING_ARGS_ARRAY(INARRAY, OUTARRAY, INOUTARRAY)
        CHARACTER(*), INTENT(IN) :: INARRAY(:)
        CHARACTER(6), INTENT(OUT), POINTER :: OUTARRAY(:)
        CHARACTER(*), INTENT(INOUT) :: INOUTARRAY(:)

        aLLOCATE(OUTARRAY(2))
        OUTARRAY = ["Hello ", "world!"]
        INOUTARRAY(1) = INARRAY(1)
        WRITE (*,*) SIZE(INARRAY), INARRAY(1)
    END SUBROUTINE

    ! Derived types as arguments.
    SUBROUTINE DERIVED_TYPE_ARGS(INTYPE, OUTTYPE, INOUTTYPE)
        TYPE(BASIC_TYPE), INTENT(IN) :: INTYPE
        TYPE(BASIC_TYPE), INTENT(OUT) :: OUTTYPE
        TYPE(BASIC_TYPE), INTENT(INOUT) :: INOUTTYPE

        WRITE (*,*) INTYPE%INTARRAY(:)

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
    SUBROUTINE STRING_RETURN_VALUE(RETURN_VALUE)
        CHARACTER*(*) :: RETURN_VALUE
        CHARACTER(LEN=29) :: VAL
        
        VAL = "Foo Bar"

        RETURN_VALUE = VAL
    END SUBROUTINE

    ! Array as return value.
    FUNCTION ARRAY_RETURN_VALUE()
        INTEGER, DIMENSION(3) :: ARRAY_RETURN_VALUE

        ARRAY_RETURN_VALUE = [1, 2, 3]
    END FUNCTION

END
