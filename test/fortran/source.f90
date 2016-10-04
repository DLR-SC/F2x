! This file is test input for F2x. Please do not misunderstand it for doing something useful ;)
MODULE SOURCE
    USE LIB1
    USE LIB2

    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: STRUCTURE
        INTEGER :: ATOM1
        REAL(KIND=8) :: ATOM2
        LOGICAL :: ATOM3, ATOM4

        CHARACTER(LEN=16) :: TEXT
        INTEGER :: ARRAY1D(10)
        REAL(KIND=8) :: ARRAY2D(5, 6)
    END TYPE

    TYPE, PUBLIC :: CONTAINER
        TYPE(STRUCTURE) :: CHILD

        CHARACTER(LEN=5) :: TEXTARRAY(3)
    END TYPE

CONTAINS

    FUNCTION FOO_BAR(BAZ, SPAM)
        INTEGER, INTENT(IN) :: BAZ, SPAM(3, 3)
        INTEGER :: FOO_BAR

        FOO_BAR = BAZ
    END FUNCTION

    SUBROUTINE BAR_FOO(SPAM, HAM, BAZ)
        INTEGER, INTENT(IN) :: SPAM(3, 3), HAM
        INTEGER, INTENT(OUT) :: BAZ

        BAZ = SPAM(HAM, HAM)
    END SUBROUTINE

END MODULE
