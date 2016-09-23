! This is a simple math module. It is mainly here for testing F2x.
MODULE F2MATH

CONTAINS

    FUNCTION POWER(B, E)
        REAL(8) :: POWER
        REAL(8), INTENT(IN) :: B, E

        POWER = B ** E
    END FUNCTION

    FUNCTION DIVIDE(P, Q, R)
        INTEGER :: DIVIDE
        INTEGER, INTENT(IN) :: P
        INTEGER, INTENT(INOUT) :: Q
        INTEGER, INTENT(OUT) :: R
        INTEGER :: M

		R = P
        DO
            M = MOD(R, Q)
            IF (M == 0) EXIT
            R = Q
            Q = M
        END DO

        DIVIDE = Q
    END FUNCTION

END
