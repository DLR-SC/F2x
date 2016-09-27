! This is a simple math module. It is mainly here for testing F2x.
MODULE F2MATH

    TYPE :: CURVE
        INTEGER :: NBITS
        REAL(8) :: COEFF(4)
    END TYPE

CONTAINS

    SUBROUTINE CURVEOUT(C)
        TYPE(CURVE), INTENT(IN) :: C
        
        WRITE (*, *) C%COEFF
    END SUBROUTINE
    
    FUNCTION POWER(B, E)
        REAL(8) :: POWER
        REAL(8), INTENT(IN) :: B, E

        POWER = B ** E
    END FUNCTION

    FUNCTION GCDIV(A, B, X, Y)
        INTEGER :: GCDIV
        INTEGER, INTENT(IN) :: A, B
		INTEGER, INTENT(OUT) :: X, Y
		INTEGER :: Ai, Bi
		INTEGER :: X1, X2, Y1, Y2, Q, R
		
		Ai = A; Bi = B
		
		IF (Bi == 0) THEN
			GCDIV = Ai; X = 1; Y = 0
			RETURN
		END IF
		
		X2 = 1; X1 = 0
		Y2 = 0; Y1 = 1
		DO WHILE (Bi > 0)
			Q = Ai / Bi; R = Ai - Q * Bi
			X = X2 - Q * X1
			Y = Y2 - Q * Y1
			Ai = Bi; Bi = R
			X2 = X1; X1 = X
			Y2 = Y1; Y1 = Y
		END DO
		
		GCDIV = Ai; X = X2; Y = Y2
    END FUNCTION

    SUBROUTINE SWAP(A, B)
        INTEGER, INTENT(INOUT) :: A, B
        INTEGER :: C

        C = A
        A = B
        B = C
    END SUBROUTINE

    SUBROUTINE INFO(TEXT)
        CHARACTER(32) :: TEXT

        WRITE (*,*) TEXT
    END SUBROUTINE

    FUNCTION ABOUT()
        CHARACTER(12) :: ABOUT

        ABOUT = "Foo Bar Baz"
    END FUNCTION

    SUBROUTINE BUFFER(INPUT, TEXT)
        CHARACTER(32), INTENT(IN) :: INPUT
        CHARACTER(32), INTENT(OUT) :: TEXT

        TEXT = INPUT
    END SUBROUTINE

END
