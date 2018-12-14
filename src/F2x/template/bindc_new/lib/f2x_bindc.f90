! F2x BIND(C) utility library.
!
! Copyright 2018 German Aerospace Center (DLR)
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
MODULE F2X_BINDC
    USE, INTRINSIC :: ISO_C_BINDING

    IMPLICIT NONE

    PUBLIC F2X_SET_ERROR
    PUBLIC F2X_GET_ERROR
    PUBLIC F2X_GET_ERROR_MESSAGE

    INTEGER, PUBLIC, PARAMETER :: F2X_NO_ERROR = 0
    INTEGER, PUBLIC, PARAMETER :: F2X_ERROR_MESSAGE_LENGTH = 60

    INTEGER :: F2X_ERROR_CODE = F2X_NO_ERROR
    CHARACTER(F2X_ERROR_MESSAGE_LENGTH) :: F2X_ERROR_MESSAGE = ""

CONTAINS

    SUBROUTINE F2X_SET_ERROR(ERROR_CODE, ERROR_MESSAGE)
        INTEGER, INTENT(IN) :: ERROR_CODE
        CHARACTER(*), INTENT(IN), OPTIONAL :: ERROR_MESSAGE

        F2X_ERROR_CODE = ERROR_CODE
        IF (PRESENT(ERROR_MESSAGE)) THEN
            F2X_ERROR_MESSAGE = ERROR_MESSAGE
        ELSE
            F2X_ERROR_MESSAGE = ""
        END IF
    END SUBROUTINE F2X_SET_ERROR

    FUNCTION F2X_GET_ERROR() RESULT(ERROR_CODE)
        INTEGER :: ERROR_CODE

        ERROR_CODE = F2X_ERROR_CODE
    END FUNCTION F2X_GET_ERROR

    FUNCTION F2X_GET_ERROR_MESSAGE() RESULT(ERROR_MESSAGE)
        CHARACTER(F2X_ERROR_MESSAGE_LENGTH) :: ERROR_MESSAGE

        ERROR_MESSAGE = F2X_ERROR_MESSAGE
    END FUNCTION F2X_GET_ERROR_MESSAGE

END MODULE F2X_BINDC