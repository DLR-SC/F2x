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
MODULE F2X_ERR

    INTERFACE
        SUBROUTINE F2X_ERR_HANDLE(CODE) BIND(C, name="f2x_err_handle")
            USE, INTRINSIC :: ISO_C_BINDING
            INTEGER, INTENT(IN), VALUE :: CODE
        END SUBROUTINE F2X_ERR_HANDLE
    END INTERFACE

END MODULE F2X_ERR