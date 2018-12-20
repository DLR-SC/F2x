..
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


F2X_ERR
=======

.. f:module:: F2X_ERR
    :synopsis: Interface to the error handling code.


.. f:subroutine:: F2X_ERR_HANDLE(CODE)

    Set an error code and return to Python caller. The Fortran control flow is interrupted.

    :param INTEGER CODE: The error code to be set (will be included in Python exception).
