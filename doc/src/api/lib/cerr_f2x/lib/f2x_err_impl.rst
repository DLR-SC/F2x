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


f2x_err_impl.c
==============

.. c:var:: static jmp_buf f2x_err_jmp_buf

   Holds the :code:`jmp_buf` for the current call.


.. c:var:: static bool f2x_err_active

   Indicates wheather a call is currently active.


.. c:var:: static int f2x_err_code

   Holds an error code for the last call. Use :c:func:`f2x_err_get` to read
   status and :c:func:`f2x_err_reset` to reset it.


.. c:function:: jmp_buf *f2x_prepare_jmp_buffer()

   Prepare :c:data:`f2x_err_jmp_buf`. If the buffer is already in use,
   indicate an error by setting :c:data:`f2x_err_code` to -1.

   :return: Address of :c:data:`f2x_err_jmp_buf` (or 0 if jump buffer is in use).


.. c:function:: void f2x_clear_jmp_buffer()

   Cleanup :c:data:`f2x_err_jmp_buf` after a call finished and release all resources.


.. c:function:: void f2x_err_handle(int code)

   Trigger error handler. This will set the :c:data:`f2x_err_code` to the given error
   and use :c:data:`f2x_err_jmp_buf` to stop the execution of the current call.

   :c:data:`f2x_err_jmp_buf` will be clean up for next use.

   :param code: The error code that should be set.


.. c:function:: void f2x_err_reset()

   Reset :c:data:`f2x_err_code` to 0 (no error).


.. c:function:: int f2x_err_get()

   Get current value of :c:data:`f2x_err_code`.

   :return: Value of :c:data:`f2x_err_code`.
