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


Build Strategies
================

F2x extends the numpy build system using strategies that interact with the build environment at defined points during
the build process. This allows to decouple specific tweaks that are done from the overall build process. In general,
every set of templates needs a specific build strategy. In return, every build strategy usally has it's specific set of
templates that it needs to successfully build a Python extension.

The base :py:class:`BuildStrategy` defines the seqence when it can interact with the build process.


Available Strategies
--------------------

A set of strategies come bundled with F2x:

.. f2x:strategysummary::

   lib
   lib_noerr


.. _man-error-handling:

Error Handling
--------------

Ususally, error handling should be done using return values etc. However, in many cases Fortran programs seem to simply
:code:`STOP` running if they face a condition. This is a big show stopper for Python as the whole process will be
killed. To accomodate this problem, F2x implements a thin C wrapper using :py:mod:`F2x.template.cerr` that qualifies as
:code:`longjmp` traget. This allows us to replace all the calls to :code:`STOP` by a calls to
:ref:`F2X_HANDLE_ERROR`. This triggers the :code:`longjmp` and program flow returns to Python.

To support this method, you need to make sure to use the correct strategy or templates:

Strategies with error handling:

.. f2x:strategysummary::

   lib

Templates with error handling:

.. f2x:templatesummary::

   F2x.template.bindc
   F2x.template.cerr
   F2x.template.ctypes
