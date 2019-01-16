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


Getting Started with F2x
========================
.. highlight:: console

This document gives a short introduction to get started working with **F2x**. More detailed information about the single
steps can be found in the respective detailed chapter.


Installation
------------

**F2x** currently depends on Python and `setuptools` in order to install it. If you have these pre-requirements in
place, you can go ahead and install **F2x** by cloning the repository and running the setup script:

.. code::

   $ git clone https://github.com/DLR-SC/F2x.git
   $ cd F2x
   $ python setup.py install

Of course you need a working :doc:`Fortran compiler </content/user_manual/compilers>`. As F2x relies on the build chain
provided by numpy, this usually mean that if you can build numpy extensions, you should also be able to build F2x
extensions.

This should install all dependencies and a command line tool :code:`F2x` to wrap your Fortran code.


One-shot building
-----------------

A :download:`tiny example </content/example/src.zip>` is available to try F2x. Extract it to some location and from the
containing folder run:

.. code::

   $ F2x -W lib mylib/test.f90

This applies a :doc:`strategy </content/user_manual/strategies>` to generate the wrapper modules and compile the extension
in one shot. You can try your results by running tests against it:

.. code::

   $ python -m mylib.test


Wrapping Fortran Sources
------------------------

If you have successfully installed **F2x**, you can use the :code:`F2x` command line tool to wrap your source files.
The general synopsis is:

.. code::

   $ F2x [-t template]... [source file]...

This will make **F2x** parse the source files and apply each template to each source file to generate the wrapper
output. A full overview of all options is available `here <command_line>`.

The following templates are the recommended choice:

.. f2x:templatesummary::

   F2x.template.bindc
   F2x.template.cerr
   F2x.template.ctypes

Alternatively, you can also use the following set of templates that generates a wrapper without
:ref:`error handling <man-error-handling>`.

.. f2x:templatesummary::

   F2x.template.bindc
   F2x.template.ctypes_noerr

Usually, you need to apply several templates to achieve full wrapping of your Fortran source (and thus make it usable
from Python). You can pass all of them at one to **F2x**.


Building the Extension
----------------------

After wrapping your Fortran sources, you need to compile the genrated sources. Some of the templates require additional
libraries to be used. To include them in compilation, you can use to following helpful commands that returns the full
pathes to all required artifacts:

.. code::

   $ export F2X_TEMPLATE_LIBS = \
       $(F2x -t bindc -t cerr -t ctypes --get libraries)
   $ export F2X_TEMPLATE_MODS = \
       $(F2x -t bindc -t cerr -t ctypes --get modules)


Example
-------

Consider the following example Fortran source:

.. literalinclude:: /content/example/mylib/test.f90
   :start-after: !-------
   :language: fortran
   :caption: :download:`mylib/test.f90 </content/example/mylib/test.f90>`
   :name: mylib/test.f90
   :linenos:

This file will be wrapped and the resulting sources are compiled to a dynamic library:

.. code::

   $ F2x -t @bindc/_glue.f90.t -t @ctypes_noerr/_glue.py.t mylib/test.f90
   $ gfortran -shared -o mylib/libTEST.so mylib/test.f90 mylib/test_glue.f90 \
       $(F2x -t bindc -t ctypes_noerr --get libraries)
   $ cp $(F2x -t bindc -t ctypes_noerr --get modules) mylib

Now you should be able to call :code:`CALL_TEST` from Python:

.. code-block:: pycon

   $ python
   >>> from mylib import test_glue as test
   >>> test.CALL_TEST(123)


Advanced Options
----------------

There are several other ways to interact with **F2x** which are described in more detail in their respective sections.

* :doc:`Using setup.py to build extensions <using_distutils>`
* :doc:`Tuning interface generation parameters for a Fortran source </content/user_manual/interface_config>`
* :doc:`Extending F2x with your own templates </content/advanced/extra_templates>`
* :doc:`Using an alternate parser for F2x </content/advanced/alternate_parser>`
