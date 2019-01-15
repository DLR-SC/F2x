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


Welcome to F2x Documentation!
=============================

**F2x** is a Python tool that allows you to take your Fortran code and make it accessible from other languages. Compared
to it's main competitor `f2py <https://f2py.org>`_ it comes with two important differences:

- A superior Fortran parser based on the work by the `OpenFortranParser <https://openfortranparser.github.com>`_
- A very flexible code generation backend that uses `Jinja2 <https://jinja.pocoo.net>`_ templates


Requirements
------------

F2x currently requires `Python 3 <https://python.org>`_ to work. It relies on the following pacakges available from
`PyPI <https://pypi.python.org>`_:

- `pyplus <https://...>`_
- `jinja2 <https://...>`_
- `numpy <https://...>`_
- `Cython <https://...>`_ (optional)

Additional requirements for building documentation and running tests exists. Please consult the `setup.py` for further
details. All requirements should be automatically provided during installation of **F2x**.

Quick Steps
-----------

.. toctree::
   :maxdepth: 1
   :titlesonly:

   content/introduction/getting_started
   content/introduction/using_distutils
   content/introduction/trouble_shooting


User Manual
-----------

.. toctree::
   :titlesonly:

   content/user_manual/toc
   content/advanced/toc
   content/publications

.. toctree::
    :hidden:

    full_toc


Indices and tables
------------------

- :doc:`full_toc`
- :doc:`api/F2x`
- :ref:`genindex`
- :ref:`modindex`
- :ref:`search`
