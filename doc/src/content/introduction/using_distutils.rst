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


Using **F2x** from :code:`setup.py`
===================================

**F2x** comes with a very good support for `distutils` based on the great work by `numpy <http://numpy.org>`.
Thanks to own implementations of :py:class:`Extension <F2x.distutils.extension.Extension>`,
:py:class:`build_src <F2x.distutils.command_new.build_src.build_src>`, and
:py:class:`build_ext <F2x.distutils.command_new.build_ext.build_ext>`.

To take advantage of the adopted build processes, you simply need to use the correct imports:

.. code:: python

    from F2x.distutils import setup, Extension

There is one additional required parameter to be added to your
:py:class:`Extension <F2x.distutils.extension.Extension>`. You need to provide a wrapping strategy for your extension.
The following strategies are already available:

.. f2x:strategysummary::

   lib
   lib_noerr

The chapter :doc:`../user_manual/strategies` explains build strategies in more detail.


Including own templates
-----------------------

You can use your own templates by adding them to the template registry. Your template needs to be contained in a
:doc:`F2x template package <../user_manual/templates>`. Then you can simply add that package to the registry:

.. code:: python

    from F2x.template import register_template
    import my_template_package

    register_template(my_template_package)

The allows to reference the template by its name from a custom strategies or the
:py:class:`Extensions <F2x.distutils.extension.Extension>` definitions using the
:py:attr:`templates <F2x.distutils.extension.Extension.templates>` attribute.
