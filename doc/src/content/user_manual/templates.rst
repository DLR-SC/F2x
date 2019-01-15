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


Templates
=========

**F2x** uses templates to generate the code. There are a bunch of templates that come bundled with **F2x**:

.. f2x:templatesummary::
    F2x.template.bindc
    F2x.template.cerr
    F2x.template.ctypes
    F2x.template.ctypes_noerr
    F2x.template.sphinx


Choosing a Template on Command Line
===================================

When you use the :doc:`F2x CLI <command_line>` to wrap your sources, you can select templates by specifying the `-t`
switch. You have three possibilities of accessing template files:

- The name of a built-in template. These start with an '@' character and are listed in the table above.
- The full (relative) path to a template file.
- A path relative to one of the template path directories. You can adjust the template path using the `-T` switch.



