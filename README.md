# F2x
A versatile, template-based FORTRAN wrapper written in Python.

## Quickstart
Installation of F2x follows the Python standard installation routine: Get the sources (e.g. using git) and run `setup.py install`. The dependencies should be installed automatically and you should be ready to go.

To generate wrapper code, you need a template that should be applied. There are some templates available that come bundled with F2x. To use them, prefix the respective template name with `@` and F2x will find them in it's package directory. For example if you wish to wrap your `module.f90` with to Python using the `BIND('C')` and `ctypes`, you should issue

	$ F2x -t @bindc/_glue.f90.t -t @ctypes/_glue.py.t module.f90

This will generate `module_gluec.f90` and `module_glue.py` alongside your `module.f90`. When compiling the FORTRAN code note that you need to link against `libF2x.so`.

For detailed information on provided templates and their inner workings, please consult the detailed documentation.

## How does it work?
F2x differs from other FORTRAN wrappers quite a bit. The basic flow is as follows: It uses a full FORTRAN 2008 grammar (adapted from the OpenFortranProject). This is fed to PlyPlus for transforming the source code into an AST. The AST is then queried by the template to generate the code.

This design makes the whole process very flexible as you can easily replace the used grammar as well as the templates to generate your wrapper code. This is however at the cost of performance. But as it does not target runtime code generation, this should be fine.

## Requirements and acknowledgements
F2x reuses a lot of stuff from other Open Source projects. Namely the following parts were really helpful:

* It is developed in Python 3 and does currently not support Python 2 out of the box.
* The PLY project is used as parser generator. PlyPlus does the heavy lifiting of converting ANTLR-like grammars to PLY.
* The FORTRAN grammar that comes bundled with F2x originates from the OpenFortranProject.
* Templates are based on Jinja2.

## Legal stuff

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
