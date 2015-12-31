# F2x
A versatile, template-based FORTRAN wrapper written in Python.

## Quickstart
Installation of F2x follows the Python standard installation routine: Get the sources (e.g. using git) and run `setup.py install`. The dependencies should be installed automatically and you should be ready to go.

To generate wrapper code, you need a template that should be applied. There are some templates available that come bundled with F2x. To use them, prefix the respective template name with `@` and F2x will find them in it's package directory. For example if you wish to wrap your `module.f90` with to Python using the `BIND('C')` and `ctypes`, you should issue

	$ F2x -t @_bindc.f90.t -t @_ctypes.py.t module.f90

This will generate `module_bindc.f90` and `module_ctypes.py` alongside your `module.f90`. When compiling the FORTRAN code note that you need to include `wrap_util.f90` to your library.

For detailed information on provided templates and their inner workings, please consult the documentation.

## How does it work?
F2x differs from other FORTRAN quite a bit. The basic flow is as follows: It uses a full FORTRAN 2008 grammar (adapted from the OpenFortranProject). This is fed to PlyPlus for transforming the source code to an AST. The AST is then queried by the template to generate the code.

This design makes the whole process very flexible as you can easily replace the used grammar as well as the templates to generate your wrapper code. This is however at the cost of performance. But as it does not target at runtime code generation, this should be fine.

## Requirements and acknowledgements
F2x reuses a lot of stuff from other Open Source projects. Namely the following parts were really helpful:

* It is developed in Python (and should run with both Python 2 and 3).
* The PLY project is used as parser generator. PlyPlus does the heavy lifiting of converting ANTLR-like grammars to to PLY.
* The FORTRAN grammar that comes bundled with F2x originates from the OpenFortranProject.
* Templates are based on Jinja2.

## Legal stuff
Copyright &copy; 2015, led-inc.eu

This software is licensed under the Apache Public License 2.0. See `LICENSE` for details.
