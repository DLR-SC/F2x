# F2x

*A versatile, template-based Fortran wrapper written in Python.*

F2x is a Python tool that allows you to take your Fortran code and make it accessible from other languages
(mainly Python). Compared to the popular tool [f2py](https://docs.scipy.org/doc/numpy/f2py/) it comes
with two important differences:

* A full Fortran parser based on the work by the [OpenFortranParser](http://fortran-parser.sourceforge.net)
* A very flexible code generation backend that uses [Jinja2 templates](http://jinja.pocoo.org)


## Quickstart

[![Documentation Status](https://readthedocs.org/projects/f2x/badge/?version=latest)](https://f2x.readthedocs.io/en/latest/?badge=latest)

To install the latest version of F2x, you can use the following commands:

1. `git clone https://github.com/DLR-SC/F2x.git`
2. `cd F2x`
3. `python setup.py install --user`
   (Note the `--user` flag to allow installation without admin rights.)
4. `F2x --help`
   (Confirms installation of F2x.)

Please refer to the [documentation](https://f2x.readthedocs.io/en/latest/)
to find more infomation about [getting started](https://f2x.readthedocs.io/en/latest/content/introduction/getting_started.html).


## Requirements and acknowledgements
F2x reuses a lot of stuff from other Open Source projects. Namely the following parts were really helpful:

* It is developed in Python 3 and does currently not support Python 2 out of the box.
* The PLY project is used as parser generator. PlyPlus does the heavy lifiting of converting ANTLR-like grammars to PLY.
* The Fortran grammar that comes bundled with F2x originates from the OpenFortranProject.
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
