# -*- coding: utf-8 -*-
#
# Copyright 2019 German Aerospace Center (DLR)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
F2x command line util.

This provides the main entry point for running F2x from command line using the provided `F2x` command. It comes with
the following sub-commands:

* `generate` - Code generation based on Fortran source code and a set of templates.
* `wrap` - Fully automated wrapping, compiling the wrapped files using `numpy` toolchain (`distutils`).
"""
import sys

from F2x import command


def main(argv=None):
    argp = command.make_argp()
    argv = argv or sys.argv[1:]
    args = argp.parse_args(argv)

    command.init_logging(args)

    cmd = command.prepare(args)
    sys.exit(cmd.run())


if __name__ == '__main__':
    main()
