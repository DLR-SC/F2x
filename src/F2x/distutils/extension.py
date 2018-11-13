# -*- encoding: utf-8 -*-
#
# Copyright 2018 German Aerospace Center (DLR)
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
import os

from numpy.distutils.extension import Extension as numpy_Extension
from numpy.distutils.misc_util import fortran_ext_match, f90_module_name_match

from F2x.distutils import strategy


class Extension(numpy_Extension):
    def __init__(self, name, sources,
                 library_name=None,
                 f2x_options=None,
                 f2x_target='dummy',
                 f2x_autosplit_ext=False,
                 **kwargs):
        super(Extension, self).__init__(name, sources, **kwargs)

        self.library_name = library_name
        self.f2x_options = f2x_options or []
        self.f2x_autosplit_ext = f2x_autosplit_ext

        if isinstance(f2x_target, str):
            self.f2x_target = strategy.get_target_strategy(f2x_target)
        else:
            self.f2x_target = f2x_target

        self.f2x_target.finish_extension(self)

    def find_modules(self):
        module_names = []

        for source in self.sources:
            if fortran_ext_match(os.path.splitext(source)[1]):
                module_name = self._extract_module_name(source)
                module_names.append((source, module_name))

        return module_names

    def _extract_module_name(self, source):
        with open(source, 'r') as source_file:
            line = source_file.readline()
            while line:
                match = f90_module_name_match(line)
                if match:
                    return match.group('name')
                line = source_file.readline()

        return None


if __name__ == '__main__':
    import sys

    ext = Extension('test', sys.argv[1:])
    print(ext.name, ext.module_names)
