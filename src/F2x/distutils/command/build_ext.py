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
from numpy.distutils.command.build_ext import build_ext as numpy_build_ext


class build_ext(numpy_build_ext):
    def run(self):
        super(build_ext, self).run()

    def get_ext_filename(self, ext_name):
        build_src = self.get_finalized_command('build_src')
        extension = build_src.find_extension(ext_name)
        filename = None

        if extension is not None:
            filename = extension.f2x_target.get_ext_filename(build_src, extension)

        if filename is None:
            filename = super(build_ext, self).get_ext_filename(ext_name)

        return filename

    def get_libraries(self, ext):
        libraries = super(build_ext, self).get_libraries(ext)
        libraries = ext.f2x_target.finish_libraries(ext, libraries)
        return libraries
