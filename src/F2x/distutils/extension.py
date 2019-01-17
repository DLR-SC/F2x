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
from numpy.distutils.extension import Extension as numpy_Extension


class Extension(numpy_Extension):
    def __init__(self, name, sources, **kwargs):
        """
        Take every parameter as keyword argument to allow easier cloning later on.

        :param name: (Full) name of the extension.
        :param sources: List of sources to build into the extension.
        :param kwargs:
           * :code:`library_name` (optional) holds the name of the library to build.
           * :code:`strategy` (optional) selects a strategy to use for build.
           * :code:`templates` (optional) contains a list of templates or names of templates to load.
           * :code:`f2x_options` (optional) is used for passing extra arguments to F2x.
           * :code:`autosplit` allows to automatically split extensions so that every extension contains only one
              wrapped Fortran module (Default: False).
           * :code:`inline_sources` instructs the build process to compile all sources together into the final extension
             (Default: True).
        """
        self._kwarg_keys = list(kwargs.keys())
        self.library_name = kwargs.pop('library_name', None)
        self.strategy = kwargs.pop('strategy', 'lib')
        self.templates = kwargs.pop('templates', [])
        self.f2x_options = kwargs.pop('f2x_options', [])
        self.autosplit = kwargs.pop('autosplit', False)
        self.inline_sources = kwargs.pop('inline_sources', True)

        super(Extension, self).__init__(name, sources, **kwargs)

        if self.strategy is None or isinstance(self.strategy, str):
            from F2x.distutils.strategy import get_strategy
            self.strategy = get_strategy(self.strategy or 'lib')

        if self.templates:
            self.templates = self.strategy.load_templates(self.templates)

        else:
            self.templates = self.strategy.templates

        self.ext_modules = []

    def clone(self, name, sources=None):
        """
        Duplicate this extension.

        The new extension will get a new name and might also get a new
        set of sources. The :py:attr:`autosplit` flag is reset to :code:`None` to avoid
        infinite splits.

        :param name: The name for the new extension.
        """
        kwargs = { attr: getattr(self, attr) for attr in self._kwarg_keys }
        kwargs.pop('autosplit', None)
        return Extension(name, sources or self.sources, **kwargs)

    def copy_to(self, other):
        for attr in self._kwarg_keys:
            setattr(other, attr, getattr(self, attr))
            other.module_dirs = (other.module_dirs or []) + self.module_dirs


if __name__ == '__main__':
    import sys

    ext = Extension('test', sys.argv[1:])
    print(ext.name, ext.module_names)
