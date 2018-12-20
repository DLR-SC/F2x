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
"""
This module controls the built-in build strategies. It provides a registry that can be used to add and retrieve
custom build strategies.

.. seealso::

    :py:class:`F2x.distutils.strategy.base.BuildStrategy`
        The documentation of :py:class:`F2x.distutils.strategy.base.BuildStrategy` contains details about the build
        process and how to modify it with own build strategies.

    :py:class:`F2x.distutils.strategy.library.ExtensionLibBuildStrategy`
        A build strategy to create Python extensions that need to load a library with the compiled wrapper code (like
        the :py:mod:`F2x.template.ctypes` template).

    :py:class:`F2x.distutils.strategy.extension.ExtensionBuildStrategy`
        A build strategy to create Python C extensions that contain the wrapper code in a loadable module.
"""
from F2x.distutils.strategy.library import ExtensionLibBuildStrategy
from F2x.distutils.strategy.base import BuildStrategy

_strategies = {
    'lib': ExtensionLibBuildStrategy(['bindc', 'cerr', 'ctypes']),
    'lib_noerr': ExtensionLibBuildStrategy(['bindc_new', 'ctypes_new']),
    'sphinx_docs': BuildStrategy(['sphinx']),
}


get_strategy = _strategies.get


def register_strategy(name, strategy):
    """
    Add a new strategy to the registry. If a strategy with the same name is already registered, it will be overwritten.

    :param name: Name for the new strategy.
    :param strategy: An instance of a subclass of :py:class:`BuildStrategy`.
    """
    _strategies[name] = strategy


def show_strategies():
    pass
