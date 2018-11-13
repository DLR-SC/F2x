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
from F2x.distutils.strategy.library import ExtensionLibBuildStrategy
from F2x.distutils.strategy.base import BuildStrategy

builtin_strategies = {
    'lib': ExtensionLibBuildStrategy(['bindc', 'ctypes']),
    'lib_err': ExtensionLibBuildStrategy(['bindc', 'cerr', 'ctypes_err']),
}


def register_strategy(name, strategy):
    builtin_strategies[name] = strategy


def get_target_strategy(target_name):
    if target_name in builtin_strategies:
        return builtin_strategies[target_name]

    return BuildStrategy([])