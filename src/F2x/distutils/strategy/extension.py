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
import re

from numpy.distutils import log
from numpy.distutils.misc_util import fortran_ext_match, cxx_ext_match

from F2x import template
from F2x.distutils.strategy.base import BuildStrategy

c_ext_match = re.compile(r'.*[.](c)', re.IGNORECASE).match
ext_matches = cxx_ext_match, fortran_ext_match, c_ext_match


class ExtensionBuildStrategy(BuildStrategy):
    pass
