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
from numpy.distutils.core import numpy_cmdclass, setup

from F2x import template
from F2x.distutils.command_new import build_src, build_ext


template.init_templates()

numpy_cmdclass['build_src'] = build_src.build_src
numpy_cmdclass['build_ext'] = build_ext.build_ext
