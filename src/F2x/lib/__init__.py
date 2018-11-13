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

def find_library_path(lang, item=None):
    library_dir = os.path.dirname(__file__)
    lang_dir = os.path.join(library_dir, lang)
    
    if item is not None:
        lang_lib = os.path.join(lang_dir, item)
        if os.path.exists(lang_lib):
            return lang_lib
    
    elif os.path.isdir(lang_dir):
        return lang_dir
    
    raise FileNotFoundError("Could not find {0}/{1}. Maybe you need to build it?".format(lang, item))

def get_internal_lib(rel_dir=None):
    basedir = os.path.join(os.path.dirname(__file__), 'fortran')
    return [os.path.join(basedir, file) for file in ('c_interface_module.f90', 'f2x_err.c')]
