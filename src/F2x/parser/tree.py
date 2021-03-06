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
'''
This module contains the base classes for the *Abtract Generation Tree* that
is built by the parser form the Fortran sources.
'''

class Node(dict):
    """
    Node constructor stores local AST node in :py:attr:`_ast` and calls
    :py:meth:`_init_children()` which should be overwritten by child classes.

    This is the base class for the simplified AST that can easily be used
    in templates. It is simply a dict which stores child nodes as values.
    This allows to simply use :code:`node.child` to access the values from a
    template. E.g. to get the modules name, you can simply use

        :code:`{{ module.name }}`
    """

    def __init__(self, ast):
        self._ast = ast
        self._init_children()

    def _init_children(self):
        """ Implement this to store useful values (i.e. lists or dicts) in the properties. """
        raise NotImplementedError()

class VarDecl(Node):
    """
    A variable declaration.

    The following properties are available:

    ================ ======= ============================================================================================
    :code:`name`             The symbolic name of the variable.
    :code:`type`             The C type of this variable. This might be a basic type (REAL, INTEGER, LOGICAL) or TYPE(C)
                             for any other type like arrays, derived types or strings.
    :code:`intent`           May be :code:`'IN'`, :code:`'OUT'`, or :code:`'INOUT'`.
    :code:`getter`           This indicates whether the generated getter should be a :code:`FUNCTIN` or
                             :code:`SUBROUTINE`'.
    :code:`setter`    (opt)  This indicates whether a :code:`SUBROUTINE` should be generated as setter.
    :code:`ftype`     (opt)  The name of the derived type.
    :code:`strlen`    (opt)  The length of the string.
    :code:`kind`      (opt)  The :code:`KIND` specifier if available.
    :code:`dynamic`   (opt)  Indicates whether the variable is :code:`ALLOCATABLE` or a :code:`POINTER`.
    :code:`dims`      (opt)  For an array contains a list with the sizes per dimension.
    ================ ======= ============================================================================================
    """

    def __init__(self, ast, prefix=""):
        """ Slightly modified constructor to allow for re-use for type-specs and var-specs.

        For the grammar to work, component arrays have their own rule which divers from the
        variable array only in the prefix "component_" which can be passed in.
        """
        self._prefix = prefix
        super(VarDecl, self).__init__(ast)

    def with_intent(self, intent):
        self["intent"] = intent
        return self


class TypeDef(Node):
    pass


class SubDef(Node):
    _PREFIX = "subroutine"

    pass


class FuncDef(SubDef):
    _PREFIX = "function"

    pass


class Module(Node):
    pass
