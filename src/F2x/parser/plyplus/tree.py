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
Created on 08.04.2016

@author: meinel
'''
from F2x.parser import tree


class VarDecl(tree.VarDecl):
    """
    A variable declaration.

    The following properties are available:

    - name: The symbolic name of the variable.
    - type: The C type of this variable. This might be a basic type (REAL, INTEGER, LOGICAL) or TYPE(C) for any
            other type like arrays, derived types or strings.
    - pytype, cstype: The type to be used by Python or C# respectively.
    - intent: May be 'IN', 'OUT' or 'INOUT'.
    - getter: This indicates whether the generated getter should be a 'function' or 'subroutine'.
    - setter (opt): This indicates whether a 'subroutine' should be generated as setter.
    - ftype (opt): The name of the derived type.
    - strlen (opt): The length of the string.
    - kind (opt): The kind specifier if available.
    - dynamic (opt): Indicates whether the variable is 'ALLOCATABLE' or a 'POINTER'.
    - dims (opt): For an array contains a list with the sizes per dimension.
    """

    _PYTYPES = {
        "REAL": "ctypes.c_double",
        "INTEGER": "ctypes.c_int",
        "LOGICAL": "ctypes.c_bool",
        "TYPE(C_PTR)": "ctypes.c_void_p",
    }

    _CSTYPES = {
        "REAL": "Double",
        "INTEGER": "Int32",
        "LOGICAL": "Int32",
        "TYPE(C_PTR)": "IntPtr",
    }

    def _init_children(self):
        self["name"] = self._ast.select1("name").tail[0]

        # Identify FORTRAN type and store properties accordingly
        full_spec = self._ast.parent().parent()
        type_spec = full_spec.select1("declaration_type_spec")
        try:
            self["ftype"] = type_spec.select1("derived_type_spec name").tail[0]
            self["type"] = "TYPE(C_PTR)"
            self["getter"] = "function"
            self["dynamic"] = False
        except ValueError:
            try:
                self["strlen"] = int(type_spec.select1("char_selector int_literal_constant").tail[0])
                self["intent"] = "IN"
                self["type"] = "TYPE(C_PTR)"
                self["pytype"] = "ctypes.c_char_p"
                self["cstype"] = "String"
                self["getter"] = "subroutine"
                self["setter"] = "subroutine"
            except ValueError:
                try:
                    self["strlen"] = type_spec.select1("char_selector /\*/")
                    self["intent"] = "IN"
                    self["type"] = "TYPE(C_PTR)"
                    self["pytype"] = "ctypes.c_char_p"
                    self["cstype"] = "String"
                    self["getter"] = "subroutine"
                    self["setter"] = "subroutine"
                except ValueError:
                    self["type"] = type_spec.select1("intrinsic_type_kind").tail[0]
                    self["getter"] = "function"
                    self["setter"] = "subroutine"

        for attr in full_spec.select("component_attr_spec"):
            if 'ALLOCATABLE' in attr.tail:
                self["dynamic"] = 'ALLOCATABLE'
            elif 'POINTER' in attr.tail:
                self["dynamic"] = 'POINTER'

        # Identify array dimensions
        for ast in (self._ast, full_spec):
            dims = ast.select(self._prefix + "array_spec int_literal_constant")
            if dims:
                self["dims"] = [int(dim.tail[0]) for dim in dims]
                break

            dims = ast.select(self._prefix + "array_spec array_spec_element")
            if not dims:
                dims = ast.select(self._prefix + "array_spec deferred_shape_spec_list")
            if dims:
                self["dims"] = [0] * len(dims[0].tail)
                break

        if "dims" in self \
        and "strlen" not in self:
            if "setter" in self:
                del self["setter"]

        if "pytype" not in self \
        and self["type"] in self._PYTYPES:
            self["pytype"] = self._PYTYPES[self["type"]]

        if "cstype" not in self \
        and self["type"] in self._CSTYPES:
            self["cstype"] = self._CSTYPES[self["type"]]

        try:
            kind_selector = type_spec.select1("kind_selector int_literal_constant")
            self["kind"] = int(kind_selector.tail[0])
        except ValueError:
            try:
                kind_selector = type_spec.select1("kind_selector part_ref")
                self["kind"] = kind_selector.tail[0]
            except ValueError:
                pass

        try:
            intent_spec = type_spec.parent().select1("intent_spec")
            self["intent"] = intent_spec.tail[0]
        except ValueError:
            self["intent"] = 'IN'

        # No setter for PARAMETERs
        if "setter" in self \
        and len(full_spec.select("attr_spec /PARAMETER/")) > 0:
            del self["setter"]

    def with_intent(self, intent):
        self["intent"] = intent
        return self


class TypeDef(tree.TypeDef):
    def _init_children(self):
        self["name"] = self._ast.select1("derived_type_stmt name").tail[0]
        try:
            self["public"] = (self._ast.select1("access_spec").tail[0].upper() == 'PUBLIC')
        except ValueError:
            self["public"] = False

        self["fields"] = [
            VarDecl(decl, 'component_') # See documentation of VarDecl.__init__
            for decl in self._ast.select("component_decl")
        ]
        for field in self["fields"]:
            del field["intent"]


class SubDef(tree.SubDef):
    _PREFIX = "subroutine"

    def _init_children(self):
        self["name"] = self._ast.select(self._PREFIX + "_stmt name")[0].tail[0]

        # Two-stage argument extraction:
        # First, identify all variables declared and the dummy argument list.
        dummy_args = [arg.tail[0] for arg in self._ast.select("dummy_arg name")]
        var_specs = dict(
            (argdecl.select1("name").tail[0], VarDecl(argdecl))
            for argdecl in self._ast.select("entity_decl")
        )

        # Fill up self["args"] based on dummy argument list order.
        self["args"] = [var_specs[argname] for argname in dummy_args]

        return var_specs # to be re-used in child classes.

class FuncDef(SubDef):
    _PREFIX = "function"
    def _init_children(self):
        var_specs = super(FuncDef, self)._init_children()

        # Capture return type of function for return value.
        res_name = self._ast.select("result_name name")
        if res_name:
            self["ret"] = var_specs[res_name[0].tail[0]]
        else:
            try:
                self["ret"] = var_specs[self["name"] + "_VALUE"]
            except KeyError:
                self["ret"] = var_specs[self["name"]]


class Module(tree.Module):
    def _init_children(self):
        self["name"] = self._ast.select1("module_stmt name").tail[0]
        self["uses"] = [use.tail[0] for use in self._ast.select("use_stmt name")]
        self["types"] = [
            TypeDef(typedef)
            for typedef in self._ast.select("derived_type_def")
        ]
        self["globals"] = [
            VarDecl(var)
            for var in self._ast.select("module > specification_part type_declaration_stmt entity_decl")
            if len(var.parent().parent().select("access_spec /PUBLIC/")) > 0
        ]


#    def export_methods(self, config):
    def export_methods(self, src):
        config = src.config
        if not config.has_section("export"):
            return
            
        methods = []
        for export in config.items("export"):
            for funcdef in self._ast.select("function_subprogram") :
                if funcdef.select("function_stmt name")[0].tail[0].lower() == export[0] :
                   method = FuncDef(funcdef)
                   method["export_name"] = config.get("export", method["name"].lower())
                   if "ret" in method:
                       if "dims" in method["ret"]:
                            l_line = [line for line in src.source_lines if method["ret"]["name"] in line and "ALLOCATE" in line]
                            if len(l_line) == 1:
                                #ok, it is a dynamic array, find the size variable of the array
                                l_aux_line = l_line[0][l_line[0].find(method["ret"]["name"]):-2]
                                l_size_var = l_aux_line[len(method["ret"]["name"])+1:-1].split(',')
                                method["ret"]["dims"] = l_size_var
                       if method["ret"]["getter"] == "subroutine":
                            method["ret"]["name"] = method["export_name"].upper() + '_OUT'
                            method["ret"]["intent"] = "OUT"
                       else:
                           method["ret"]["name"] = method["export_name"].upper()
                           del method["ret"]["intent"]
                   methods.append(method)
                   break
            else :
                for subdef in self._ast.select("subroutine_subprogram") :
                    if subdef.select("subroutine_stmt name")[0].tail[0].lower() == export[0] :
                        method = SubDef(subdef)
                        method["export_name"] = config.get("export", method["name"].lower())
                        l_array_args = [ l_arg for l_arg in method["args"] if "dims" in l_arg ]
                        if len(l_array_args) > 0:
                            #okay, we have arguments of array type
                            sub_start, sub_end = self._get_subroutine(method["name"], src.source_lines)
                            for arg in l_array_args:
                                self._set_array_size(arg, src.source_lines[sub_start: sub_end])

                        if "ret" in method:
                            method["ret"]["name"] = method["export_name"].upper() + '_OUT'
                            method["ret"]["intent"] = "OUT"
                        methods.append(method)
                        break

        self["methods"] = methods

        for method in methods:
            section_key = "{0}:Cleanup".format(method["name"])

            if config.has_section(section_key):
                if "ret" in method and config.has_option(section_key, method["ret"]["name"]):
                    method["ret"]["free"] = config.get(section_key, method["ret"]["name"])

                for var in method["args"]:
                    if config.has_option(section_key, var["name"]):
                        var["free"] = config.get(section_key, var["name"])

    def _set_array_size(self, a_argument, a_src):
        l_arg = a_argument["name"]
        l_arg_len = len(l_arg)
        l_key_len = 8  # keyword "ALLOCATE"

        for index, line in enumerate(a_src) :
            # to do: skip the comments
            l_line = line[line.find("::")+2 : ].strip()

            # this is the declaration line
            if l_line.startswith(l_arg+'(') :
                l_declare = l_line.split('!')
                l_array_var = l_declare[0].strip()
                l_size_var = l_array_var[l_arg_len+1:-1].split(',')
                if  l_size_var[0] == ':':
                   # check if the array is dynamically allocated within the function/subroutine body 
                   for line in a_src[index:] :
                       line = line.strip()
                       if line.startswith("ALLOCATE") :
                           # skip comment
                           l_alloc = line.split('!')[0].strip()
                           l_line = l_alloc[l_key_len:].strip()[1:-1]
                           l_alloc_list = l_line.split('),')
                           # check if more than one variables are allocated
                           if len(l_alloc_list) > 1 :
                               for l_alloc in l_alloc_list :
                                   l_alloc = l_alloc.strip()
                                   if l_alloc.startswith(l_arg + '(') :
                                       l_aux_line = ''
                                       if l_alloc.endswith(')') :
                                            l_aux_line = l_alloc[l_arg_len+1:-1].strip()
                                       else :
                                            l_aux_line = l_alloc[l_arg_len+1:].strip()
                                       l_size_var = l_aux_line.split(',')
                                       a_argument["dims"] = l_size_var
                                       break
                           else :
                               l_alloc = l_alloc_list[0].strip()
                               if l_alloc.startswith(l_arg + '(') :
                                    l_aux_line = l_alloc[l_arg_len+1:-1].strip()
                                    l_size_var = l_aux_line.split(',')
                                    a_argument["dims"] = l_size_var
                   else :
                        # okay, no size variable is found. It could be "IN" or "INOUT" type, 
                        if len(l_declare) == 2 :
                            l_comment = l_declare[1].strip()
                            l_f2x_markup='@F2x=>'
                            if l_comment.startswith(l_f2x_markup) :
                                l_vars = l_comment.split(l_f2x_markup+l_arg)[1]
                                l_size_var = l_vars[1:-1].split(',')
                                a_argument["dims"] = l_size_var
                            else :
                                # Attention: no information is provided, code is not reliable !!
                                # But at leaset make sure the dimension is correctly set
                                n = len(l_size_var)
                                a_argument["dims"] = [ x.replace(':', '0') for x in l_size_var ] 
                        else :
                            # Same problem as above !! 
                            n = len(l_size_var)
                            a_argument["dims"] = [ x.replace(':', '0') for x in l_size_var ] 
                else :
                    # size variables are set explicitly
                    a_argument["dims"] = l_size_var
                break



    def _get_subroutine(self,a_argument, a_src):
        startIndex = 0
        stopIndex =0
        for i in range(len(a_src)):
            l_str = a_src[i].strip()
            if l_str.startswith("SUBROUTINE") and a_argument in l_str :
                startIndex = i
                for j, line in enumerate(a_src[i:]):
                    line = line.strip()
                    if line.startswith("END SUBROUTINE") :
                        stopIndex = i + j
                        break
                break
        else:
            # should not happend
            pass

        return (startIndex, stopIndex)






