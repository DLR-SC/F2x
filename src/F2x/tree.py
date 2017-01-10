'''
Created on 08.04.2016

@author: meinel
'''

class Node(dict):
    """ This is the base class for the simplified AST that can easily be used
        in templates. It is simply a dict which stores child nodes as values.
        This allows to simply use node.child to access the values from a
        template. E.g. to get the modules name, you can simply use
        
        {{ module.name }}
    """
    
    def __init__(self, ast):
        """ Node constructor stores local AST node in self._ast and calls
            self._init_children() which should be overwritten by child classes.
        """
        self._ast = ast
        self._init_children()
    
    def _init_children(self):
        """ Implement this to store useful values (i.e. lists or dicts) in the properties. """
        raise NotImplementedError()

class VarDecl(Node):
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
     
    def __init__(self, ast, prefix=""):
        """ Slightly modified constructor to allow for re-use for type-specs and var-specs.
        
        For the grammar to work, component arrays have their own rule which divers from the
        variable array only in the prefix "component_" which can be passed in.
        """
        self._prefix = prefix
        super(VarDecl, self).__init__(ast)

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

        if "dims" in self:
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

    def with_intent(self, intent):
        self["intent"] = intent
        return self


class TypeDef(Node):
    def _init_children(self):
        self["name"] = self._ast.select1("derived_type_stmt name").tail[0]
        self["fields"] = [
            VarDecl(decl, 'component_') # See documentation of VarDecl.__init__
            for decl in self._ast.select("component_decl")
        ]
        for field in self["fields"]:
            del(field["intent"])


class SubDef(Node):
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

class Module(Node):
    def _init_children(self):
        self["name"] = self._ast.select1("module_stmt name").tail[0]
        self["uses"] = [use.tail[0] for use in self._ast.select("use_stmt name")]
        self["types"] = [
            TypeDef(typedef)
            for typedef in self._ast.select("derived_type_def")
        ]
        self["subroutines"] = [
            SubDef(subdef)
            for subdef in self._ast.select("subroutine_subprogram")
        ]
        self["functions"] = [
            FuncDef(funcdef)
            for funcdef in self._ast.select("function_subprogram")
        ]

    def export_methods(self, config):
        if not config.has_section("export"):
            return

        methods = []
        for method in self["subroutines"] + self["functions"]:
            if config.has_option("export", method["name"].lower()):
                method["export_name"] = config.get("export", method["name"].lower())
                if "ret" in method:
                    if method["ret"]["getter"] == "subroutine":
                        method["ret"]["name"] = method["export_name"].upper() + '_OUT'
                        method["ret"]["intent"] = "OUT"
                    else:
                        method["ret"]["name"] = method["export_name"].upper()
                        del method["ret"]["intent"]
                methods.append(method)

        self["methods"] = methods
