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
    _PYTYPES = {
        "REAL": "ctypes.c_double",
        "INTEGER": "ctypes.c_int",
        "LOGICAL": "ctypes.c_bool",
        "TYPE(C_PTR)": "ctypes.c_void_p",
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
        type_spec = self._ast.parent().parent().select1("declaration_type_spec")
        try:
            self["ftype"] = type_spec.select1("derived_type_spec name").tail[0]
            self["type"] = "TYPE(C_PTR)"
            self["getter"] = "function"
        except ValueError:
            try:
                self["strlen"] = int(type_spec.select1("char_selector int_literal_constant").tail[0])
                self["intent"] = "IN"
                self["type"] = "TYPE(C_PTR)"
                self["getter"] = "subroutine"
                self["setter"] = "subroutine"
            except ValueError:
                self["type"] = type_spec.select1("intrinsic_type_kind").tail[0]
                self["getter"] = "function"
                self["setter"] = "subroutine"
                
        # Identify additional modifiers (i.e. arrays etc.)
        dims = self._ast.select(self._prefix + "array_spec int_literal_constant")
        if dims:
            self["dims"] = [int(dim.tail[0]) for dim in dims]
            self["getter"] = "subroutine"
            if "setter" in self:
                del self["setter"]
        
        if self["type"] in self._PYTYPES:
            self["pytype"] = self._PYTYPES[self["type"]]
        
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
            pass

class TypeDef(Node):
    def _init_children(self):
        self["name"] = self._ast.select1("derived_type_stmt name").tail[0]
        self["fields"] = [
            VarDecl(decl, 'component_') # See documentation of VarDecl.__init__
            for decl in self._ast.select("component_decl")
        ]

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
        
#        import pprint
#        pp = pprint.PrettyPrinter(indent=4)
#        pp.pprint(self)

