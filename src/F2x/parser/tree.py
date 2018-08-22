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
