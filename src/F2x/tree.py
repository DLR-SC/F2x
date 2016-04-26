'''
Created on 08.04.2016

@author: meinel
'''

class Node(dict):
    MAPPING = dict()
    
    def __init__(self, node):
        super(Node, self).__init__()
        self._node = node
        self._init_children()
    
    def _init_children(self):
        for key, (sel, coll, cls) in self.MAPPING.iteritems():
            items = self._node.select(sel)
            if coll:
                self[key] = map(cls, items)
            else:
                self[key] = cls(items[0])

def tail(item):
    return item.tail[0]

class VarDef(Node):
    MAPPING = {
        u'name': (u'entity_decl name', False, tail),
    }
    
    PYTYPE = {
        u'INTEGER': u'ctypes.c_int',
        u'REAL': u'ctypes.c_double',
        u'LOGICAL': u'ctypes.c_bool',
    }

    def _init_children(self):
        super(VarDef, self)._init_children()
        
        dims = self._node.select(u"array_spec int_literal_constant") 
        if dims:
            self[u'dims'] = map(tail, dims)
    
        intent = self._node.select(u'intent_spec')
        if intent:
            self[u'intent'] = tail(intent[0])
        else:
            self[u'intent'] = u'IN'

        if self._node.select(u'intrinsic_type_char'):
            self[u'type'] = u'TYPE(C_PTR)'
            self[u'pytype'] = u'ctypes.c_char_p'
            self[u'getter'] = u'subroutine'
            self[u'setter'] = True
            try:
                self[u'strlen'] = int(tail(self._node.select(u'char_selector int_literal_constant')[0]))
            except IndexError:
                self[u'strlen'] = u'*'
        else:
            items = self._node.select(u'intrinsic_type_kind') or self._node.select(u'intrinsic_type_spec') 
            if items:
                self[u'type'] = tail(items[0])
                self[u'pytype'] = VarDef.PYTYPE[self[u'type']]
                self[u'getter'] = u'subroutine' if dims else u'function'
                self[u'setter'] = True
                items = self._node.select(u'kind_selector part_ref')
                if items:
                    self[u'type'] += u'(KIND={0})'.format(tail(items[0]))
            else:
                items = self._node.select(u'derived_type_spec name')
                if items:
                    self[u'type'] = u'TYPE(C_PTR)'
                    self[u'getter'] = u'function'
                    self[u'setter'] = False
                    self[u'ftype'] = tail(items[0])
        
class TypeDefField(VarDef):
    MAPPING = {
        u'name': (u'component_decl name', False, tail),
    }

class TypeDef(Node):
    MAPPING = {
        u'name': (u'derived_type_stmt name', False, tail),
        u'fields': (u'component_def_stmt', True, TypeDefField),
    }

class Subroutine(Node):
    MAPPING = {
        u'name': (u'subroutine_stmt name', False, tail),
    }
    
    def _init_children(self):
        super(Subroutine, self)._init_children()
        
        arg_names = map(tail, self._node.select(u'dummy_arg name'))
        self[u'args'] = [
            arg
            for arg in map(VarDef, self._node.select(u'type_declaration_stmt'))
            if arg[u'name'].upper() in arg_names
        ]

class Function(Subroutine):
    MAPPING = {
        u'name': (u'function_stmt name', False, tail),
    }
    
    def _init_children(self):
        super(Function, self)._init_children()
        
        args = map(VarDef, self._node.select(u'type_declaration_stmt'))
        for var in args:
            if var[u'name'] in (self[u'name'], self[u'name'] + u'_VALUE'):
                self[u'ret'] = var
                break

class Module(Node):
    MAPPING = {
        u'name': (u'module_stmt name', False, tail),
        u'uses': (u'use_stmt name', True, tail),
        u'types': (u'derived_type_def', True, TypeDef),
        u'functions': (u'function_subprogram', True, Function),
        u'subroutines': (u'subroutine_subprogram', True, Subroutine),
    }
