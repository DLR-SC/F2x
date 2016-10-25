'''
Created on 12.02.2016

@author: meinel
'''
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser

import logging
import os
import re
import sys
import time

import plyplus


PREPROCESS_RULES = (
    # Disambiguate END statements by joining them into single keyword
    (u'end/associate',  r'(?i)END[ \t\u000C]+ASSOCIATE',    r'ENDASSOCIATE'),
    (u'end/block',      r'(?i)END[ \t\u000C]+BLOCK',        r'ENDBLOCK'),
    (u'end/block data', r'(?i)END[ \t\u000C]+BLOCK\s*DATA', r'ENDBLOCKDATA'),
    (u'end/critical',   r'(?i)END[ \t\u000C]+CRITICAL',     r'ENDCRITICAL'),
    (u'end/do',         r'(?i)END[ \t\u000C]+DO',           r'ENDDO'),
    (u'end/enum',       r'(?i)END[ \t\u000C]+ENUM',         r'ENDENUM'),
    (u'end/file',       r'(?i)END[ \t\u000C]+FILE',         r'ENDFILE'),
    (u'end/for',        r'(?i)END[ \t\u000C]+FOR\s*ALL',    r'ENDFORALL'),
    (u'end/function',   r'(?i)END[ \t\u000C]+FUNCTION',     r'ENDFUNCTION'),
    (u'end/if',         r'(?i)END[ \t\u000C]+IF',           r'ENDIF'),
    (u'end/interface',  r'(?i)END[ \t\u000C]+INTERFACE',    r'ENDINTERFACE'),
    (u'end/module',     r'(?i)END[ \t\u000C]+MODULE',       r'ENDMODULE'),
    (u'end/procedure',  r'(?i)END[ \t\u000C]+PROCEDURE',    r'ENDPROCEDURE'),
    (u'end/program',    r'(?i)END[ \t\u000C]+PROGRAM',      r'ENDPROGRAM'),
    (u'end/select',     r'(?i)END[ \t\u000C]+SELECT',       r'ENDSELECT'),
    (u'end/submodule',  r'(?i)END[ \t\u000C]+SUBMODULE',    r'ENDSUBMODULE'),
    (u'end/subroutine', r'(?i)END[ \t\u000C]+SUBROUTINE',   r'ENDSUBROUTINE'),
    (u'end/type',       r'(?i)END[ \t\u000C]+TYPE',         r'ENDTYPE'),
    (u'end/where',      r'(?i)END[ \t\u000C]+WHERE',        r'ENDWHERE'),
    
    # Remove spaces from data types
    (u'type/double precision', r'(?i)DOUBLE\s+PRECISION', r'DOUBLEPRECISION'),
    (u'type/double complex',   r'(?i)DOUBLE\s+COMPLEX',   r'DOUBLECOMPLEX'),
    
    # Remove empty function arguments
    (u'empty args',
        r'([(.=]\s*([a-zA-Z][a-zA-Z0-9_]*)(%[a-zA-Z][a-zA-Z0-9_]*)*)\(\s*\)',
        r'\1'),

    # Replace (/ ... /) array constructs with [ ... ]
    (u'array/left',  r'\(/', '['),
    (u'array/right', r'/\)', ']'),
                    
    # DDT in Array
    (u'array/ddt', r'\)(%[a-zA-Z0-9_]+)+', ')'),
    
    # Remove substring range on LHS comparison
    (u'substring/lhs',
        r'([\n\(,]\s*[a-zA-Z][a-zA-Z0-9_]*)\((\d+|[a-zA-Z][a-zA-Z0-9_]*)([:,](\d+|[a-zA-Z][a-zA-Z0-9_]*))*\)(\s*=)',
        r'\1\5'),
    
    # Convert substring range to arguments on RHS
    #(u'substring/rhs',
    #    r'\(\s*(\d+|[a-zA-Z][a-zA-Z0-9_]*)(\s*[:,]\s*(\d+|[a-zA-Z][a-zA-Z0-9_]*))+\s*\)(?!\s*=)',
    #    r'(\1,\3)'),
    (u'substring/rhs',
        r'\(\s*(\d+|[a-zA-Z][a-zA-Z0-9_]*)(\s*[:]\s*(\d+|[a-zA-Z][a-zA-Z0-9_]*)(,(\d+|[a-zA-Z][a-zA-Z0-9_]*))?)+\s*\)(?!\s*=)',
        r'(\1,\3)'),
    
    # Prefix Type-Casts
    (u'cast', r'(?i)([-+*/(.=][ \t\u000C]*REAL)\s*\(', r'\1_CAST('),
    
    # Kind of numeric constant
    (u'kind', r'(\d+)_(\d+|LF)', r'\1'),
)


log = logging.getLogger(__name__)
grammar_cache = {}
package_path, _ = os.path.split(__file__)


def load_grammar(grammar_filename):
    if grammar_filename[0] == u'@':
        # Replace '@'-prefix with path to F2x.grammar.
        grammar_filename = os.path.join(package_path, u'grammar', grammar_filename[1:])

    # Need to adjust recursion limit as plyplus is very recusive and FORTRAN grammars are complex.
    old_recursionlimit = sys.getrecursionlimit()
    sys.setrecursionlimit(3000)

    log.info(u"Loading grammar from {0}. This may take some time...".format(grammar_filename))

    start = time.time()
    grammar_file = open(grammar_filename, 'r')
    grammar = plyplus.Grammar(grammar_file)
    grammar_cache[grammar_filename] = grammar
    timer = time.time() - start
    
    log.debug(u"* Loaded grammar in {0}.".format(timer))
    sys.setrecursionlimit(old_recursionlimit)

    return grammar


class SourceFile(object):
    def __init__(self, filename, args):
        self.source = None
        self.source_lines = None
        self.pre_source_lines = None
        self.tree = None

        self.filename = filename
        self.config_filename = filename + args.config_suffix
        self.config = ConfigParser.ConfigParser()

        if os.path.exists(self.config_filename):
            log.debug("Reading config for {0} from {1}...".format(self.filename, self.config_filename))
            self.config.read(self.config_filename)
        
        if not self.config.has_section('parser'):
            self.config.add_section('parser')
        
        for opt in ('grammar', 'output_pre', 'encoding'):
            if not self.config.has_option('parser', opt):
                self.config.set('parser', opt, str(getattr(args, opt)))
    
    def read(self):
        log.debug("Reading source from {0}...".format(self.filename))
        source_file = open(self.filename, 'rb')
        self.source = source_file.read().decode(self.config.get('parser', 'encoding'))
        self.source_lines = map(str.rstrip, self.source.split('\n'))
    
    def preprocess(self, rules=None):
        if self.source is None:
            self.read()
        
        log.debug("Preprocessing {0}...".format(self.filename))
        rules = rules or PREPROCESS_RULES
        
        if self.config.has_option('parser', 'ignore'):
            ignore = self.config.get('parser', 'ignore')
            if '\n' in ignore:
                ignore_lines = map(int, [line.split(';')[0].rstrip() for line in ignore.splitlines() if line])
            else:
                ignore_lines = map(int, map(str.strip, ignore.split(',')))

            lines = map(str.rstrip, self.source.split('\n'))
            for index in ignore_lines:
                lines[index - 1] = '!F2x/ignore ' + lines[index - 1]
            
            self.source = '\n'.join(lines)
        
        if self.config.has_section('replace'):
            lines = map(str.rstrip, self.source.split('\n'))
            for index in self.config.options('replace'):
                lines[int(index) - 1] = self.config.get('replace', index)
            
            self.source = '\n'.join(lines)
        
        for name, pattern, repl in rules:
            self.source, count = re.subn(pattern, repl, self.source)
            if count:
                log.debug("* {0} applied {1} times.".format(name, count))
        
        if not self.source.endswith('\n'):
            self.source += '\n'
        
        if self.config.getboolean('parser', 'output_pre'):
            pre_source_filename = self.filename + '.pre'
            log.info("Writing preprocessed source to {0}...".format(pre_source_filename))
            open(pre_source_filename, 'wb').write(self.source.encode(self.config.get('parser', 'encoding')))

        self.pre_source_lines = map(str.rstrip, self.source.split('\n'))
    
    def parse(self):
        grammar_filename = self.config.get('parser', 'grammar')

        if grammar_filename in grammar_cache:
            grammar = grammar_cache[grammar_filename]
        else:
            grammar = load_grammar(grammar_filename)

        self.tree = grammar.parse(self.source)
