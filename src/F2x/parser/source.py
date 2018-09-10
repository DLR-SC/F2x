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
Created on 12.02.2016

@author: meinel
'''
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
    unicode = str

import logging
import os
import re
import sys
import time

import plyplus


PREPROCESS_RULES = (
    # Disambiguate END statements by joining them into single keyword
    (u'end/function',   r'(?i)END[ \t\u000C]+FUNCTION',     r'ENDFUNCTION'),
    (u'end/interface',  r'(?i)END[ \t\u000C]+INTERFACE',    r'ENDINTERFACE'),
    (u'end/module',     r'(?i)END[ \t\u000C]+MODULE',       r'ENDMODULE'),
    (u'end/procedure',  r'(?i)END[ \t\u000C]+PROCEDURE',    r'ENDPROCEDURE'),
    (u'end/program',    r'(?i)END[ \t\u000C]+PROGRAM',      r'ENDPROGRAM'),
    (u'end/submodule',  r'(?i)END[ \t\u000C]+SUBMODULE',    r'ENDSUBMODULE'),
    (u'end/subroutine', r'(?i)END[ \t\u000C]+SUBROUTINE',   r'ENDSUBROUTINE'),
    (u'end/type',       r'(?i)END[ \t\u000C]+TYPE',         r'ENDTYPE'),

    # Remove spaces from data types
    (u'type/double precision', r'(?i)DOUBLE\s+PRECISION', r'DOUBLEPRECISION'),
    (u'type/double complex',   r'(?i)DOUBLE\s+COMPLEX',   r'DOUBLECOMPLEX'),

    # Replace (/ ... /) array constructs with [ ... ]
    (u'array/left',  r'\(/', '['),
    (u'array/right', r'/\)', ']'),

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
    
    log.debug(u"Loaded grammar in {0}.".format(timer))
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
        self.config = ConfigParser.RawConfigParser()

        if os.path.exists(self.config_filename):
            log.debug("Reading config for {0} from {1}...".format(self.filename, self.config_filename))
            self.config.read(self.config_filename)
        
        if not self.config.has_section('parser'):
            self.config.add_section('parser')
        
        for opt in ('grammar', 'output_pre', 'encoding'):
            if not self.config.has_option('parser', opt):
                self.config.set('parser', opt, unicode(getattr(args, opt)))
    
    def read(self):
        log.debug("Reading source from {0}...".format(self.filename))
        with open(self.filename, 'rb') as source_file:
            self.source = source_file.read().decode(self.config.get('parser', 'encoding'))
        self.source_lines = list(map(unicode.rstrip, self.source.split('\n')))

    def preprocess(self, rules=None):
        if self.source is None:
            self.read()
        
        log.debug("Preprocessing {0}...".format(self.filename))
        rules = rules or PREPROCESS_RULES

        # Process ignore rules from config
        lines = self.source.splitlines()
        if self.config.has_option('parser', 'ignore'):
            ignore = self.config.get('parser', 'ignore')
            if '\n' in ignore:
                ignore_lines = map(int, [line.split(';')[0].rstrip() for line in ignore.splitlines() if line])
            else:
                ignore_lines = map(int, map(unicode.strip, ignore.split(',')))

            for index in ignore_lines:
                lines[index - 1] = '!F2x/ignore ' + lines[index - 1]

        # Automatically ignore executable lines.
        # This little state machine tracks where it is:
        # st=0 -> Header (everything before CONTAINS)
        # st=1 -> Outside of FUNCTION/SUBROUTINE
        # st=2 -> In declaration part
        # st=21 -> In declaration part : TYPE specification
        # st=3 -> In execution part
        ln, st, info = 0, 0, None
        while ln < len(lines):
            line = lines[ln].strip().upper()

            if line \
            and line[0] != u'!':
                if st == 0:
                    if line == u'CONTAINS':
                        st = 1
    
                elif st == 1:
                    if u'FUNCTION' in line:
                        st = 2
                        info = u'FUNCTION'
                    elif u'SUBROUTINE' in line:
                        st = 2
                        info = u'SUBROUTINE'

                elif st == 2:
                    # TODO not very nice, only works with single space (but should be okay for now)
                    # YWA -- Interface definition is not covered
                    lineBefore = lines[ln-1].strip().upper()
                    if line.startswith(u'TYPE ::'):
                        st = 21

                    # YWA -- cover the case for continuous line
                    elif u'::' not in line and u'&' not in lineBefore :
                        st = 3
                        ln -= 1

                elif st == 21:
                    if line.startswith(u'END') \
                    and line.endswith(u'TYPE'):
                        st = 2

                elif st == 3:
                    if u'END' in line \
                    and info in line:
                        st = 1
    
                    elif line \
                    and line[0] != u'!':
                        lines[ln] = u'!F2x-exe' + lines[ln]
    
            # Make sure get the whole statement with continuation
            if st != 3:
                while ln < len(lines) \
                and lines[ln].strip().endswith('&'):
                    ln += 1
            
            ln +=1


        # Replace lines.
        if self.config.has_section('replace'):
            for index in self.config.options('replace'):
                lines[int(index) - 1] = self.config.get('replace', index)
                self.source_lines[int(index)-1] = lines[int(index)-1]
            
        self.source = u'\n'.join(lines)

        # Apply preprocessing rules.
        for name, pattern, repl in rules:
            self.source, count = re.subn(pattern, repl, self.source)
            if count:
                log.debug("* {0} applied {1} times.".format(name, count))

        # Ensure source ends with newline.
        if not self.source.endswith('\n'):
            self.source += '\n'

        if self.config.getboolean('parser', 'output_pre'):
            pre_source_filename = self.filename + '.pre'
            log.info("Writing preprocessed source to {0}...".format(pre_source_filename))
            with open(pre_source_filename, 'wb') as pre_source_file:
                pre_source_file.write(self.source.encode(self.config.get('parser', 'encoding')))

        self.pre_source_lines = list(map(unicode.rstrip, self.source.split('\n')))
    
    def parse(self):
        raise NotImplementedError()

    def get_gtree(self):
        raise NotImplementedError()
