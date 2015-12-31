# -*- coding: utf-8 -*-
u""" Main program for F2x - A versatile, template based FORTRAN wrapper.
"""
from __future__ import print_function

import argparse
import logging
import os
import re
import sys
import time
import ConfigParser

import jinja2
import plyplus


VERSION = u"0.1"
DESCRIPTION = u"F2x - A versatile FORTRAN wrapper."

PREPROCESS_RULES = (
    # Disambiguate END statements by joining them into single keyword
    (u'end/associate',  r'(?i)END\s+ASSOCIATE',    r'ENDASSOCIATE'),
    (u'end/block',      r'(?i)END\s+BLOCK',        r'ENDBLOCK'),
    (u'end/block data', r'(?i)END\s+BLOCK\s*DATA', r'ENDBLOCKDATA'),
    (u'end/critical',   r'(?i)END\s+CRITICAL',     r'ENDCRITICAL'),
    (u'end/do',         r'(?i)END\s+DO',           r'ENDDO'),
    (u'end/enum',       r'(?i)END\s+ENUM',         r'ENDENUM'),
    (u'end/file',       r'(?i)END\s+FILE',         r'ENDFILE'),
    (u'end/for',        r'(?i)END\s+FOR\s*ALL',    r'ENDFORALL'),
    (u'end/function',   r'(?i)END\s+FUNCTION',     r'ENDFUNCTION'),
    (u'end/if',         r'(?i)END\s+IF',           r'ENDIF'),
    (u'end/interface',  r'(?i)END\s+INTERFACE',    r'ENDINTERFACE'),
    (u'end/module',     r'(?i)END\s+MODULE',       r'ENDMODULE'),
    (u'end/procedure',  r'(?i)END\s+PROCEDURE',    r'ENDPROCEDURE'),
    (u'end/program',    r'(?i)END\s+PROGRAM',      r'ENDPROGRAM'),
    (u'end/select',     r'(?i)END\s+SELECT',       r'ENDSELECT'),
    (u'end/submodule',  r'(?i)END\s+SUBMODULE',    r'ENDSUBMODULE'),
    (u'end/subroutine', r'(?i)END\s+SUBROUTINE',   r'ENDSUBROUTINE'),
    (u'end/type',       r'(?i)END\s+TYPE',         r'ENDTYPE'),
    (u'end/where',      r'(?i)END\s+WHERE',        r'ENDWHERE'),
    
    # Remove spaces from data types
    (u'type/double precision', r'(?i)DOUBLE\s+PRECISION', r'DOUBLEPRECISION'),
    (u'type/double complex',   r'(?i)DOUBLE\s+COMPLEX',   r'DOUBLECOMPLEX'),
    
    # Remove empty function arguments
    (u'empty args',
        r'([(.=]\s*([a-zA-Z][a-zA-Z0-9_]*)(%[a-zA-Z][a-zA-Z0-9_]*)*)\(\s*\)',
        r'\1'),

    # Remove substring range on LHS comparison
    (u'substring/lhs',
        r'\((\d+|[a-zA-Z][a-zA-Z0-9_]*)(:(\d+|[a-zA-Z][a-zA-Z0-9_]*))?\)(\s*=)',
        r'\4'),
    
    # Convert substring range to arguments on RHS
    (u'substring/rhs',
        r'\((\d+|[a-zA-Z][a-zA-Z0-9_]*):(\d+|[a-zA-Z][a-zA-Z0-9_]*)\)(?!\s*=)',
        r'(\1,\2)'),
    
    # Prefix Type-Casts
    (u'cast', r'(?i)([(.=]\s*REAL)\s*\(', r'\1_CAST('),
)

package_path, _ = os.path.split(__file__)

def parse_args(argv=None):
    argp = argparse.ArgumentParser(description=DESCRIPTION)

    argp_parser = argp.add_argument_group(u"Parsing FORTRAN input")
    argp_parser.add_argument(u'-G', u'--grammar',
                             help=u"Use specified grammar. Use '@'-prefix for bundled grammars. (Default:  %(default)s)",
                             default=u"@fortran.g")
    argp_parser.add_argument(u'-C', u'--config-suffix',
                             help=u"Suffix for per-source configuration file. (Default: %(default)s)",
                             default=u"-wrap")
    argp_parser.add_argument(u'-P', u'--output-pre', action=u"store_true",
                             help=u"Write pre-processed source.",
                             default=False)
                         
    argp_generator = argp.add_argument_group(u"Code generation")
    argp_generator.add_argument(u'-t', u'--template', action=u'append',
                                help=u"Generate wrapping code for each template given. Uset '@'-prefix for bundled templates.",
                                required=True)

    argp.add_argument(u'-e', u'--encoding',
                      help=u"Use the specified encoding for reading/writing source files.",
                      default='utf8')
    argp.add_argument(u'-l', u'--logfile',
                      help=u"Write detailed log to LOGFILE.")
    argp.add_argument(u'-v', u'--verbose', action=u'count',
                      help=u"Increase verbosity",
                      default=0)
    argp.add_argument(u'-q', u'--quiet', action=u'count',
                      help=u"Decrease verbosity",
                      default=2)
    argp.add_argument(u'source', nargs=u'*', metavar=u"SOURCE")

    args = argp.parse_args(argv)
    return args

def init_logger(args):
    # Calculate and set log level: Default is logging.INFO (2*10).
    # Increase for every -q, decrease for every -v. Scale to range logging.DEBUG..logging.FATAL (0..40).
    log_level = min(max((args.quiet - args.verbose) * 10, logging.DEBUG), logging.FATAL)
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)-5s %(msg)s")
    # HACK'd, but why?: log = logging.getLogger(__name__)
    class log(object):
        info = staticmethod(print)
        debug = staticmethod(print)
    return log

def load_templates(log, args):
    log.info(u"Loading {0} templates...".format(len(args.template)))
    templates = []
    start = time.time()
    
    for template_filename in args.template:
        if template_filename[0] == u'@':
            template_filename = os.path.join(package_path, u'template', template_filename[1:])
        
        log.debug(u"* Loading template from {0}...".format(template_filename))
        template_file = open(template_filename, 'r')
        template = jinja2.Template(template_file.read())
        template_suffix, _ = os.path.splitext(os.path.basename(template_filename))
        templates.append((template, template_suffix))
    
    timer = time.time() - start
    log.debug(u"* Loaded {0} templates in {1}.".format(len(templates), timer))
    
    return templates

def load_grammar(log, args):
    grammar_filename = args.grammar
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
    timer = time.time() - start
    log.debug(u"* Loaded grammar in {0}.".format(timer))
    
    sys.setrecursionlimit(old_recursionlimit)
    return grammar

def preprocess(log, config, source):
    if config.has_option(u'parser', u'ignore'):
        source_lines = source.splitlines()
        line_nos = map(int, config.get(u'parser', u'ignore').split(','))
        for line_no in line_nos:
            source_lines[line_no - 1] = u'!F2x/ignore ' + source_lines[line_no - 1]
        
        # u'\n'.join(...) won't work on Python 2.7 :(
        source = u''
        for source_line in source_lines:
            source += source_line + u'\n'

        log.debug(u"  * Ignoring {0} lines.".format(len(line_nos)))

    for rule_name, pattern, replace in PREPROCESS_RULES:
        source, count = re.subn(pattern, replace, source)
        if count:
            log.debug(u"  * Applied '{0}' {1} times.".format(rule_name, count))
    
    return source

class TreeAccess(object):
    DEFAULT_MAPPING = {
        u'module_name': (u'module_stmt name', False, None),
        u'uses': (u'use_stmt', True, {
            u'name': (u'name', False, None)
        }),
        u'type_defs': (u'derived_type_def', True, {
            u'name': (u'derived_type_stmt name', False, None),
            u'fields': (u'component_def_stmt', True, {
                u'name': (u'component_decl name', False, None),
                u'type': (u'intrinsic_type_kind', False, None),
                u'kind': (u'kind_selector part_ref', False, None),
                u'double_type': (u'intrinsic_type_spec', False, None),
                u'type_name': (u'derived_type_spec name', False, None),
                u'char_length': (u'char_selector int_literal_constant', False, None),
            }),
        }),
        u'functions': (u'function_subprogram', True, {
            u'name': (u'function_stmt name', False, None),
            u'args': (u'dummy_arg', True, {
                u'name': (u'name', False, None),
            })
        }),
        u'subroutines': (u'subroutine_subprogram', True, {
            u'name': (u'subroutine_stmt name', False, None),
            u'args': (u'dummy_arg', True, {
                u'name': (u'name', False, None),
            })
        }),
    }
    
    def __init__(self, source_tree, mapping=None):
        self._ast = source_tree
        self._mapping = mapping or TreeAccess.DEFAULT_MAPPING
    
    def __getitem__(self, key):
        path, coll, mapping = self._mapping.get(key, (key, True, None))
        item = self._ast.select(path)
        if coll:
            item = [TreeAccess(node, mapping) for node in item]
        else:
            item = item[0].tail[0]
        return item

def main():
    args = parse_args()
    log = init_logger(args)
    
    log.info(DESCRIPTION + u" Version " + VERSION)
    templates = load_templates(log, args)
    grammar = load_grammar(log, args)
    
    for source_filename in args.source:
        log.info(u"Processing {0}...".format(source_filename))
        
        config_filename = source_filename + args.config_suffix
        config = ConfigParser.ConfigParser()
        if os.path.isfile(config_filename):
            log.debug(u"* Found source config at {0}.".format(config_filename))
            config.read(config_filename)
        
        source_file = open(source_filename, 'rb')
        source = source_file.read().decode(args.encoding)
        log.debug(u"* Source loaded, preprocessing...")

        source = preprocess(log, config, source)
        if args.output_pre:
            pre_filename = source_filename + u'.pre'
            open(pre_filename, 'wb').write(source.encode(args.encoding))
            log.debug(u"* Pre-processed source written to {0}.".format(pre_filename))
        
        log.debug(u"* Parsing FORTRAN source...")
        source_tree = grammar.parse(source)
        access_tree = TreeAccess(source_tree)
        
        output_basename, _ = os.path.splitext(source_filename)
        for template, suffix in templates:
            output_filename = output_basename + suffix
            log.debug(u"* Generating {0}...".format(output_filename))
            output = template.render({
                u'ast': source_tree, u'src': access_tree,
                u'context': { u'filename': source_filename, u'basename': os.path.basename(output_basename), u'args': args } })
            output_file = open(output_filename, 'wb').write(output.encode(args.encoding))

if __name__ == '__main__':
    main()
