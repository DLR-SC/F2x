# -*- coding: utf-8 -*-
u""" Main program for F2x - A versatile, template based FORTRAN wrapper.
"""
from __future__ import print_function

import argparse
import logging
import os
import shutil
import time

import jinja2
import plyplus

from F2x import source, tree
from F2x.template.ctypes import glue

VERSION = u"0.1"
DESCRIPTION = u"F2x - A versatile FORTRAN wrapper."

IGNORE_DELTA = 3

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
    argp_parser.add_argument(u'-F', u'--configure', action=u"store_true",
                             help=u"Create/update configuration file.",
                             default=False)
    argp_parser.add_argument(u'-e', u'--encoding',
                             help=u"Use the specified encoding for reading/writing source files.",
                             default='utf8')
                         
    argp_generator = argp.add_argument_group(u"Code generation")
    argp_generator.add_argument(u'-t', u'--template', action=u'append',
                                help=u"Generate wrapping code for each template given. Uset '@'-prefix for bundled templates.",
                                required=True)
    argp_generator.add_argument(u'-d', u'--copy-glue', action=u"store", nargs='?',
                                help="Copy 'glue.py' (used by ctypes template) into destination folder.",
                                default=True)

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

LOG_LEVELS = [
    logging.DEBUG,
    logging.INFO,
    logging.WARNING,
    logging.ERROR,
    logging.FATAL
]
def init_logger(args):
    # Calculate and set log level: Default is logging.INFO (2*10).
    # Increase for every -q, decrease for every -v. Scale to range logging.DEBUG..logging.FATAL (0..40).
    level = max(0, min(args.quiet - args.verbose, len(LOG_LEVELS) - 1))
    logging.basicConfig(filename=args.logfile, level=LOG_LEVELS[level], format="%(levelname)-5s %(msg)s")

    log = logging.getLogger(__name__)
    return log

def load_templates(log, args):
    log.info(u"Loading {0} templates...".format(len(args.template)))
    templates = []
    start = time.time()
    
    for template_filename in args.template:
        if template_filename[0] == u'@':
            template_filename = os.path.join(package_path, u'template', template_filename[1:])
        
        log.debug(u"* Loading template from {0}...".format(template_filename))
        template_path = os.path.dirname(template_filename)
        template_loader = jinja2.FileSystemLoader(template_path)
        template_env = jinja2.Environment(loader=template_loader, extensions=['jinja2.ext.do'])
        with open(template_filename, 'r') as template_file:
            template = template_env.from_string(template_file.read())
        template_suffix, _ = os.path.splitext(os.path.basename(template_filename))
        templates.append((template, template_suffix))
    
    timer = time.time() - start
    log.debug(u"* Loaded {0} templates in {1}.".format(len(templates), timer))
    
    return templates


def main():
    args = parse_args()
    log = init_logger(args)
    #log.setLevel(logging.DEBUG)
    
    log.info(DESCRIPTION + u" Version " + VERSION)
    templates = load_templates(log, args)
    
    for source_filename in args.source:
        log.info(u"Processing {0}...".format(source_filename))

        src = source.SourceFile(source_filename, args)
        src.read()
        src.preprocess()
        
        log.debug(u"* Parsing FORTRAN source...")
        try:
            src.parse()
        except plyplus.ParseError as e:
            if args.configure:
                ignore_lines = set((err.args['line'] for err in e.errors if 'line' in err.args))
                if src.config.has_option('parser', 'ignore'):
                    ignore = src.config.get('parser', 'ignore')
                    if '\n' in ignore:
                        config_ignore_lines = set(map(int, [line.split(';')[0].rstrip() for line in ignore.splitlines() if line]))
                    else:
                        config_ignore_lines = set(map(int, map(str.strip, ignore.split(','))))
                    ignore_lines = ignore_lines.union(config_ignore_lines)
                
                index = 0
                ignore_lines = sorted(ignore_lines)
                
                while index < len(ignore_lines) - 2:
                    while index < len(ignore_lines) - 2 \
                    and 'END' in src.pre_source_lines[ignore_lines[index] - 1]:
                        ignore_lines.pop(index)
 
                    curr = ignore_lines[index]
                    succ = ignore_lines[index + 1]
                    delta = succ - curr

                    if delta < IGNORE_DELTA \
                    and 'END' not in src.pre_source_lines[succ - 1]:
                        ignore_lines[index + 1:index + 1] = range(curr + 1, succ)
                        index += delta
                    
                    index += 1
                
                print('ignore =')
                for line in ignore_lines:
                    print('\t{0}\t;{1}'.format(line, src.pre_source_lines[line - 1]))
                    
            else:
                for err in e.errors:
                    val = err.args.get('value', '<Not defined>')
                    line = err.args.get('line', -1)
                    col = err.args.get('col', -1)
                    
                    prefix = "{0}:{1}{2}:".format(source_filename, line, (':' + str(col)) if col >= 0 else '')
                        
                    print(prefix + "Syntax error near '{0}'".format(val))
                    if col >= 0:
                        space = ''.join([(c if c == '\t' else ' ') for c in src.pre_source_lines[line - 1][:col - 1]])
                        print(prefix + src.pre_source_lines[line - 1])
                        print(prefix + space + ('^' * len(val)))
        
        access_tree = tree.Module(src.tree)
        access_tree.export_methods(src.config)
        print(access_tree)
        
        if not src.config.has_section('generate'):
            src.config.add_section('generate')
        
        if not src.config.has_option('generate', 'dll'):
            src.config.set('generate', 'dll', 'lib' + access_tree['name'] + '.so')
        
        output_basename, _ = os.path.splitext(source_filename)
        for template, suffix in templates:
            output_filename = output_basename + suffix
            log.debug(u"* Generating {0}...".format(output_filename))
            output = template.render({
                u'ast': src.tree, u'module': access_tree,
                u'config': src.config, u'ifort_dll': True,
                u'context': { u'filename': source_filename, u'basename': os.path.basename(output_basename), u'args': args } })
            with open(output_filename, 'wb') as output_file:
                output_file.write(output.encode(args.encoding))

        if args.copy_glue == 'auto':
            output_dir, _ = os.path.split(output_basename)
            output_file = os.path.join(output_dir, "glue.py")
            if os.path.exists(output_file):
                os.unlink(output_file)
            shutil.copy(glue.__file__, output_file)

if __name__ == '__main__':
    main()
