import argparse
import logging

import F2x


def get_args_parser():
    argp = argparse.ArgumentParser(prog=F2x.program_name, description=f'{F2x.program_name} - {F2x.program_description}')

    argp.add_argument('-c', '--config', action="store",
                      help="Load configuration file.")

    argp_parser = argp.add_argument_group(u"Fortran parser")
    argp_parser.add_argument('-G', '--grammar', default=u"@fortran.g",
                             help="Use specified grammar. Bundled grammars should be prefixed by @. "
                                  "(Default:  %(default)s)")
    argp_parser.add_argument('-C', '--config-suffix', default=u"-wrap",
                             help="Suffix for per-source configuration file. (Default: %(default)s)")
    argp_parser.add_argument('-P', '--output-pre', action=u"store_true",  default=False,
                             help="Write pre-processed source.")
    argp_parser.add_argument('-F', '--configure', action=u"store_true", default=False,
                             help="Create/update configuration file.")
    argp_parser.add_argument('-e', '--encoding', default='utf8',
                             help="Use the specified encoding for reading/writing source files.")
    argp_parser.add_argument('-i', '--tree-class', action="store",
                             help="Tree class to use for parsing (module.name:ClassName).")

    argp_wrapping = argp.add_argument_group("Automatic wrapping")
    argp_wrapping.add_argument('-W', '--wrap', action=u'store', metavar='STRATEGY',
                                help=u"Wrap sources by applying the given STRATEGY.")
    argp_wrapping.add_argument('-m', '--module-name', action='store', default='ext.*',
                                help="Full name of the module to generate.")
    argp_wrapping.add_argument('-n', '--library-name', action='store',
                                help="Set the library name.")
    argp_wrapping.add_argument('-s', '--autosplit', action='store_true', default=False,
                               help="Automatically create an own extension for each source file.")
    argp_wrapping.add_argument('-S', '--add-strategy', action='append', nargs=3, metavar=('NAME', 'CLASS', 'TEMPLATES'),
                               help="Load a strategy for later use. NAME should be the name for the strategy and "
                                    "CLASS should be the fully qualified class name of a build strategy. TEMPLATES is "
                                    "a comma separated list of templates to use.")

    argp_generator = argp.add_argument_group(u"Code generation")
    argp_generator.add_argument('-R', '--register-template', action='append', metavar='PACKAGE',
                                help="Load a template pacakge into the regitry for later use. PACAKGE should be the "
                                     "fully qualified name of a F2x template package.")
    argp_generator.add_argument(u'-t', u'--template', action=u'append', metavar='NAME', default=[],
                                help="Generate wrapping for the given template. NAME should be the name of a F2x "
                                     "template package, the path to a template that can be found in the template path "
                                     "or a bundled template that should be prefixed by @.")
    argp_generator.add_argument('-T', '--template-path', action='append', metavar='PATH', default=[],
                                help="Add PATH to the template search path.")
    argp_generator.add_argument('-x', '--jinja-ext', action='append', metavar='EXTENSION', default=['jinja2.ext.do'],
                                help="Add EXTENSION to Jinja2 environment.")

    argp_logging = argp.add_argument_group(u"Logging")
    argp_logging.add_argument(u'-l', u'--logfile', action='store',
                              help=u"Write detailed log to LOGFILE.")
    argp_logging.add_argument(u'-v', u'--verbose', action=u'count', default=0,
                              help=u"Increase verbosity.")
    argp_logging.add_argument(u'-q', u'--quiet', action=u'count', default=2,
                              help=u"Decrease verbosity.")

    argp_action = argp.add_argument_group(u"Action")
    argp_action.add_argument('-f', '--force', action='store_true', default=False,
                             help="Force rebuild of wrapper.")
    argp_action.add_argument(u'--get', nargs=1, choices=['depends', 'modules', 'libraries', 'extlib'],
                             help="Collect template information about dependencies, modules, libraries, or "
                                  "the name of the extension library.")
    argp_action.add_argument(u'source', nargs=u'*', metavar=u"SOURCE",
                             help=u"Wrap the given templates to the SOURCE files.")

    argp_deprecated = argp.add_argument_group("Deprecated")
    argp_deprecated.add_argument(u'-d', u'--copy-glue', action=u"store_true", default=False,
                                 help=u"Copy 'glue.py' (used by ctypes template) into destination folder.")
    argp_deprecated.add_argument(u'--py-absolute-import', action=u"store_true", default=False,
                                 help=u"Use absolute import for Python helper modules (e.g. F2x.template.ctypes.glue).")

    return argp


LOG_LEVELS = [
    logging.NOTSET,
    logging.DEBUG,
    logging.INFO,
    logging.WARNING,
    logging.ERROR,
    logging.FATAL
]


class _ParentLogHandler(logging.Handler):
    def __init__(self, parent, level=logging.NOTSET):
        super(_ParentLogHandler, self).__init__(level or parent.level)
        self.parent = parent

    def emit(self, record):
        self.parent.log(record.levelno, record.msg, *record.args)


def init_logger(args, parent=None, fmt=None):
    level = max(0, min(args.quiet - args.verbose, len(LOG_LEVELS) - 1))
    root = logging.root
    fmt = fmt or "%(levelname)-5s %(message)s"

    if parent:
        # If a parent logger is present, install it as 'handler'
        if not root.hasHandlers():
            logging.basicConfig(level=LOG_LEVELS[level], handlers=[_ParentLogHandler(parent)])

    else:
        # Calculate and set log level: Default is logging.INFO (2*10).
        # Increase for every -q, decrease for every -v. Scale to range logging.DEBUG..logging.FATAL (0..40).
        if not root.hasHandlers():
            logging.basicConfig(filename=args.logfile, level=LOG_LEVELS[level], format=fmt)

        else:
            root.setLevel(LOG_LEVELS[level])
            for handler in root.handlers:
                handler.setLevel(LOG_LEVELS[level])
                handler.setFormatter(logging.Formatter(fmt))

    log = logging.getLogger(__name__)
    if not parent:

        def new_log(level, msg, args):
            log.log(level * 10, msg, *args)

        from numpy.distutils import log as numpy_log
        numpy_log._global_log._log = new_log

    return log


def parse_args(argv=None):
    args = get_args_parser().parse_args(argv)
    return args


def get_arg(args, config, name, section, default, cast):
    arg = getattr(args, name, default)
    if arg != default:
        return arg

    if config.has_section(section) and config.has_option(section, name):
        return cast(config.get(section, name))

    return default
