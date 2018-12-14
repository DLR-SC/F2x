import argparse
import logging

import F2x


def get_args_parser():
    argp = argparse.ArgumentParser(description=F2x.program_name)

    argp_parser = argp.add_argument_group(u"Fortran parser")
    argp_parser.add_argument('-G', '--grammar', default=u"@fortran.g",
                             help="Use specified grammar. Use '@'-prefix for bundled grammars. "
                                  "(Default:  %(default)s)")
    argp_parser.add_argument('-C', '--config-suffix', default=u"-wrap",
                             help="Suffix for per-source configuration file. (Default: %(default)s)")
    argp_parser.add_argument('-P', '--output-pre', action=u"store_true",  default=False,
                             help="Write pre-processed source.")
    argp_parser.add_argument('-F', '--configure', action=u"store_true", default=False,
                             help="Create/update configuration file.")
    argp_parser.add_argument('-e', '--encoding', default='utf8',
                             help="Use the specified encoding for reading/writing source files.")

    argp_wrapping = argp.add_argument_group("Automatic wrapping")
    argp_wrapping.add_argument('-W', '--wrap', action=u'store', metavar='STRATEGY',
                                help=u"Wrap sources by applying the given strategy.")
    argp_wrapping.add_argument('-m', '--module-name', action='store', default='ext.*',
                                help="Full name of the module to generate.")
    argp_wrapping.add_argument('-n', '--library-name', action='store',
                                help="Set the library name.")
    argp_wrapping.add_argument('-s', '--autosplit', action='store_true', default=False,
                               help="Automatically create an own extension for each source file.")
    argp_wrapping.add_argument('-S', '--add-strategy', action='append',
                               help="Load a STRATEGY for later use. STRATEGY should be the path to a Python module "
                                    "that defines annd adds strategies.")

    argp_generator = argp.add_argument_group(u"Code generation")
    argp_generator.add_argument(u'-t', u'--template', action=u'append',
                                help=u"Generate wrapping code for each template given. Prefix pathes with \@ to "
                                     u"reference bundled templates.")
    argp_generator.add_argument('-T', '--template-path', action='append', metavar='PATH', default=[],
                                help="Add PATH to the template search path.")
    argp_generator.add_argument('-x', '--jinja-ext', action='append', metavar='EXTENSION', default=['jinja2.ext.do'],
                                help="Add EXTENSION to Jinja2 environment.")

    argp_deprecated = argp.add_argument_group("Deprecated")
    argp_deprecated.add_argument(u'-d', u'--copy-glue', action=u"store_true", default=False,
                                 help=u"Copy 'glue.py' (used by ctypes template) into destination folder.")
    argp_deprecated.add_argument(u'--py-absolute-import', action=u"store_true", default=False,
                                 help=u"Use absolute import for Python helper modules (e.g. F2x.template.ctypes.glue).")

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
    argp_action.add_argument(u'--get', nargs=1, choices=['depends', 'modules', 'libraries'],
                             help=u"Get [requires, modules, libraries] for the given templates.")
    argp_action.add_argument(u'source', nargs=u'*', metavar=u"SOURCE",
                             help=u"Wrap the given templates to the SOURCE files.")

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


def init_logger(args, parent=None):
    level = max(0, min(args.quiet - args.verbose, len(LOG_LEVELS) - 1))
    root = logging.root

    if parent:
        # If a parent logger is present, install it as 'handler'
        if not root.hasHandlers():
            logging.basicConfig(level=LOG_LEVELS[level], handlers=[_ParentLogHandler(parent)])

    else:
        # Calculate and set log level: Default is logging.INFO (2*10).
        # Increase for every -q, decrease for every -v. Scale to range logging.DEBUG..logging.FATAL (0..40).
        if not root.hasHandlers():
            logging.basicConfig(filename=args.logfile, level=LOG_LEVELS[level], format="%(levelname)-5s %(message)s")

        else:
            root.setLevel(LOG_LEVELS[level])
            for handler in root.handlers:
                handler.setLevel(LOG_LEVELS[level])
                handler.setFormatter(logging.Formatter("%(levelname)-5s %(message)s"))

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