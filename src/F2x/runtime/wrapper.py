import ctypes
import os
from importlib import import_module

import jinja2
import plyplus

from numpy.distutils import system_info
from F2x.distutils.strategy import get_strategy, base
from F2x.parser.plyplus.source import SourceFile
from F2x.template import package_dir as template_package_dir, get_template


class _PlatformInfo(object):
    _default_kind = {
        'INTEGER': ctypes.sizeof(ctypes.c_int),
        'REAL': ctypes.sizeof(ctypes.c_float),
        'LOGICAL': ctypes.sizeof(ctypes.c_int32),
    }

    get_default_kind = _default_kind.get


class F2xWrapper(object):
    def __init__(self, args, log):
        self.args = args
        self.log = log

    def run(self):
        strategy, template_files = self._templates_from_args()
        if strategy is None:
            strategy = base.BuildStrategy()
        template_sources = self._resolve_templates(template_files)
        templates = self._load_templates(template_sources)

        self.progress = 0
        self.total_progress = (len(templates) + 4) * len(self.args.source)
        if self.args.tree_class is not None:
            mod_name, cls_name = self.args.tree_class.split(':')
            if '.' in mod_name:
                pkg_name, _ = mod_name.rsplit('.', 1)
                mod = import_module(mod_name, pkg_name)
            else:
                mod = import_module(mod_name)
            cls = getattr(mod, cls_name)
        else:
            cls = None

        for input_file in self.args.source:
            self.progress += 1
            self.log.info(f'{self._current_progress} reading {input_file}')

            src = self._load_src(input_file)
            if src is None:
                self.progress += 3
                continue
            self.progress += 2

            module = src.get_gtree(cls)
            self.progress += 1

            if not src.config.has_option('generate', 'dll'):
                lib_name = self.args.library_name or module["name"].lower()
                src.config.set('generate', 'dll', f'lib{lib_name}{system_info.so_ext}')

            self._generate_from_templates(templates, src, module)

    @property
    def _current_progress(self):
        return f'[{self.progress * 100 // self.total_progress:-3}%]'

    def _generate_from_templates(self, templates, src, module):
        output_basename, _ = os.path.splitext(src.filename)

        for template, suffix in templates:
            output_filename = output_basename + suffix
            self.log.info(f'{self._current_progress} generating {output_filename} from {src.filename}')
            self.progress += 1

            output = template.render({
                u'ast': src.tree, u'module': module,
                u'platform': _PlatformInfo(),
                u'config': src.config, u'ifort_dll': True,
                u'context': {u'filename': src.filename, u'basename': os.path.basename(output_basename),
                             u'args': self.args}})

            with open(output_filename, 'wb') as output_file:
                output_file.write(output.encode(self.args.encoding))

    def _load_src(self, input_file):
        src = SourceFile(input_file, self.args)
        try:
            src.read()
            src.preprocess()
            src.parse()
        except plyplus.ParseError as e:
            self.log.error(f'error reading source file {e}')
            return None

        return src

    def _load_templates(self, template_sources):
        self.log.debug(f'loading {len(template_sources)} templates')
        templates = []
        for mod, name, filename, path, suffix in template_sources:
            path = list(self.args.template_path) + path
            extensions = list(self.args.jinja_ext)

            if mod is not None and hasattr(mod, 'extensions'):
                extensions += mod.extensions
                path.append(mod.package_dir)

            loader = jinja2.FileSystemLoader(path)
            env = jinja2.Environment(loader=loader, extensions=extensions)

            template = None
            if name[0] != '@':
                try:
                    template = loader.load(env, filename)
                except IOError as e:
                    self.log.info(f'error reading template {filename} from environment ({e})')

            if template is None:
                with open(filename, 'r') as template_file:
                    template = env.from_string(template_file.read())
                    template.name = name
                    template.filename = filename

            templates.append((template, suffix))

        return templates
    
    def _resolve_templates(self, template_files):
        template_sources = []

        for template_name, template_file, template_path in template_files:
            if template_name[0] == u'@':
                template_path.append(os.path.dirname(template_file))
                template_dirname, template_basename = os.path.split(template_file)
                _, template_modname = os.path.split(template_dirname)
                template_mod = get_template(template_modname)
                template_suffix, template_ext = os.path.splitext(template_basename)
            else:
                template_mod = get_template(template_name)
                template_suffix, template_ext = os.path.splitext(os.path.basename(template_file))

            template_sources.append((template_mod, template_name, template_file, template_path, template_suffix))

        return template_sources

    def _templates_from_args(self):
        strategy = None
        templates = []
        template_args = self.args.template[:]

        if self.args.wrap:
            # Load strategy if supplied
            strategy = get_strategy(self.args.wrap)
            if strategy is not None and not templates:
                template_args = strategy.templates[:]

        if template_args:
            for filename in template_args:
                if filename[0] == '@':
                    # Add internal template
                    templates.append((filename, os.path.join(template_package_dir, filename[1:]), []))
                else:
                    # Add external template with path
                    dirname, basename = os.path.split(filename)
                    templates.append((basename, filename, [dirname]))

        return strategy, templates