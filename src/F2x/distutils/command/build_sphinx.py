import os

from F2x.distutils.core import numpy_cmdclass

from sphinx.ext import apidoc
from sphinx.setup_command import BuildDoc


class build_sphinx(BuildDoc):
    """
    Build documentation based on Sphinx.

    This implementation adds automatic generation of the API documentation (:code:`sphinx-apidoc ...`) during the build.
    """

    use_f2x_template = False

    def run(self):
        source_dirs = list(self.distribution.package_dir.values()) \
                      or [os.path.dirname(__file__)]

        # Update automatically generated docs.
        apidoc.main(['-f', '-T', '-e', '-M', '-o', os.path.join(self.source_dir, 'api')] + source_dirs)
        self.document_templates()

        super(build_sphinx, self).run()

    def document_templates(self):
        from F2x.template import collect_template_data

        # Find templates and generate automatic documentation
        template_base_dir = os.path.join(self.source_dir, 'api', 'template')

        for template, template_files in collect_template_data({ 'templates', 'depends' }):
            template_toc_path = os.path.join(template_base_dir, template.name, 'toc.rst')
            self.mkpath(os.path.dirname(template_toc_path))

            for template_file in template_files:
                template_ref, _ = os.path.splitext(template_file)
                if template_ref.startswith('.'):
                    template_ref = '_' + template_ref[1:]
                template_auto_path = os.path.join(template_base_dir, template.name, template_ref + '.rst')

                self.mkpath(os.path.dirname(template_auto_path))
                with open(template_auto_path, 'w') as template_auto_file:
                    title = f'{template_file}'
                    template_auto_file.write(f'{title}\n{"=" * len(title)}\n\n')
                    template_auto_file.write(f'.. autojinja:: {os.path.join(template.name, template_file)}\n')


numpy_cmdclass['build_sphinx'] = build_sphinx
