import os
from distutils import dir_util

from sphinx.ext import apidoc
from sphinx.setup_command import BuildDoc


def document_templates(source_dir):
    """
    Write a Sphinx documentation file for each template (including dependencies).

    :param source_dir: Base directory to write output to.
    """
    from F2x.template import collect_template_data

    # Find templates and generate automatic documentation
    template_base_dir = os.path.join(source_dir, 'api', 'template')

    for template, template_files in collect_template_data({'templates', 'depends'}):
        template_dir = os.path.join(template_base_dir, template.name)
        dir_util.mkpath(template_dir)

        for template_file in template_files:
            template_ref, _ = os.path.splitext(template_file)

            if template_ref.startswith('.'):
                template_ref = '_' + template_ref[1:]

            template_auto_path = os.path.join(template_base_dir, template.name, template_ref + '.rst')
            dir_util.mkpath(os.path.dirname(template_auto_path))
            with open(template_auto_path, 'w') as template_auto_file:
                template_auto_file.write(f'{template_file}\n')
                template_auto_file.write('=' * len(template_file) + '\n\n')
                template_auto_file.write(f'.. autojinja:: {os.path.join(template.name, template_file)}\n')


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
        document_templates(self.source_dir)

        super(build_sphinx, self).run()
