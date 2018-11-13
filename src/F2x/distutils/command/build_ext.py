from numpy.distutils.command.build_ext import build_ext as numpy_build_ext


class build_ext(numpy_build_ext):
    def run(self):
        super(build_ext, self).run()

    def get_ext_filename(self, ext_name):
        build_src = self.get_finalized_command('build_src')
        extension = build_src.find_extension(ext_name)
        filename = None

        if extension is not None:
            filename = extension.f2x_target.get_ext_filename(build_src, extension)

        if filename is None:
            filename = super(build_ext, self).get_ext_filename(ext_name)

        return filename

    def get_libraries(self, ext):
        libraries = super(build_ext, self).get_libraries(ext)
        libraries = ext.f2x_target.finish_libraries(ext, libraries)
        return libraries
