"""F2x.lib.setup

Implements the F2x 'build_glue' command.
"""
import os
import sys

from distutils import log
from distutils.core import Command
from distutils.command.build import build

from F2x.lib import find_library_path
import subprocess


class Build(build):
    # Overrides standard 'build' command to include 'build_glue' as subcommand.

    sub_commands = [('build_glue', lambda _: True)] + build.sub_commands


class BuildGlue(Command):
    description = "Build the glue libraries for F2x."

    # List of option tuples: long name, short name (None if no short name), and help string.
    user_options = [
        ('mingw-path=', None,
         "Path to MinGW environment."),
   ]

    def initialize_options(self):
        self.mingw_path = None

    def finalize_options(self):
        if sys.platform == "win32":
            if self.mingw_path is None:
                self.mingw_path = self._find_mingw()
            
            if self.mingw_path is None:
                log.warn("No MinGW path configured. Make sure, 'make' is available.")

    def _find_mingw(self):
        # TODO Implement heuristic.
        return r"D:\MinGW"
    
    def run(self):
        old_cwd = os.getcwd()
        old_path = os.environ["PATH"].split(os.pathsep)
        
        mingw_path = []
        if self.mingw_path is not None:
            mingw_path = [
                os.path.join(self.mingw_path, "bin"),
                os.path.join(self.mingw_path, "msys", "1.0", "bin"),
            ]
            os.environ["PATH"] = os.pathsep.join(mingw_path + old_path)
        log.info("Buliding Fortran library...")
        self._run_make("fortran")
        os.chdir(old_cwd)
        
        os.environ["PATH"] = os.pathsep.join(old_path)
    
    def _run_make(self, lang, args=None):
        lang_dir = find_library_path(lang)
        os.chdir(lang_dir)
        
        make_cmd = ["make"]
        if args:
            make_cmd += args
        
        make = subprocess.Popen(make_cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = make.communicate()
        if make.returncode:
            log.error(out.decode('latin-1'))
            log.error(err.decode('latin-1'))
        else:
            clean = subprocess.Popen(make_cmd + ["clean"], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            clean.communicate()
