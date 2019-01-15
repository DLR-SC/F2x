# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
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


# -- Path setup --------------------------------------------------------------

import os
import sys

dirname = os.path.dirname(__file__)
sys.path.insert(0, os.path.abspath(os.path.join(dirname, '..', '..', 'src')))  # F2x source dir
sys.path.insert(1, os.path.abspath(os.path.join(dirname, '..', 'contrib', 'sphinx-fortran')))
sys.path.insert(2, os.path.abspath(os.path.join(dirname, '..', 'contrib', 'ext')))  # own extensions


# -- Project information -----------------------------------------------------

import F2x

project = F2x.program_name
copyright = '2018, German Aerospace Center (DLR)'
author = 'Michael Meinel'

# The short X.Y version
version = F2x.get_version_string()
# The full version, including alpha/beta/rc tags
release = F2x.get_version_string(full=True)


# -- General configuration ---------------------------------------------------

extensions = [
    # Automatic generation for Python API docs
    'sphinx.ext.autodoc',

    # Required by f2x_ext - F2xSummaryTable (f2x:templatesummary, f2x:strategysummary)
    'sphinx.ext.autosummary',

    # Language support: Fortran, Jinja2
    'sphinxfortran.fortran_domain',
    'sphinxcontrib.jinjadomain',
    'sphinxcontrib.autojinja.jinja',

    # Generate doc from ArgParser
    'sphinxarg.ext',

    # Custom extensions for F2x
    'f2x_ext',
]

templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'
language = None
exclude_patterns = []
pygments_style = None


# -- Options for HTML output -------------------------------------------------

html_theme = 'bizstyle'
html_static_path = ['_static']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
# html_sidebars = {}


# -- Options for HTMLHelp output ---------------------------------------------

htmlhelp_basename = 'F2xdoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'F2x.tex', 'F2x Documentation', 'Michael Meinel', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'f2x', 'F2x Documentation', [author, ], 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc,
     'F2x', 'F2x Documentation', author,
     'F2x', 'A versatile Fortran wrapper',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Extension configuration -------------------------------------------------

# -- sphinxjinja.autojinja.jinja ---------------------------------------------

jinja_template_path = os.path.join(os.path.dirname(__file__), '..', '..', 'src', 'F2x', 'template')

# -- F2x ---------------------------------------------------------------------

f2x_examples = [
    ('content/example', 'mylib', 'src.zip'),
]
