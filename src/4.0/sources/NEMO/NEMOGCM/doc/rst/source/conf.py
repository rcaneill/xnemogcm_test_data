# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Project information -----------------------------------------------------

project = 'NEMO'
author = 'NEMO System Team'

# The short X.Y version
version = '4.0'
# The full version, including alpha/beta/rc tags
release = 'release-4.0'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = ['sphinx.ext.extlinks', 'sphinxcontrib.bibtex']

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The master toctree document.
master_doc = 'NEMO_guide'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path .
exclude_patterns = ['global.rst', 'coarsening.rst']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

html_favicon = '_static/ORCA.ico'


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
    (master_doc, 'NEMO_guide.tex', 'NEMO Quick Start Guide',
     'NEMO System Team', 'howto'),
]


# -- Customisation -----------------------------------------------------------

# Timestamping
copyright = '2008-2019, NEMO Consortium'

# Link aliases
extlinks = {
	'doi'    : ('https://doi.org/%s'                       , None),
	'forge'  : ('https://forge.ipsl.jussieu.fr/nemo/%s'    , None),
	'github' : ('https://github.com/%s'                    , None),
	'xios'   : ('https://forge.ipsl.jussieu.fr/ioserver/%s', None),
	'website': ('https://www.nemo-ocean.eu/%s'             , None),
	'zenodo' : ('https://zenodo.org/publication/%s'        , None)
}

# Include common directives for every rst file
rst_epilog = open('global.rst', 'r').read()

# SVN revision
import subprocess
revision = subprocess.check_output("svnversion").decode("utf-8")
rst_prolog = '.. |revision| replace:: %s' % revision

# Default language to highlight set to fortran
highlight_language = 'fortran'
