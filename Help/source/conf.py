# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'PyScripter'
copyright = '2024, Kiriakos Vlahos'
author = 'Kiriakos Vlahos'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['myst_parser']

exclude_patterns = []
rst_epilog = ''
html_show_sourcelink = False
suppress_warnings = [
    'myst.header',  # Non-consecutive header level increase
]

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinxdoc'
html_title = "PyScripter Help"
html_static_path = ['_static']
html_css_files = ['custom.css']
html_show_copyright = False
html_show_sphinx = False
html_output_encoding = "utf-8"
htmlhelp_basename = 'Pyscripter'
html_file_suffix = ".htm"
html_link_suffix = ".htm"
htmlhelp_file_suffix = ".htm"
htmlhelp_link_suffix = ".htm"

myst_enable_extensions = [
    "attrs_inline",
    "colon_fence",
    "html_image",
]
myst_heading_anchors = 4