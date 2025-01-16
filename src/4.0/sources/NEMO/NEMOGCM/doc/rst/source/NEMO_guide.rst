#################
Quick Start Guide
#################

..
	For headings markup, this convention is recommended from Python’s Style Guide
	# with overline, for parts
	* with overline, for chapters
	=, for sections
	-, for subsections
	^, for subsubsections
	", for paragraphs

	'global.rst' contains a list of common directives (roles, substitutions and URL links)
	It is loaded for each file thanks to 'rst_epilog' setting in 'conf.py'

.. toctree::
   :hidden:
   :caption: Getting started

   install
   configurations
   test_cases

.. toctree::
   :hidden:
   :caption: Setup your configuration

   setup
   diagnostics
   tools

.. toctree::
   :hidden:
   :caption: Advanced use

   zooms
   coupling
   data_assimilation
   tracers

.. toctree::
   :hidden:
   :caption: Miscellaneous

   contributing
   release_notes
   Glossary<definitions>

.. include:: readme.rst

.. Next headings markup acording to readme.rst

References
==========

.. bibliography:: references.bib
   :all:
   :style: unsrt
   :labelprefix: R

Disclaimer
==========

The NEMO source code is freely available and distributed under CeCILL license
(GNU GPL compatible - see ``./LICENSE``).

You can use, modify and/or redistribute the software under its terms,
but users are provided only with a limited warranty and the software's authors and
the successive licensor's have only limited liability.
