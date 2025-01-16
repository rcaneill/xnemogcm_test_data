**Contents**

.. contents::
   :local:




.. _`Former web platform forge`: https://forge.ipsl.jussieu.fr/nemo
.. _`NEMO users' guide`: https://sites.nemo-ocean.io/user-guide
.. _`Migration Guide`: https://sites.nemo-ocean.io/user-guide/migration.html
.. _`Change list`: https://sites.nemo-ocean.io/user-guide/changes.html
.. _`Test case repository`: https://github.com/NEMO-ocean/NEMO-examples
.. _`How to cite`: https://www.nemo-ocean.eu/bibliography/how-to-cite/
.. _`NEMO forums`: https://nemo-ocean.discourse.group
.. _`NEMO newsletter`: https://listes.ipsl.fr/sympa/subscribe/nemo-newsletter
.. _`NEMO publications`: https://www.nemo-ocean.eu/bibliography/publications/add
.. _`NEMO projects`: https://www.nemo-ocean.eu/projects/add
.. _`Special Issue`: https://gmd.copernicus.org/articles/special_issue40.html
.. _`NEMO System Team wiki`: https://forge.nemo-ocean.eu/developers/home/-/wikis/Home
.. _`NEMO ocean engine`: https://zenodo.org/record/1464816
.. _`NEMO Tracers engine` : https://zenodo.org/record/1471700
.. _`NEMO Sea Ice engine`: https://zenodo.org/record/1471689


**Welcome to NEMO home page!**
  
NEMO (*Nucleus for European Modelling of the Ocean*) is a state-of-the-art modelling
framework for research activities and forecasting services in ocean and climate sciences,
developed in a sustainable way by the NEMO European consortium since 2008.

This page intends to help you to get started using the NEMO platform and to introduce you
to the different levels of information available. It starts here with NEMO release 4.2.0.

Reminder: Our `Former web platform forge`_ (SVN+Trac) contains the previous documentation
and releases made available from the beginning of the project up to of NEMO 4.0.

Getting started
===============

Getting your hands on NEMO: the first steps are described in detail in the 
`NEMO users' guide`_ . This explains how to download the code, build the environment, 
create the executable, and perform a first integration.

If you are already using a previous release of NEMO, please refer to the 
`Migration Guide`_ which aims to help you to make the move to 4.2.0.

The above users guides cover in detail what is available from gitlab and supported by NEMO
System Team. Aside from this web platform, a set of test cases is also available from the
`Test case repository`_ . These test cases can be useful for students, outreach, and
exploring specific aspects of NEMO with light configurations. The web page also allows you
to submit test cases you have developed and want to share with the community. Feel free to
contribute!


Project documentation
=====================

Reference manuals fully describing NEMO  for the three main component

* |OCE| models the ocean {thermo}dynamics and solves the primitive equations (`./src/OCE <./src/OCE>`_)

* |ICE| simulates sea-ice {thermo}dynamics, brine inclusions and subgrid-scale thickness 
  variations (`./src/ICE <./src/ICE>`_)

* |MBG| models the {on,off}line oceanic tracers transport and biogeochemical processes 
  (`./src/TOP <./src/TOP>`_)
are available from Zenodo:

============ ======================== =====
 Component    Reference Manual         DOI
============ ======================== =====
 |NEMO-OCE|   `NEMO ocean engine`_    .. image:: https://zenodo.org/badge/DOI/10.5281/zenodo.6334656.svg
                                         :target: https://doi.org/10.5281/zenodo.6334656                        
 |NEMO-ICE|   `NEMO Sea Ice engine`_  *not yet available*
 |NEMO-MBG|   `NEMO Tracers engine`_  .. image:: https://zenodo.org/badge/DOI/10.5281/zenodo.1471700.svg
                                         :target: https://doi.org/10.5281/zenodo.1471700
============ ======================== =====

These reference manuals are the publications that should be cited in your own
publications. Please visit `How to cite`_? for details.

New features of 4.2.0 release are described in the `Change list`_ section of the `NEMO users' guide`_


Asking questions, and exchanging information
============================================
- Register once for all and use the `NEMO forums`_ on discourse to share and discuss with the NEMO community.

- Register once for all and receive by mail the `NEMO newsletter`_ : recommended for all
  users to receive the major announcements from the project (new releases, open meetings and
  main informations). Low traffic: about ten messages a year.


Contributing to NEMO visibility: projects and publications
==========================================================
Please help us justifying the NEMO development efforts by

-  Adding your publications using NEMO and its outputs to the `NEMO publications`_ page

-  Describing your project using NEMO on the `NEMO projects`_ page

NEMO also has a `Special Issue`_ in the open-access journal
Geoscientific Model Development (GMD) from the European Geosciences Union
The main scope is to collect relevant manuscripts covering various topics and
to provide a single portal to assess the model potential and evolution.



Contributing to NEMO development
================================

NEMO strives to be written in a way which allows the easy incorporation of developments.
You are welcome to contribute to the development of the NEMO Shared reference.  NEMO
development is driven by  NEMO Consortium planning and producing NEMO's sustainable
development in order to keep a reliable evolving framework.  Development is organised and
scheduled through a five years development strategy, Working groups and the activities of
the development team (named NEMO System Team) in a yearly workplan. More information is
available on the `NEMO System Team wiki`_


Disclaimer
==========

The NEMO source code is freely available and distributed under
`CeCILL v2.0 license <./LICENSE>`_ (GNU GPL compatible).

You can use, modify and/or redistribute the software under its terms,
but users are provided only with a limited warranty and the software's authors and
the successive licensor's have only limited liability.
