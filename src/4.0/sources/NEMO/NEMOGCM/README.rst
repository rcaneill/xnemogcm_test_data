:Release:  |release|
:Date:     Jan. 2019

NEMO_ for **Nucleus for European Modelling of the Ocean** is a state-of-the-art modelling framework for
research activities and forecasting services in ocean and climate sciences,
developed in a sustainable way by a European consortium since 2008.

.. contents::
   :local:

Overview
========

The NEMO ocean model has 3 major components:

- |OPA| models the ocean {thermo}dynamics and solves the primitive equations
  (``./src/OCE``)
- |SI3| simulates seaice {thermo}dynamics, brine inclusions and subgrid-scale thickness variations
  (``./src/ICE``)
- |TOP| models the {on,off}line oceanic tracers transport and biogeochemical processes
  (``./src/TOP``)

These physical core engines are described in their respective `references <#project-documentation>`_ that
must be cited for any work related to their use.

Assets and solutions
====================

Not only does the NEMO framework model the ocean circulation,
it offers various features to enable

- Create :doc:`embedded zooms<zooms>` seamlessly thanks to 2-way nesting package AGRIF_.
- Opportunity to integrate an :doc:`external biogeochemistry model<tracers>`
- Versatile :doc:`data assimilation<data_assimilation>`
- Generation of :doc:`diagnostics<diagnostics>` through effective :xios:`XIOS system<>`
- Roll-out Earth system modeling with :doc:`coupling interface<coupling>` based on OASIS_

Several :doc:`built-in configurations<configurations>` are provided to
evaluate the skills and performances of the model which
can be used as templates for setting up a new configurations (``./cfgs``).

The user can also checkout available :doc:`idealized test cases<test_cases>` that
address specific physical processes(``./tests``).

A set of :doc:`utilities <tools>` is also provided to {pre,post}process your data (``./tools``).

Project documentation
=====================

A walkthrough tutorial illustrates how to get code dependencies, compile and execute NEMO
(``./INSTALL.rst``) . 

Reference manuals and quick start guide can be build from source and
exported to HTML or PDF formats (``./doc``) or
downloaded directly from the :website:`website<bibliography/documentation>`.

=========== ===================== ===============
 Component   Reference Manual      Quick start
=========== ===================== ===============
 |OPA|       |NEMO manual|_        |NEMO guide|
             :cite:`NEMO_manual`
 |SI3|       |SI3 manual|
             :cite:`SI3_manual`
 |TOP|       |TOP manual|
             :cite:`TOP_manual`
=========== ===================== ===============

Since 2014 the project has a `Special Issue`_ in the open-access journal
Geoscientific Model Development (GMD) from the European Geosciences Union (EGU).
The main scope is to collect relevant manuscripts covering various topics and
to provide a single portal to assess the model potential and evolution.

Used by a wide audience,
numerous :website:`associated projects<projects>` have been carried out and
extensive :website:`bibliography<bibliography/publications>` published.

Development board
=================

The NEMO Consortium pulling together 5 European institutes (CMCC_, CNRS_, MOI_, `Met Office`_ and NERC_)
plans the sustainable development in order to keep a reliable evolving framework since 2008.

It defines the |NEMO strategy|_ that is implemented by the System Team on a yearly basis in order to
release a new version almost every four years.

When the need arises, :forge:`working groups<wiki/WorkingGroups>` are created or resumed to
gather the community expertise for advising on the development activities.


.. Substitutions / Links

.. |NEMO manual| image:: https://zenodo.org/badge/DOI/10.5281/zenodo.1464816.svg
.. |NEMO guide|  image:: https://zenodo.org/badge/DOI/10.5281/zenodo.1475325.svg
.. |SI3 manual|  image:: https://zenodo.org/badge/DOI/10.5281/zenodo.1471689.svg
.. |TOP manual|  image:: https://zenodo.org/badge/DOI/10.5281/zenodo.1471700.svg

.. |NEMO strategy| replace:: multi-year development strategy

.. _Special Issue: https://www.geosci-model-dev.net/special_issue40.html
