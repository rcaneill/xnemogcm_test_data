*************
Release Notes
*************

New sea-ice component SI3 (in place of LIMx)
============================================

- Physics

  - Lateral melting
  - Melt ponds: constant or :doi:`Holland et al. 2012<10.1175/JCLI-D-11-00078.1>` formulation (and soon topographic melt ponds)
  - Ice-atm. drags from :doi:`Lupkes et al. 2012<10.1029/2012JD017630>` (depending on ice concentration) or :doi:`Lupkes et al. 2015<10.1002/2014JD022418>` (depending on sea ice concentration and atm. stability)
  - Landfast ice (:doi:`Lemieux et al. 2016<10.1002/2016JC012006>`)

- Numerics

  - Advection: Ultimate-Macho scheme
  - Rheology: adaptive EVP (:doi:`Kimmritz et al. 2017<10.1016/j.ocemod.2016.03.004>`)
  - Coupling interface: conductivity as surface forcing instead of heat fluxes (Met Office requirement)

- Performance: all thermodynamics in 1D and reduced mpp communications
- Usability (users & developers friendly)

  - Comprehensive set of outputs (universal units and understandable names + includes CMIP)
  - New architecture and namelist
  - All processes can be decoupled from each other (switch on/off)
  - Ice categories bounds can be defined by the user or set automatically
  - For open boundaries, the number of ice categories from the forcing model can be different from the number of categories in the regional simulation
  - Fully compatible with AGRIF_
  - Revised documentation 

AGRIF for embedded zooms
========================

- Now compatible with new sea ice component
- Now compatible with z* coordinate
- Extended ghost cells area to properly handle scheme with spatial order >2
- Added vertical refinement (beta)
- Nesting tools for setup now up to date and working 

Enhancements
============

- Fix for tracer conservation with split explicit free surface
- Bulk formulae : move to aerobulk package (:doi:`Brodeau et al. 2016<10.1175/JPO-D-16-0169.1>`), i.e. NCAR, COARE and ECMWF bulk (remove Clio and MFS bulk)
- Wetting and drying
- Added tidal self attraction and loading either read from a file or from usual "scalar" approximation
- Add a 4th order centered (CEN) and Flux Corrected Transport (FCT) tracer advection
  (using a 4th compact in the vertical)
- iso-neutral mixing (iso and triad operators):
  add the Method of Stabilizing Correction (MSC) (more accurate calculation) + add a bilaplacian case
- Lateral physics (``LDF``): scale aware setting of eddy viscosity and diffusivity
- Vorticity: 2 new energy conserving scheme: ENT with Coriolis defined at T-point (better for Flux form) and EET a variant of EEN where e3t is used instead of e3f (solved the issue with e3f specification but is not enstrophy conserving) 

Test Cases
==========

The first test cases available for now are:

- ``CANAL``: east-west periodic canal of variable size with several initial states and associated geostrophic currents (zonal jets or vortex)
- ``ICE_AGRIF``: east-west + north-south periodic channel. The common configuration includes an AGRIF zoom (1:3) in the middle of the basin to test how an ice patch is advected through it but one can also test the advection schemes (Prather and Ultimate-Macho) by removing the ``key_agrif`` in the cpp keys
- ``ISOMIP``: simple box configuration with an ice shelf with simple geometry on top. The purpose of this test case is to evaluate the impact of various schemes and new development with iceshelf cavities. The exact original setup is described ​here
- ``LOCK-EXCHANGE``: classical fluid dynamics experiment that has been adapted by Haidvogel and Beckmann (1999) for testing advection schemes in ocean circulation models. It has been used by several authors including Burchard and Bolding (2002) and :doi:`Ilıcak et al. (2012)<10.1016/j.ocemod.2011.10.003>`. The ``LOCK EXCHANGE`` experiment can in particular illustrate the impact of different choices of numerical schemes and/or subgrid closures on spurious interior mixing
- ``OVERFLOW``: illustrates the impact of different choices of numerical schemes and/or subgrid closures on spurious interior mixing close to bottom topography. It is adapted from the non-rotating overflow configuration described in Haidvogel and Beckmann (1999) and further used by :doi:`Ilıcak et al. (2012)<10.1016/j.ocemod.2011.10.003>`
- ``VORTEX``: illustrates the propagation of an anticyclonic eddy over a Beta plan and flat bottom. It is implemented here with an online refined subdomain (thanks to ``AGRIF`` library) out of which the vortex propagates. It serves as a benchmark to diagnose nesting errors as in :doi:`Debreu et al. (2012)<10.1016/j.ocemod.2012.03.003>`, :doi:`Penven et al. (2006)<10.1016/j.ocemod.2005.05.002>` and :doi:`Spall and Holland (1991)<10.1175/1520-0485(1991)021<0205:ANPEMF>2.0.CO;2>`
- ``WAD``: a set of simple closed basin geometries for testing the wetting and drying capabilities. Examples range from a closed channel with EW linear bottom slope to a parabolic EW channel with a Gaussian ridge

New Reference configurations
============================

``AGRIF_DEMO``: 2 interlocked zooms (1:4 & 1:3) in the Nordic Seas + 1 zoom (1:1) at the equator

``SPITZ12``: regional configuration around the Svalbard archipelago 

Wave coupling
=============

Coupled interface to external wave model

Large scale wave interaction process added in momentum and tracer equations 

Passive tracer TOP and biogeochemical PISCES components
=======================================================

- The passive tracers transport component was redesigned toward a modular structure and users can enable each module directly through logical flags in namelist_top (no more fortran macros!)
- :doc:`TOP on-line user documentation<tracers>`
- TOP currently accounts for the following 5 modules: ``CFC`` contains inorganic carbon tracers (CFC11/CFC12/SF6), ``MY_TRC`` is a template for new modules (or external couplings), ``AGE`` deals with water age tracking, ``C14`` as a radiocarbon passive tracer, and the companion ecosystem model ``PISCES``
- A generalized infrastructure was developed to handle the prescription of either surface, coastal, or open boundaries conditions for each passive tracer
- A new configuration, named ``ORCA2_OFF_TRC``, was created to provide a benchmark simulation environment to deal with inert carbon tracers dynamics by exploiting the offline coupling with NEMO
- PISCES model contains new developments and modifications:

  - Particulate Organic Carbon (POC) component comes with a new liability scheme, while the former Kriest parametrisation was superseded
  - A complex iron chemistry scheme is now available, with an improved description of ligands for the marine iron cycle
  - Carbonate chemistry is based on MOCSY 2.0 routines (see :doi:`10.5194/gmd-8-485-2015 <Orr and Epitalon, 2015>`), by complying also with CMIP6 standards
  - Ecosystem components can be optionally modelled by means of explicit nutrient quotas (PISCES-QUOTA) 

High Performance Computing (HPC): performances improvements
===========================================================

- Reduce number of MPI communications
  (suppression of redundant communications, gather multiple communications into one)
- Use of MPI-3 asynchronous routines for performance (use ``key_mpi2`` if MPI-3 not available)
- Back to standard dynamical allocation (remove of wrk_alloc/dealloc statements)
- :xios:`XIOS software<>` for IOs version 2.5 as default, and optionally available for restarts 

Simplification and robustness
=============================

- Revised structure of ``namelist_ref`` / ``namelist_cfg`` and default reference values
- Lateral physics (``LDF``): simplification of user interface and removal of CPP keys
- Vertical physics (``ZDF``) (modularity, share shear production calculation between TKE and GKS, removal of all ZDF CPP keys, removal of ``avmu`` & ``avmv``, minimization of MPP comm.: ~15 removed)
- Remove the split-explicit ZDF scheme for both ``TRA`` and ``DYN``
- Remove the acceleration of convergence
- Generalised ``lbc_lnk`` and ``lbc_nfd``
- Unify mppini
- Use non uniform jpi/jpj with dynamic allocation to avoid ghost rows/columns
- MPI Message passing re coded
- Configuration interface completely rewritten (``DOM`` module mainly suppressed , and in place: ``domain_cfg.nc`` file, or ``usr_def`` module) 

Collaborative Development Environment
=====================================

- Access to information on wiki reorganised through portals for users/developers/System Team and complete refactoring of all pages and their layout
- Reorganisation of SVN repository to be compliant with usual directory tree and facilitate building of the executable
- Define and install a separate repository for test cases to all easy contributions from the community
- :forge:`Forums<discussion>` created
- :website:`Public web site<>` has been revamped and cleaned using Wordpress
- New mailing lists have been set up
- Improvements of reliability through automatic and regular testing of the changes made in repository 
