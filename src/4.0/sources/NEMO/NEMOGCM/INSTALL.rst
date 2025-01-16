*******************
Build the framework
*******************

.. contents::
   :local:

Prerequisites
=============

| The NEMO source code is written in *Fortran 95* and
  some of its prerequisite tools and libraries are already included in the ``./ext`` subdirectory.
| It contains the AGRIF_ preprocessing program ``conv``; the FCM_ build system and
  the IOIPSL_ library for parts of the output.

System dependencies
-------------------

In the first place the other requirements should be provided natively by your system or
can be installed from the official repositories of your Unix-like distribution:

- *Perl* interpreter
- *Fortran* compiler (``ifort``, ``gfortran``, ``pgfortran``, ...),
- *Message Passing Interface (MPI)* implementation (e.g. |OpenMPI|_ or |MPICH|_).
- |NetCDF|_ library with its underlying |HDF|_ 

**NEMO, by default, takes advantage of some MPI features introduced into the MPI-3 standard.**

.. hint::

   The MPI implementation is not strictly essential
   since it is possible to compile and run NEMO on a single processor.
   However most realistic configurations will require the parallel capabilities of NEMO and
   these use the MPI standard.

.. note::

   On older systems, that do not support MPI-3 features,
   the ``key_mpi2`` preprocessor key should be used at compile time.
   This will limit MPI features to those defined within the MPI-2 standard
   (but will lose some performance benefits).

Specifics for NetCDF and HDF
----------------------------

NetCDF and HDF versions from .
However access to all the options available with the XIOS IO-server will require
the parallel IO support of these libraries which can be unavailable.

| **To satisfy these requirements, it is common to have to compile from source
  in this order HDF (C library) then NetCDF (C and Fortran libraries)**
| It is also necessary to compile these libraries with the same version of the MPI implementation that
  both NEMO and XIOS (see below) are compiled and linked with.

.. hint::

   | It is difficult to define the options for the compilation as
     they differ from one architecture to another according to
     the hardware used and the software installed.
   | The following is provided without any warranty

   .. code-block:: console

      $ ./configure [--{enable-fortran,disable-shared,enable-parallel}] ...

   It is recommended to build the tests ``--enable-parallel-tests`` and run them with ``make check``

Particular versions of these libraries may have their own restrictions.
State the following requirements for netCDF-4 support:

.. caution::

   | When building NetCDF-C library versions older than 4.4.1, use only HDF5 1.8.x versions.
   | Combining older NetCDF-C versions with newer HDF5 1.10 versions will create superblock 3 files
     that are not readable by lots of older software.
   
Extract and install XIOS
========================

With the sole exception of running NEMO in mono-processor mode
(in which case output options are limited to those supported by the ``IOIPSL`` library),
diagnostic outputs from NEMO are handled by the third party ``XIOS`` library.
This can be used in two different modes:

- *attached* - Every NEMO process also acts as a XIOS server
- *detached* - Every NEMO process runs as a XIOS client.
  Output is collected and collated by external, stand-alone XIOS server processors.

.. important::

   In either case, XIOS needs to be compiled before NEMO,
   since the libraries are needed to successfully create the NEMO executable.

Instructions on how to obtain and install the software can be found on the :xios:`XIOS wiki<wiki>`.

.. hint::

   It is recommended to use XIOS version 2.5.
   This version should be more stable (in terms of future code changes) than the XIOS trunk.
   It is also the version used by the NEMO system team when testing all developments and new releases.
   
   This particular version has its own branch and can be checked out and downloaded with:

   .. code-block:: console

      $ svn co https://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-2.5

Download the NEMO source code
=============================

.. code-block:: console

   $ svn co https://forge.ipsl.jussieu.fr/nemo/svn/NEMO/releases/release-4.0

Description of directory tree
-----------------------------

+-----------+------------------------------------------------------------+
| Folder    | Purpose                                                    |
+===========+============================================================+
| ``arch``  | Settings (per architecture-compiler pair)                  |
+-----------+------------------------------------------------------------+
| ``cfgs``  | :doc:`Reference configurations <configurations>`           |
+-----------+------------------------------------------------------------+
| ``doc``   | - ``latex``    : LaTex source code for ref. manuals        |
|           | - ``namelists``: k start guide                             |
|           | - ``rst``      : ReST files for quick start guide          |
+-----------+------------------------------------------------------------+
| ``ext``   | Dependencies included (``AGRIF``, ``FCM`` & ``IOIPSL``)    |
+-----------+------------------------------------------------------------+
| ``mk``    | Building  routines                                         |
+-----------+------------------------------------------------------------+
| ``src``   | Modelling routines                                         |
|           |                                                            |
|           | - ``ICE``: |SI3| for sea ice                               |
|           | - ``NST``: AGRIF for embedded zooms                        |
|           | - ``OCE``: |OPA| for ocean dynamics                        |
|           | - ``TOP``: |TOP| for tracers                               |
+-----------+------------------------------------------------------------+
| ``tests`` | :doc:`Test cases <test_cases>` (unsupported)               |
+-----------+------------------------------------------------------------+
| ``tools`` | :doc:`Utilities <tools>` to [pre|post]process data         |
+-----------+------------------------------------------------------------+

Setup your architecture configuration file
==========================================

All compiler options in NEMO are controlled using files in
``./arch/arch-'my_arch'.fcm`` where 'my_arch' is the name of the computing
architecture.  It is recommended to copy and rename an configuration file from
an architecture similar to your owns. You will need to set appropriate values
for all of the variables in the file. In particular the FCM variables:
``%NCDF_HOME``; ``%HDF5_HOME`` and ``%XIOS_HOME`` should be set to the
installation directories used for XIOS installation.

.. code-block:: sh

        %NCDF_HOME           /opt/local
        %HDF5_HOME           /opt/local
        %XIOS_HOME           /Users/$( whoami )/xios-2.5
        %OASIS_HOME          /not/defined

Compile and create NEMO executable
==================================

The main script to compile and create executable is called makenemo and located in the CONFIG directory, it is used to identify the routines you need from the source code, to build the makefile and run it.
As an example, compile GYRE with 'my_arch' to create a 'MY_GYRE' configuration:

.. code-block:: sh

	./makenemo –m 'my_arch' –r GYRE -n 'MY_GYRE'

The image below shows the structure and some content of "MY_CONFIG" directory from the launching of the configuration creation (directories and fundamental files created by makenemo).

+------------+----------------------------------------------------+
| Folder     | Purpose                                            |
+============+====================================================+
| ``BLD``    |                                                    |
+------------+----------------------------------------------------+
| ``EXP00``  |                                                    |
+------------+----------------------------------------------------+
| ``EXPREF`` |                                                    |
+------------+----------------------------------------------------+
| ``MY_SRC`` |                                                    |
+------------+----------------------------------------------------+
| ``WORK``   |                                                    |
+------------+----------------------------------------------------+

Folder with the symbolic links to all unpreprocessed routines considered in the configuration
Compilation folder (executables, headers files, libraries, preprocessed routines, flags, …)
Computation folder for running the model (namelists, xml, executables and inputs-outputs)
Folder intended to contain your customised routines (modified from initial ones or new entire routines)

After successful execution of makenemo command, the executable called opa is created in the EXP00 directory (in the example above, the executable is created in CONFIG/MY_GYRE/EXP00).

More makenemo options
---------------------

``makenemo`` has several other options that can control which source files are selected and
the operation of the build process itself.
These are::

   Optional:
      -d  Set of new sub-components (space separated list from ./src directory)
      -e  Path for alternative patch  location (default: 'MY_SRC' in configuration folder)
      -h  Print this help
      -j  Number of processes to compile (0: no build)
      -n  Name for new configuration
      -s  Path for alternative source location (default: 'src' root directory)
      -t  Path for alternative build  location (default: 'BLD' in configuration folder)
      -v  Level of verbosity ([0-3])

These options can be useful for maintaining several code versions with only minor differences but
they should be used sparingly.
Note however the ``-j`` option which should be used more routinely to speed up the build process.
For example:

.. code-block:: sh

        ./makenemo –m 'my_arch' –r GYRE -n 'MY_GYRE' -j 8

which will compile up to 8 modules simultaneously.


Default behaviour
-----------------

At the first use, you need the -m option to specify the architecture
configuration file (compiler and its options, routines and libraries to
include), then for next compilation, it is assumed you will be using the
same compiler.  If the –n option is not specified the last compiled configuration
will be used.

Tools used during the process
-----------------------------

*   functions.sh : bash functions used by makenemo, for instance to create the WORK directory
*   cfg.txt : text list of configurations and source directories
*   bld.cfg : FCM rules to compile 

Examples
--------

.. code-block:: sh

        echo "Example to install a new configuration MY_CONFIG";
        echo "with OPA_SRC and LIM_SRC_2 ";
        echo "makenemo -n MY_CONFIG -d \"OPA_SRC LIM_SRC_2\"";
        echo "";
        echo "Available configurations :"; cat ${CONFIG_DIR}/cfg.txt;
        echo "";
        echo "Available unsupported (external) configurations :"; cat ${CONFIG_DIR}/uspcfg.txt;
        echo "";
        echo "Example to remove bad configuration ";
        echo "./makenemo -n MY_CONFIG clean_config";
        echo "";
        echo "Example to clean ";
        echo "./makenemo clean";
        echo "";
        echo "Example to list the available keys of a CONFIG ";
        echo "./makenemo list_key";
        echo "";
        echo "Example to add and remove keys";
        echo "./makenemo add_key \"key_iomput key_mpp_mpi\" del_key \"key_agrif\" ";
        echo "";
        echo "Example to add and remove keys for a new configuration, and do not compile";
        echo "./makenemo -n MY_CONFIG -j0 add_key \"key_iomput key_mpp_mpi\" del_key \"key_agrif\" ";

Running the model
=================

Once makenemo has run successfully, the opa executable is available in ``CONFIG/MY_CONFIG/EXP00``
For the reference configurations, the EXP00 folder also contains the initial input files (namelists, \*xml files for the IOs…). If the configuration also needs NetCDF input files, this should be downloaded here from the corresponding tar file, see Users/Reference Configurations

.. code-block:: sh

        cd 'MY_CONFIG'/EXP00
        mpirun -n $NPROCS ./opa    # $NPROCS is the number of processes ; mpirun is your MPI wrapper


Viewing and changing list of active CPP keys
============================================

For a given configuration (here called MY_CONFIG), the list of active CPP keys can be found in:

.. code-block:: sh

        ./cfgs/'MYCONFIG'/cpp_'MY_CONFIG'.fcm


This text file can be edited to change the list of active CPP keys. Once changed, one needs to recompile opa executable using makenemo command in order for this change to be taken in account.
Note that most NEMO configurations will need to specify the following CPP keys:
``key_iomput`` and ``key_mpp_mpi``

.. Links and substitutions

.. |OpenMPI| replace:: *OpenMPI*
.. _OpenMPI: https://www.open-mpi.org
.. |MPICH|   replace:: *MPICH*
.. _MPICH:   https://www.mpich.org
.. |NetCDF|  replace:: *Network Common Data Form (NetCDF)*
.. _NetCDF:  https://www.unidata.ucar.edu/downloads/netcdf
.. |HDF|     replace:: *Hierarchical Data Form (HDF)*
.. _HDF:     https://www.hdfgroup.org/downloads
