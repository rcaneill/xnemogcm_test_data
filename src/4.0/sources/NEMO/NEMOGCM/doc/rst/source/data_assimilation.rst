*****************
Data assimilation
*****************

.. contents::
   :local:

The assimilation interface to NEMO is split into three modules.
- OBS for the observation operator
- ASM for the application of increments and model bias correction (based on the assimilation increments).
- TAM the tangent linear and adjoint model.

Please see the `NEMO manual`_ for more details including information about the input file formats and
the namelist settings.

Observation and model comparison (OBS)
======================================

The observation and model comparison code (OBS) reads in observation files (profile temperature and salinity,
sea surface temperature, sea level anomaly, sea ice concentration, and velocity) and
calculates an interpolated model equivalent value at the observation location and nearest model timestep.
The resulting data are saved in a feedback file (or files).
The code was originally developed for use with the NEMOVAR data assimilation code, but
can be used for validation or verification of model or any other data assimilation system.
This is all controlled by the namelist.
To build with the OBS code active ``key_diaobs`` must be set. 

More details in the `NEMO manual`_ chapter 12.

Standalone observation operator (SAO)
-------------------------------------

The OBS code can also be run after a model run using saved NEMO model data.
This is accomplished using the standalone observation operator (SAO)
(previously known the offline observation operator).

To build the SAO use makenemo.
This means compiling NEMO once (in the normal way) for the chosen configuration.
Then include ``SAO`` at the end of the relevant line in ``cfg.txt`` file.
Then recompile with the replacement main program in ``./src/SAO``.
This is a special version of ``nemogcm.F90`` (which doesn't run the model, but reads in the model fields, and
observations and runs the OBS code.
See section 12.4 of the `NEMO manual`_.

Apply assimilation increments (ASM)
===================================

The ASM code adds the functionality to apply increments to the model variables:
temperature, salinity, sea surface height, velocity and sea ice concentration.
These are read into the model from a NetCDF file which may be produced by separate data assimilation code.
The code can also output model background fields which are used as an input to data assimilation code.
This is all controlled by the namelist nam_asminc.
To build the ASM code ``key asminc`` must be set.

More details in the `NEMO manual`_ chapter 13.

Tangent linear and adjoint (TAM)
================================

This is the tangent linear and adjoint code of NEMO which is useful to 4D VAR assimilation.
