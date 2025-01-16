********
Coupling
********

NEMO currently exploits OASIS-3-MCT (versions 1 to 4) to implement a generalised coupled interface
(`Coupled Formulation <http://forge.ipsl.jussieu.fr/nemo/doxygen/node50.html?doc=NEMO>`_).
It can be used to interface with most of the European atmospheric GCM (ARPEGE, ECHAM, ECMWF, Ha- dAM, HadGAM, LMDz), as well as to regional atmospheric models (WRF, COSMO, Meso-NH, AROME).
To implement the coupling of two independent NEMO components, ocean on one hand and sea-ice plus other surface processes on the other hand
(`Standalone Surface Module - SAS <http://forge.ipsl.jussieu.fr/nemo/doxygen/node46.html?doc=NEMO>`_).

To enable the OASIS interface the required compilation key is ``key_oasis3``. You must also add ``key_oasis3_v1v2`` if you use OASIS-3-MCT version 1 and 2.
The parameters to set are in sections ``namsbc_cpl`` and in case of using of SAS also in section ``namsbc_sas``.
