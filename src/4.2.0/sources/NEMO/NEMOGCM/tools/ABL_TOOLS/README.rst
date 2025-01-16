ABL_TOOLS
---------

3 steps to generate atmospheric forcings from ECMWF products for ABL1d model with NEMO:
- main_uvg_hpg (optional): geostrophic wind or horizontal pressure gradient computation on ECMWF eta-levels (to force ABL dynamics)
- main_vinterp: vertical interpolation from ECWMF vertical eta-levels to ABL Z-levels
- main_hdrown: 3D-fields horizontal drowning (extrapolation over land totally inspired from SOSIE by L. Brodeau) 

Mandatory ECMWF fields: temperature (T), specific humidity (Q), mean sea-level pressure (MSL), wind x-component (U) and wind y-component (V)
Optional ECMWF fields: surface geopotential (Z)
Reference: Lemarie et al. 2020 GMD

It is possible to use only necessary ECMWF vertical levels that cover the ABL height.
For example with ERAI reanalysis, vertical levels from near-surface (L60) to 3100m (L44) are enough to generate forcings for ABL height up to 3000m.
More informations about ECMWF vertical levels altitude are available here: https://www.ecmwf.int/en/forecasts/documentation-and-support/60-model-levels

ECMWF ERAI 6h (analysis) forcings on native horizontal grid (Gaussian F128) for the ABL model (50 vertical levels between 10m and 2000m) can be delivered upon request.
Contact: gsamson@mercator-ocean.fr
