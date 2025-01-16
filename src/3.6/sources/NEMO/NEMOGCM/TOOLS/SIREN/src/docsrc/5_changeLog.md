# Change log

@tableofcontents

# Release $Date:: 2016-11-16 #$ ($Revision: 7235 $)

## New Features
- merge_bathy.f90: allow to choose the number of boundary point with coarse grid value.
- dimension.f90: dimension allowed read in configuration file.
- variable.f90: allow to add scalar value.
## Changes
- create_coord.f90: allow to define sub domain with coarse grid indices or coordinates.
- grid.f90:grid__get_closest_str: add function to get closest grid point using coarse grid coordinates strucutre.
- iom_cdf.f90:iom_cdf__get_info: define cdf4 as cdf.
- variable.f90: add subroutine to clean global array of extra information, and define logical for variable to be used.
- create_coord.f90: dimension to be used select from configuration file.
- create_bathy.f90: dimension to be used select from configuration file.
- merge_bathy.f90: dimension to be used select from configuration file.
- create_boundary.f90: dimension to be used select from configuration file.
- create_restart.f90: dimension to be used select from configuration file.
## Bug fixes
- boundary.f90:boundary_check: take into account that boundaries are compute on T point, but expressed on U,V point.
- grid.f90:grid__get_closest_str: use max of zero and east-west overlap instead of east-west overlap.
- mpp.f90: compare index to td_lay number of proc instead of td_mpp (bug fix) .
- iom_cdf.f90 : check if attribute cl_value is not bug (in read file)

# Release 2016-07-01

## Changes
## New Features
## Bug fixes
 - correct check of boundary indices

# Initial Release 2016-04-11

 <HR>
   <b>
   - @ref index
   - @ref md_src_docsrc_1_install
   - @ref md_src_docsrc_2_quickstart
   - @ref md_src_docsrc_3_support_bug
   - @ref md_src_docsrc_4_codingRules
   - @ref todo
   </b>
