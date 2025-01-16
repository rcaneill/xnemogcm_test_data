#! /usr/bin/python
# ======================================================================
#                        ***  TOOL diamlr.py  ***
# Postprocessing of intermediate NEMO model output for
# multiple-linear-regression analysis (diamlr)
# ======================================================================
# History :      !  2019  (S. Mueller)
# ----------------------------------------------------------------------
import sys
# ----------------------------------------------------------------------
# NEMO/TOOLS 4.0 , NEMO Consortium (2019)
# $Id$
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#                    ***  SUBROUTINE get_args  ***
# Parse command line arguments
# ----------------------------------------------------------------------
def get_args():
    from argparse import ArgumentParser
    import re

    # Set up command-line argument parser
    parser = ArgumentParser(
        description='Postprocessing of intermediate NEMO model output'+
                    ' for multiple-linear-regression analysis (diamlr)')
    parser.add_argument('--file_scalar', help=
                        'Filename of scalar intermediate NEMO model'+
                        ' output for multiple-linear-regression'+
                        ' analysis')
    parser.add_argument('--file_grid', help=
                        'Filename of gridded intermediate NEMO model'+
                        ' output for multiple-linear-regression'+
                        ' analysis')
    parser.add_argument('--regressors', nargs='+', required=False,
                        help='Optional list of regressors to include'+
                        ' in analysis; if omitted, all available'+
                        ' regressors will be included')
    args = parser.parse_args()
    return args

# ----------------------------------------------------------------------
#                    ***  SUBROUTINE main  ***
# Finalisation of multiple-linear-regression analysis
# ----------------------------------------------------------------------
def main():
    from netCDF4 import Dataset as nc
    import re
    import numpy as np
    from os.path import basename, splitext
    from time import strftime, localtime

    # Get command-line arguments
    args = get_args()

    # Get filenames/locations of intermdiate diamlr output
    fn_scalar = args.file_scalar
    fn_grid   = args.file_grid

    # Open regressor-regressor scalar-product data set
    f = nc(fn_scalar, 'r')

    # Detect available regressors; reduce list of regressors according
    # to list of selected regressors (if specified)
    regs = {}
    vn_re = re.compile('(diamlr_r[0-9]{3})\.(diamlr_r[0-9]{3})')
    for vn in f.variables.keys():
        vn_match = vn_re.match(vn)
        if (vn_match):
            reg1 = vn_match.group(1)
            reg2 = vn_match.group(2)
            if not args.regressors or reg1 in args.regressors:
                regs[vn_match.group(1)] = True
            if not args.regressors or reg2 in args.regressors:
                regs[vn_match.group(2)] = True
    regs = regs.keys()
    regs.sort()

    print('Compile and invert matrix of regressor-regressor scalar'+
          'products ...')

    # Set up square matrix, XX, of regressor-regressor scalar products
    xx = np.matrix(np.zeros((len(regs), len(regs))))
    for i1 in range(len(regs)):
        vn1 = regs[i1]
        for i2 in range(regs.index(vn1)+1):
            vn2 = regs[i2]
            if f.variables[vn1+'.'+vn2]:
                xx_sum = np.sum(f.variables[vn1+'.'+vn2][:])
            else:
                xx_sum = np.sum(f.variables[vn2+'.'+vn1][:])
            xx[i1,i2] = xx_sum
            if i1 != i2:
                xx[i2,i1] = xx_sum

    # Close regressor-regressor scalar-product data set
    f.close()

    # Compute inverse matrix, XX^-1; convert matrix to an array to
    # enable the dot-product computation of XX with a large array below
    ixx = np.array(xx**-1)

    print('   ... done')

    # Open field-regressor scalar-product data set
    f = nc(fn_grid, 'r')

    # Detect analysed fields
    flds = {}
    vn_re = re.compile('(diamlr_f[0-9]{3})\.(diamlr_r[0-9]{3})')
    for vn in f.variables.keys():
        vn_match = vn_re.match(vn)
        if (vn_match):
            if vn_match.group(2) in regs:
                flds[vn_match.group(1)] = True
    flds = flds.keys()
    flds.sort()

    # Open and prepare output file, incl. replication of
    # domain-decomposition metadata (if present) from the
    # field-regressor data set
    fn_out = './'+basename(fn_grid)
    if fn_out.find('diamlr') > 0:
        fn_out = fn_out.replace('diamlr', 'diamlr_coeffs')
    else:
        fn_parts = splitext(basename(fn_grid))
        fn_out = fn_parts[0]+'_diamlr_coeffs'+fn_parts[1]
    nc_out = nc(fn_out, 'w', format='NETCDF4', clobber=False)
    nc_atts = {
        'name'        : fn_out,
        'description' : 'Multiple-linear-regression analysis output',
        'title'       : 'Multiple-linear-regression analysis output',
        'timeStamp'   : strftime('%Y-%m-%d %H:%M:%S %Z', localtime())}
    for nc_att in f.ncattrs():
        if nc_att in ['ibegin', 'jbegin',
                      'ni', 'nj'] or nc_att.startswith('DOMAIN'):
           nc_atts[nc_att] = f.getncattr(nc_att)
    nc_out.setncatts(nc_atts)
    for nc_dimname in f.dimensions.keys():
        nc_dim = f.dimensions[nc_dimname]
        if nc_dim.isunlimited():
            nc_out.createDimension(nc_dim.name)
        else:
            nc_out.createDimension(nc_dim.name, nc_dim.size)

    # Read in fields of scalar products of model diagnostics and
    # regressors and compute the regression coefficients for the current
    # field; add resulting fields to output file
    for fld in flds:
        print('Completing analysis for field '+fld+' ...')
        xy = np.array([])
        for reg in regs:
            if xy.size == 0:
                xy = np.sum(f.variables[fld+'.'+reg][:],
                            axis=0)[np.newaxis,:]
            else:
                xy = np.r_[xy, np.sum(f.variables[fld+'.'+reg][:],
                                      axis=0)[np.newaxis,:]]
        b=np.reshape(np.dot(ixx, np.reshape(xy,(len(xy),xy[0,:].size))),
                     (xy.shape))
        print('   ... done')

        for reg in regs:
            nr = regs.index(reg)
            nc_gridvar = f.variables[fld+'.'+reg]
            name = nc_gridvar.name.split('.')
            nc_var = nc_out.createVariable(name[0]+'-'+name[1],
                                           nc_gridvar.datatype,
                                           nc_gridvar.dimensions)
            for nc_att in nc_gridvar.ncattrs():
                if nc_att in ['_FillValue', 'missing_value']:
                    nc_var.setncattr(nc_att, nc_gridvar.getncattr(
                        nc_att))
            name = nc_gridvar.getncattr('standard_name').split('.')
            nc_var.setncattr(
                'standard_name', name[0]+' regressed on '+name[1])
            nc_var[0,:] = b[nr,:].data

    # Close output file; close field-regressor scalar-product data set
    nc_out.close()
    f.close()

# ----------------------------------------------------------------------
#                     ***  main PROGRAM  ***
# ----------------------------------------------------------------------
if __name__ == "__main__":

    main()
