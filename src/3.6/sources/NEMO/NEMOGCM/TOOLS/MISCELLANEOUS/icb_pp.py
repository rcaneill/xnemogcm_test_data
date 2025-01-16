from netCDF4 import Dataset
from argparse import ArgumentParser
import numpy as np
import sys

#
# Basic iceberg trajectory post-processing python script.
# This script collates iceberg trajectories from the distributed datasets written
# out by each processing region and rearranges the ragged arrays into contiguous
# streams for each unique iceberg. The output arrays are 2D (ntraj, ntimes) arrays.
# Note that some icebergs may only exist for a subset of the possible times. In these
# cases the missing instances are filled with invalid (NaN) values.
#

parser = ArgumentParser(description='produce collated trajectory file from distributed output\
                                     files, e.g. \n python ./icb_pp.py \
                                     -t  trajectory_icebergs_004248_ -n 296 -o trajsout.nc' )

parser.add_argument('-t',dest='froot',help='fileroot_of_distrbuted_data; root name of \
                     distributed trajectory output (usually completed with XXXX.nc, where \
                     XXXX is the 4 digit processor number)', 
                     default='trajectory_icebergs_004248_')

parser.add_argument('-n',dest='fnum',help='number of distributed files to process', 
                     type=int, default=None)

parser.add_argument('-o',dest='fout',help='collated_output_file; file name to receive the \
                     collated trajectory data', default='trajsout.nc')

args = parser.parse_args()

default_used = 0
if args.froot is None:
    pathstart = 'trajectory_icebergs_004248_'
    default_used = 1
else:
    pathstart = args.froot

if args.fnum is None:
    procnum = 0
    default_used = 1
else:
    procnum = args.fnum

if args.fout is None:
    pathout = 'trajsout.nc'
    default_used = 1
else:
    pathout = args.fout

if default_used == 1:
   print('At least one default value will be used; command executing is:')
   print('icb_pp.py -t ',pathstart,' -n ',procnum,' -o ',pathout)

if procnum < 1:
   print('Need some files to collate! procnum = ',procnum)
   sys.exit(11)

icu = []
times = []
#
# Loop through all distributed datasets to obtain the complete list
# of iceberg identification numbers and timesteps
#
for n in range(procnum):
 nn = '%4.4d' % n
 fw = Dataset(pathstart+nn+'.nc')
 if len(fw.dimensions['n']) > 0:
   print pathstart+nn+'.nc'
   ic = fw.variables['iceberg_number'][:,0]
   ts = fw.variables['timestep'][:]
   icv = np.unique(ic)
   ts = np.unique(ts)
   print('Min Max ts: ',ts.min(), ts.max())
   print('Number unique icebergs= ',icv.shape[0])
   icu.append(icv)
   times.append(ts)
 fw.close()
#
# Now flatten the lists and reduce to the unique spanning set
#
icu = np.concatenate(icu)
icu = np.unique(icu)
times = np.concatenate(times)
times = np.unique(times)
ntraj = icu.shape[0]
print(ntraj, ' unique icebergs found across all datasets')
print('Icebergs ids range from: ',icu.min(), 'to: ',icu.max())
print('times range from:        ',times.min(), 'to: ', times.max())
#
# Declare 2-D arrays to receive the data from all files
#
nt = times.shape[0]
lons = np.zeros((ntraj, nt))
lats = np.zeros((ntraj, nt))
tims = np.zeros((ntraj, nt))
xis  = np.zeros((ntraj, nt))
yjs  = np.zeros((ntraj, nt))
#
# initially fill with invalid data
#
lons.fill(np.nan)
lats.fill(np.nan)
xis.fill(np.nan)
yjs.fill(np.nan)
tims.fill(np.nan)
#
# loop through distributed datasets again, this time
# checking indices against icu and times lists and
# inserting data into the correct locations in the 
# 2-D collated sets.
#
for n in range(procnum):
 nn = '%4.4d' % n
 fw = Dataset(pathstart+nn+'.nc')
# Note many distributed datafiles will contain no iceberg data
# so skip quickly over these
 m  = len(fw.dimensions['n'])
 if m > 0:
   inx = np.zeros(m, dtype=int)
   tsx = np.zeros(m, dtype=int)
   print pathstart+nn+'.nc'
   ic = fw.variables['iceberg_number'][:,0]
   ts = fw.variables['timestep'][:]
   lns = fw.variables['lon'][:]
   lts = fw.variables['lat'][:]
   xxs = fw.variables['xi'][:]
   yys = fw.variables['yj'][:]
   for k in range(m):
     inxx   = np.where(icu == ic[k])
     inx[k] = inxx[0]
   for k in range(m):
     inxx   = np.where(times == ts[k])
     tsx[k] = inxx[0]
   lons[inx[:],tsx[:]] = lns[:]
   lats[inx[:],tsx[:]] = lts[:]
   tims[inx[:],tsx[:]] = ts[:]
   xis[inx[:],tsx[:]] = xxs[:]
   yjs[inx[:],tsx[:]] = yys[:]
 fw.close()

# Finally create the output file and write out the collated sets
#
fo = Dataset(pathout, 'w', format='NETCDF4')
ntrj = fo.createDimension('ntraj', ntraj)
nti  = fo.createDimension('ntime', None)
olon = fo.createVariable('lon', 'f4',('ntraj','ntime'))
olat = fo.createVariable('lat', 'f4',('ntraj','ntime'))
otim = fo.createVariable('ttim', 'f4',('ntraj','ntime'))
oxis = fo.createVariable('xis', 'f4',('ntraj','ntime'))
oyjs = fo.createVariable('yjs', 'f4',('ntraj','ntime'))
icbn = fo.createVariable('icbn', 'f4',('ntraj'))
olon[:,:] = lons
olat[:,:] = lats
otim[:,:] = tims
oxis[:,:] = xis
oyjs[:,:] = yjs
icbn[:] = icu
fo.close()
