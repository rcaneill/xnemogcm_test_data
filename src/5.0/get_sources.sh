#! /bin/bash

# Use: run this script once and then git commit

mkdir sources
cd sources


mkdir -p ZLIB
cd ZLIB
LIB_VERSION="zlib-1.3.1"
LIB_FILE="${LIB_VERSION}.tar.gz"
wget https://www.zlib.net/${LIB_FILE}
tar xvfz $LIB_FILE
rm $LIB_FILE
mv $LIB_VERSION zlib
cd ..


mkdir -p HDF5
cd HDF5
LIB_VERSION="hdf5-1.14.5"
LIB_FILE="${LIB_VERSION}.tar.gz"
wget 'https://www.hdfgroup.org/download/hdf5-1-14-5-tar-gz/?ind=1727799446135&filename=hdf5-1.14.5.tar.gz&wpdmdl=25746&refresh=6784eaabf0f421736764075' -O $LIB_FILE
tar xvfz $LIB_FILE
rm $LIB_FILE
mv $LIB_VERSION hdf5
cd ..

mkdir -p NetCDF-c
cd NetCDF-c
LIB_VERSION="4.9.2"
LIB_FILE="v${LIB_VERSION}.tar.gz"
wget https://github.com/Unidata/netcdf-c/archive/${LIB_FILE}
tar xvfz $LIB_FILE
rm $LIB_FILE
mv netcdf-c-${LIB_VERSION} netcdf-c
cd ..

mkdir -p NetCDF-fortran
cd NetCDF-fortran
LIB_VERSION="4.6.1"
LIB_FILE="v${LIB_VERSION}.tar.gz"
wget https://github.com/Unidata/netcdf-fortran/archive/${LIB_FILE}
tar xvfz $LIB_FILE
rm $LIB_FILE
mv netcdf-fortran-${LIB_VERSION} netcdf-fortran
cd ..

mkdir -p XIOS
cd XIOS
svn co -r 2701 http://forge.ipsl.fr/ioserver/svn/XIOS/trunk xios-trunk
cd ..

mkdir -p NEMO
cd NEMO
git clone --branch 5.0 https://forge.nemo-ocean.eu/nemo/nemo.git NEMOGCM
rm -rf NEMOGCM/tools
cd ..
