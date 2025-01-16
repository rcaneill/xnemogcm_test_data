#! /bin/bash

# Use: run this script once and then git commit

mkdir -p sources
cd sources


mkdir -p ZLIB
cd ZLIB
LIB_VERSION="zlib-1.2.13"
LIB_FILE="${LIB_VERSION}.tar.gz"
wget https://www.zlib.net/fossils/${LIB_FILE}
tar xvfz $LIB_FILE
rm $LIB_FILE
mv $LIB_VERSION zlib
cd ..


mkdir -p HDF5
cd HDF5
LIB_VERSION="hdf5-1.10.5"
LIB_FILE="${LIB_VERSION}.tar.gz"
wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.5/src/hdf5-1.10.5.tar.gz -O $LIB_FILE
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
LIB_VERSION="4.6.0"
LIB_FILE="v${LIB_VERSION}.tar.gz"
wget https://github.com/Unidata/netcdf-fortran/archive/${LIB_FILE}
tar xvfz $LIB_FILE
rm $LIB_FILE
mv netcdf-fortran-${LIB_VERSION} netcdf-fortran
cd ..

mkdir -p XIOS
cd XIOS
svn co -r 2481 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-2.5 xios-2.5
rm -rf $(find . -iname .svn)
cd ..

mkdir -p NEMO
cd NEMO
svn co https://forge.ipsl.jussieu.fr/nemo/svn/NEMO/releases/release-4.0 NEMOGCM
rm -rf $(find . -iname .svn)
cd ..
