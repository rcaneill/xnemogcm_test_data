FC=gfortran
FFLAGS=  -O0 -fdefault-real-8  -ffree-line-length-none -I$(NCDF_ROOT)/include
FFLAGS_SPEC=  -J$(DIR_OBJS)
#-fbounds-check  -fbacktrace -g -fcheck=all -Wall
CPP=cpp
CPPFLAGS=-P -traditional 
CPPARCH = 
CPPFLAGS += $(CPPARCH) 

CC=mpicc
CFLAGS = -Df2cFortran 

NCDF_ROOT=/usr/local
INC_NETCDF = -I$(NCDF_ROOT)/include
LIB_NETCDF= -L$(NCDF_ROOT)/lib/ -lnetcdf -lnetcdff

INCS = $(INC_NETCDF) 
LIBS =$(LIB_NETCDF)  

# language-specific flags
#
F77FLG =  -ffixed-form # -Mfixed
F90FLG =  -ffree-form -ffree-line-length-0 # -Mfree

MPI = NO

