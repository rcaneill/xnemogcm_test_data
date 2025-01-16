#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==============
# Fprep_agrif.sh
# ==============
#
# ---------------------
# Preparation for AGRIF
# ---------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fprep_agrif.sh
#
#
# DESCRIPTION
# ===========
#
#
# Prepare directories for AGRIF and copy files needed
#
# Compile the conv
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fprep_agrif.sh CONFIG_NAME
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: Fprep_agrif.sh 6204 2016-01-04 13:47:06Z cetlod $
#
#
#
#   * creation
#
#-

#- AGRIF conv
if [ "$AGRIFUSE" == 1 ]; then
#-MPI for AGRIF
if [ ! -f ${MAIN_DIR}/EXTERNAL/AGRIF/nemo_mpi.h ];then
   echo '#if defined key_mpp_mpi' > ${MAIN_DIR}/EXTERNAL/AGRIF/nemo_mpi.h
   echo '#define AGRIF_MPI'      >> ${MAIN_DIR}/EXTERNAL/AGRIF/nemo_mpi.h
   echo '#endif'                 >> ${MAIN_DIR}/EXTERNAL/AGRIF/nemo_mpi.h
fi

#- CONV
fcm build ${COMPIL_DIR}/conv.cfg || exit 1
#C_COMPILER=${CC-cc}
#gmake CC=${C_COMPILER} -C ${MAIN_DIR}/EXTERNAL/AGRIF/LIB

#- AGRIF sources
[ ! -d $2/$1/OPAFILES ] && mkdir  $2/$1/OPAFILES
[ ! -d $2/$1/OPAFILES/AGRIF_INC ] && mkdir  $2/$1/OPAFILES/AGRIF_INC
[ ! -d $2/$1/OPAFILES/AGRIF_MODELFILES ] && mkdir  $2/$1/OPAFILES/AGRIF_MODELFILES
cp -f -r ${MAIN_DIR}/EXTERNAL/AGRIF/agrif_opa.in  $2/$1/OPAFILES/
#cp -f -r ${MAIN_DIR}/EXTERNAL/AGRIF/conv  $2/$1/OPAFILES/
cp -f -r $2/$1/AGRIFLIB/bin/conv  $2/$1/OPAFILES/

fi
