#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==========
# agrifpp.sh
# ==========
#
# ----------------------------
# Preform AGrif pre-processing
# ----------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ agrifpp.sh
#
#
# DESCRIPTION
# ===========
#
#
# Preprocess file using the conv in NEMOFILES directory
# Standard preprocessed files are stored in NEMOFILES/ppsrc/nemo
# Source files are stored under NEMOFILES/obj
# Include filess  in NEMOFILES/inc
# Note that agrif2model.F90 should not be preprocess (standard one) 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./agrifpp.sh FILE_TO_PROCESS
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
# $Id: agrifpp.sh 2143 2010-10-04 12:49:55Z rblod $
#
#
#
#   * creation
#
#-
MYDIR=$1
MYFILE=$(basename "$2")
if [ "$MYFILE" == "agrif2model.f90" ];then
   if [ -d ${MYDIR}/${NEW_CONF}/WORK ]; then
      \cp ${MYDIR}/${NEW_CONF}/WORK/${MYFILE/.f90/.F90} ${MYDIR}/${NEW_CONF}/NEMOFILES/obj/$MYFILE
   else
      \cp ${MYDIR}/${NEW_CONF}/src/${MYFILE/.f90/.F90} ${MYDIR}/${NEW_CONF}/NEMOFILES/obj/$MYFILE
   fi   
else
cd ${MYDIR}/${NEW_CONF}/NEMOFILES/ppsrc/nemo ; ${MYDIR}/${NEW_CONF}/NEMOFILES/conv ${MYDIR}/${NEW_CONF}/NEMOFILES/agrif_oce.in -rm -incdir ${MYDIR}/${NEW_CONF}/NEMOFILES/inc -comdirout ${MYDIR}/${NEW_CONF}/NEMOFILES/obj -convfile ${MYFILE} > /dev/null 
fi