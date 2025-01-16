#!/bin/bash
######################################################
# Author : Rachid Benshila for NEMO
# Contact : rblod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fmake_WORK      : create links in the WORK
######################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# =============
# Fmake_WORK.sh
# =============
#
# -----------------------
# Make the WORK directory
# -----------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_WORK.sh
#
#
# DESCRIPTION
# ===========
#
#
# Make the WORK directory:
#
# - Create line in NEW_CONF/WORK
# - Use specified sub-directories previously
# - OCE has to be done first !!!
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_WORK.sh ORCA2_LIM OCE ICE
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
# $Id: Fmake_WORK.sh 15223 2021-09-01 12:12:03Z gsamson $
#
#
#
#   * creation
#
#-
declare ZSRC=${@}
ZCONF=${NEW_CONF}
ZTAB=${NEM_SUBDIR[@]}
declare NDIR=${#ZTAB[@]}

echo 'Creating '${ZCONF}'/WORK = '${ZTAB[*]}' for '${ZCONF}

[ ! -d ${ZCONF}/MY_SRC ] && \mkdir ${ZCONF}/MY_SRC
[   -d ${ZCONF}/WORK   ] || \mkdir ${ZCONF}/WORK

for comp in ${ZTAB[*]}; do
	find ${NEMO_DIR}/$comp -name *.[Ffh]90 -exec ln -sf {} ${ZCONF}/WORK \;
done

cd ${ZCONF}
for ZDIR in ${ZSRC[@]}; do
    if [ -d ${ZDIR} ] ; then
        d=${ZDIR}
    else
        d='MY_SRC'
        echo 'External directory for MY_SRC unspecified or does not exist. Using default.'
    fi

    for ff in `(find ${d} -name *.[Ffh]90 2>/dev/null)`
    do
        if [ "$ff" != "${ff#/}" ]; then
          ln -sf $ff WORK/.
        else
          ln -sf ../$ff WORK/.
        fi
    done
    echo ${d}' content is linked to '${ZCONF}/WORK
done
cd -

unset -v ZCONF ZTAB NDIR
