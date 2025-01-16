#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ============
# Fmake_bld.sh
# ============
#
# --------------------
# Make build directory
# --------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_bld.sh
#
#
# DESCRIPTION
# ===========
#
#
# Under CONFIG_NAME :
# - Make the build directory 
# - Create repositories needed :
# - BLD for compilation 
#
# A tmpdir can be specified for memory issues.
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_bld.sh NEMOGCM/cfgs GYRE  /usr/tmp
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
# $Id: Fmake_bld.sh 9651 2018-05-28 06:47:14Z nicolasmartin $
#
#
#
#   * creation
#
#-
[ ! -d ${3}/${2}     ] && \mkdir ${3}/${2}
[ ! -d ${3}/${2}/BLD ] && \mkdir ${3}/${2}/BLD
[ ! -d ${1}/${2}/BLD ] && ln -sf ${3}/${2}/BLD ${1}/${2}/BLD
# enforce presence of cpp_tools.fcm (write a blank one if not present in the tools directory)
# cp instead of ln to avoid overwiting previous tool cpp_XXX.fcm file when compiling a file without cpp_YYY.fcm file.
[ -f ${1}/${NEW_CONF}/cpp_${NEW_CONF}.fcm ] && cp -f ${1}/${NEW_CONF}/cpp_${NEW_CONF}.fcm ${COMPIL_DIR}/cpp_tools.fcm || echo 'bld::tool::fppkeys ' > ${COMPIL_DIR}/cpp_tools.fcm
rm -f  ${1}/${NEW_CONF}/BLD/fcm.bld.lock
