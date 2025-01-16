#!/bin/bash
#############################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# sette_beginner.sh   : example of script of SET TEsts for NEMO (SETTE)
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# ===================
# sette_beginner.sh
# ===================
#
# COMPILER          : name of compiler as defined in NEMOGCM/ARCH directory 
# BATCH_COMMAND_PAR :  name of the command for submitting parallel batch jobs
# BATCH_COMMAND_SEQ :  name of the command for submitting sequential batch jobs  
# INTERACT_FLAG     : flag to run in interactive mode "yes"
#                           to run in batch mode "no"
# MPIRUN_FLAG       : flag to run in parallel (MPI) "yes"
#                           to run in sequential mode (NB_PROC = 1) "no"
# USING_XIOS        : flag to control the activation of key_xios
#                      "yes" to compile using key_xios and link to the external XIOS library
#                      "no"  to compile without key_xios and link to the old IOIPSL library
# USING_MPMD        : flag to control the use of stand-alone IO servers
#                     requires USING_XIOS="yes"
#                      "yes" to run in MPMD (detached) mode with stand-alone IO servers
#                      "no"  to run in SPMD (attached) mode without separate IO servers 
# NUM_XIOSERVERS    : number of stand-alone IO servers to employ
#                     set to zero if USING_MPMD="no"
#
# Principal script is sette.sh, that calls 
#
#  makenemo  : to create successive exectuables in ${CONFIG_NAME}/BLD/bin/nemo.exe 
#              and links to nemo in ${CONFIG_NAME}/EXP00)
#
#  param.cfg : sets and loads following directories:
#
#   FORCING_DIR         : is the directory for forcing files (tarfile)
#   INPUT_DIR           : is the directory for input files storing 
#   TMPDIR              : is the temporary directory (if needed)
#   NEMO_VALIDATION_DIR : is the validation directory
#
#   (NOTE: this file is the same for all configrations to be tested with sette)
#
#   all_functions.sh : loads functions used by sette (note: new functions can be added here)
#   set_namelist     : function declared in all_functions that sets namelist parameters 
#   post_test_tidyup : creates validation storage directory and copies required output files 
#                      (run.stat and ocean.output) in it after execution of test.
#
#  VALIDATION tree is:
#
#   NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/TEST_NAME/REVISION_NUMBER(or DATE)
#
#  prepare_exe_dir.sh : defines and creates directory where the test is executed
#                       execution directory takes name of TEST_NAME defined for every test 
#                       in sette.sh. (each test in executed in its own directory)
#
#  prepare_job.sh     : to generate the script run_job.sh
#
#  fcm_job.sh         : run in batch (INTERACT_FLAG="no") or interactive (INTERACT_FLAG="yes")
#                        see sette.sh and BATCH_TEMPLATE directory
#
#  NOTE: jobs requiring initial or forcing data need to have an input_CONFIG.cfg in which 
#        can be found paths to the input tar file)
#  NOTE: if job is not launched for any reason you have the executable ready in ${EXE_DIR} 
#        directory
#  NOTE: the changed namelists are left in ${EXE_DIR} directory whereas original namelists 
#        remain in ${NEW_CONF}/EXP00
# 
#  NOTE: a log file, output.sette, is created in ${SETTE_DIR} with the echoes of 
#        executed commands
#
#  NOTE: if sette.sh is stopped in output.sette there is written the last command 
#        executed by sette.sh
#
# example use: ./sette_beginner.sh 
#########################################################################################
#
# Compiler among those in NEMOGCM/ARCH
COMPILER=x3750_ADA
export BATCH_COMMAND_PAR="llsubmit"
export BATCH_COMMAND_SEQ=$BATCH_COMMAND_PAR
export INTERACT_FLAG="yes"
export MPIRUN_FLAG="yes"

export DEL_KEYS="key_xios"
if [ ${USING_XIOS} == "yes" ]
 then 
   export DEL_KEYS=""
fi

#
# Settings which control the use of stand alone servers (only relevant if using xios)
#
export USING_MPMD="no"
export NUM_XIOSERVERS=4
export JOB_PREFIX=batch-mpmd
#
if [ ${USING_MPMD} == "no" ] 
 then
   export NUM_XIOSERVERS=0
   export JOB_PREFIX=batch
fi
#
#
if [ ${USING_MPMD} == "yes" ] && [ ${USING_XIOS} == "no"]
 then
   echo "Incompatible choices. MPMD mode requires the XIOS server"
   exit
fi
#

# Directory to run the tests
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=${SETTE_DIR%/SETTE}
CONFIG_DIR=${MAIN_DIR}/cfgs
TOOLS_DIR=${MAIN_DIR}/tools

CMP_NAM=${1:-$COMPILER}
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/batch-${COMPILER} job_batch_template || exit

# Run for GYRE CONFIG
# small test to start
# compile GYRE configuration with gfortran_osx compiler run with 4 proc : 
export TEST_NAME="SHORT_TEST"
cd ${CONFIG_DIR}
. ./makenemo -m ${CMP_NAM} -n GYRE_SHORT -r GYRE -j 10 add_key "key_nosignedzero" del_key ${DEL_KEYS}
cd ${SETTE_DIR}
. ./param.cfg 
. ./all_functions.sh
. ./prepare_exe_dir.sh
# creation of execution directory
JOB_FILE=${EXE_DIR}/run_job.sh
# setting number of procs used
NPROC=4
if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
cd ${EXE_DIR}
# setting namelist parameters
# experience name
set_namelist namelist_cfg cn_exp \"GYRE_SHORT\"
# first time step
set_namelist namelist_cfg nn_it000 1
# last time step
set_namelist namelist_cfg nn_itend 120
# frequency of creation of a restart file
set_namelist namelist_cfg nn_stock 60
if [ ${USING_MPMD} == "yes" ] ; then
      set_xio_using_server iodef.xml true
   else
      set_xio_using_server iodef.xml false
fi
cd ${SETTE_DIR}
. ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
# run job, with 4 processors, test named SHORT (= 60 time steps)
cd ${SETTE_DIR}   
. ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
