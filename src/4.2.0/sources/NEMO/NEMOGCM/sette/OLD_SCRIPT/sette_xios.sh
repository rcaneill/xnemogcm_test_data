#!/bin/bash
############################################################
# Author : Italo Epicoco - CMCC
# Contact: italo.epicoco@unisalento.it
# 2014   : A.C. Coward added new namelist settings for GYRE configuration
#
# sette_xios.sh   : additional script of SET TEsts for XIOS within NEMO
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2014)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -x
set -o posix
#set -u
#set -e
# ===========
# DESCRIPTION
# ===========
#
# Variables to be checked by user:
#
# COMPILER          : name of compiler as defined in NEMOGCM/ARCH directory 
# BATCH_COMMAND_PAR :  name of the command for submitting parallel batch jobs
# BATCH_COMMAND_SEQ :  name of the command for submitting sequential batch jobs  
# INTERACT_FLAG     : flag to run in interactive mode "yes"
#                           to run in batch mode "no"
# MPIRUN_FLAG       : flag to run in parallel (MPI) "yes"
#                           to run in sequential mode (NB_PROC = 1) "no"
# USING_MPMD        : flag to control the use of stand-alone IO servers
#                      "true" to run in MPMD (detached) mode with stand-alone IO servers
#                      "false"  to run in SPMD (attached) mode without separate IO servers 
# NUM_XIOSERVERS    : number of stand-alone IO servers to employ
#                     set to zero if USING_MPMD="false"
#
# Principal script is sette_xios.sh, that calls 
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
#  NOTE: if sette_xios.sh is stopped in output.sette there is written the last command 
#        executed by sette_xios.sh
#
# example use: ./sette_xios.sh 
#########################################################################################
#
# Compiler among those in NEMOGCM/ARCH
COMPILER=
export BATCH_COMMAND_PAR="llsubmit"
export BATCH_COMMAND_SEQ=$BATCH_COMMAND_PAR
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"
#

# Directory to run the tests
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=${SETTE_DIR%/SETTE}
CONFIG_DIR=${MAIN_DIR}/cfgs
TOOLS_DIR=${MAIN_DIR}/tools

CMP_NAM=${1:-$COMPILER}
#
#=================================================================================
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
# Note this batch template needs to be capable of launching both SPMD and MPMD
# tasks with internal selection depending on the value of NUM_XIOSERVERS
# (0=SPMD; >0 = MPMD)
#=================================================================================
#
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/batch-${COMPILER} job_batch_template || exit

for config in 1 2  

do

#==========================================================
# TESTS FOR XIOS USING GYRE CONFIGURATION AT LOW RESOLUTION
#==========================================================

if [ ${config} -eq 1 ] ;  then
    ## Test of XIOS configured in attached mode with multiple output files (one for each process). 
    ## $NPROC processes are used 

    NPROC=4
    jp_cfg=1
    jpni=2
    jpnj=2
    export NUM_XIOSERVERS=0
    export USING_MPMD="false"

    export TEST_NAME="ATTACHED_MULTIPLE"

    cd ${SETTE_DIR}
    . ../cfgs/makenemo -m ${CMP_NAM} -n GYRE_XIOS_LR -r GYRE_XIOS -j 8 
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}  
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_LR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml multiple_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}


    ## Test of XIOS configured in attached mode with a single output file. 

    export TEST_NAME="ATTACHED_ONE"
    export NUM_XIOSERVERS=0
    export USING_MPMD="false"
    cd ${SETTE_DIR} 
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_LR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml one_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}


    ## Test of XIOS configured in detached mode with a single output file. 
    ## $NUM_XIOSERVERS IO server are used.
    ## the total number of allocated cores is $NUM_XIOSERVERS + $NPROC 

    export TEST_NAME="DETACHED_ONE"
    export NUM_XIOSERVERS=2
    export USING_MPMD="true"
    cd ${SETTE_DIR}
#
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_LR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml one_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    ## Test of XIOS configured in detached mode with multiple output files (one for each IO server). 
    ## $NUM_XIOSERVERS IO server are used.
    ## the total number of allocated cores is $NUM_XIOSERVERS + $NPROC 


    export TEST_NAME="DETACHED_MULTIPLE"
    export NUM_XIOSERVERS=2
    export USING_MPMD="true"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_LR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml multiple_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

#==========================================================
# TESTS FOR XIOS USING GYRE CONFIGURATION AT HIGH RESOLUTION
#==========================================================

if [ ${config} -eq 2 ] ;  then
    ## Test of XIOS configured in attached mode with multiple output files (one for each process). 
    ## $NPROC processes are used 

    NPROC=64
    jp_cfg=30
    jpni=8
    jpnj=8
    export NUM_XIOSERVERS=0
    export USING_MPMD="false"

    export TEST_NAME="ATTACHED_MULTIPLE"

    cd ${SETTE_DIR}
    . ../cfgs/makenemo -m ${CMP_NAM} -n GYRE_XIOS_HR -r GYRE_XIOS -j 8
    cd ${SETTE_DIR}
#
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
#
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_HR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg nn_bench 1
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml multiple_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}


    ## Test of XIOS configured in attached mode with a single output file. 

    export TEST_NAME="ATTACHED_ONE"
    export NUM_XIOSERVERS=0
    export USING_MPMD="false"
    cd ${SETTE_DIR}
#
    . ./prepare_exe_dir.sh
#
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_HR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg nn_bench 1
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml one_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}


    ## Test of XIOS configured in detached mode with a single output file. 
    ## $NUM_XIOSERVERS IO server are used.
    ## the total number of allocated cores is $NUM_XIOSERVERS + $NPROC 

    export TEST_NAME="DETACHED_ONE"
    export NUM_XIOSERVERS=8
    export USING_MPMD="true"
    cd ${SETTE_DIR}
#
    . ./prepare_exe_dir.sh
#
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_HR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg nn_bench 1
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml one_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    ## Test of XIOS configured in detached mode with multiple output files (one for each IO server). 
    ## $NUM_XIOSERVERS IO server are used.
    ## the total number of allocated cores is $NUM_XIOSERVERS + $NPROC 

    export TEST_NAME="DETACHED_MULTIPLE"
    export NUM_XIOSERVERS=8
    export USING_MPMD="true"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_XIOS_HR\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg nn_bench 1
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jp_cfg $jp_cfg
    set_namelist namelist_cfg jpidta $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjdta $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpiglo $(( $jp_cfg * 30 + 2 ))
    set_namelist namelist_cfg jpjglo $(( $jp_cfg * 20 + 2 ))
    set_namelist namelist_cfg jpni $jpni
    set_namelist namelist_cfg jpnj $jpnj
    set_namelist namelist_cfg jpnij $NPROC

    set_xio_using_server iodef.xml $USING_MPMD
    set_xio_file_type    iodef.xml multiple_file

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

done
