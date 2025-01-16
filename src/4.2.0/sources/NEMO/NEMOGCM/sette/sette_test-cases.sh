#!/bin/bash
############################################################
# Author : Simona Flavoni for NEMO
# Contact: sflod@locean-ipsl.upmc.fr
#
# sette_test-cases.sh   : principal script of SET TEsts for NEMO (SETTE)
#                       : this script : compiles, run and tests TEST_CASES
#
#                       : TO DO: test if nitend is equal to end of run.stat
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2018)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -vx
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
# NUM_XIOSERVERS    : number of stand-alone IO servers to employ
#                     set to zero if USING_MPMD="no"
#
# Principal script is sette_test-cases.sh, that calls 
#
#  makenemo  : to create successive exectuables in ${CONFIG_NAME}/BLD/bin/nemo.exe 
#              and links to nemo in ${CONFIG_NAME}/EXP00)
#
#  param.cfg : sets and loads following directories:
#              This will have been run by the parent sette.sh and values exported here
#
#   FORCING_DIR         : is the directory for forcing files (tarfile)
#   INPUT_DIR           : is the directory for input files storing 
#   TMPDIR              : is the temporary directory (if needed)
#   NEMO_VALIDATION_DIR : is the validation directory
#
#   (NOTE: this file is the same for all configrations to be tested with sette_test-cases.sh)
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
#                       in sette_test-cases.sh. (each test in executed in its own directory)
#
#  set_valid_dir       : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#
#  clean_valid_dir    : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#                       ( not doing it could lead to false positive )
#
#  prepare_job.sh     : to generate the script run_job.sh
#
#  fcm_job.sh         : run in batch (INTERACT_FLAG="no") or interactive (INTERACT_FLAG="yes")
#                        see sette_test-cases.sh and BATCH_TEMPLATE directory
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
#  NOTE: if sette_test-cases.sh is stopped in output.sette there is written the last command 
#        executed by sette_test-cases.sh
#
# example use: ./sette_test-cases.sh 
#########################################################################################
#
# LOAD param value
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)

export BATCH_COMMAND_PAR=${BATCH_CMD}
export BATCH_COMMAND_SEQ=${BATCH_CMD}
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"
#
# Settings which control the use of stand alone servers (only relevant if using xios)
#
export NUM_XIOSERVERS=4
export JOB_PREFIX=${JOB_PREFIX_MPMD}
#
if [ ${USING_MPMD} == "no" ] 
 then
   export NUM_XIOSERVERS=0
   export JOB_PREFIX=${JOB_PREFIX_NOMPMD}
fi
#
# Directory to run the tests
CONFIG_DIR0=${MAIN_DIR}/cfgs
TOOLS_DIR=${MAIN_DIR}/tools

CMP_NAM=${1:-$COMPILER}
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/${JOB_PREFIX}-${COMPILER} job_batch_template || exit
# Description of configuration tested:
# OVERFLOW       : TEST s-coordinates : (tracers) Advection schemes: FCT2, FCT4, ubs 
#                                     & (dynamics) advection schemes: flux form (ubs, centered), vector form (een)
#                       zps-coordinates : (tracers) Advection schemes: FCT2, FCT4, ubs
#                                     & (dynamics) advection schemes: flux form (ubs, centered), vector form (een, and een + Hollingsworth correction)
# LOCK_EXCHANGE  : 
# VORTEX         : 
# ICE_AGRIF      : 
# ISOMIP+         : 
# WAD

. ./all_functions.sh
for config in ${TEST_CONFIGS[@]}
do

# ---------
#  OVERFLOW
# ---------
if [ ${config} == "OVERFLOW" ] ;  then
    SETTE_CONFIG="OVERFLOW"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=120
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    #
    clean_config OVERFLOW ${SETTE_CONFIG} 'tests'
    #
    sync_config  OVERFLOW ${SETTE_CONFIG} 'tests'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a OVERFLOW -j ${CMPL_CORES} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "OVERFLOW" ] && [ ${DO_RESTART} == "1" ] ;  then
    ## Restartability tests for OVERFLOW
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=1
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}  
    set_namelist namelist_cfg cn_exp \"OVF_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OVF_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"OVF_LONG_${ITRST}_restart\"
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    ln -sf ../LONG/OVF_LONG_${ITRST}_restart.nc .

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}


fi

if [ ${config} == "OVERFLOW" ] && [ ${DO_PHYOPTS} == "1" ] ;  then
    ## Test for all advection, vert. coordinates, vector form, flux form: test runability and complete all time steps
    ## Needed namelist-xxxx for every type of run tested
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=6120
    fi
    cd ${CONFIG_DIR}/${NEW_CONF}/EXP00

    for file in $(echo `ls namelist_*_cfg `) ; do
        TEST_NAME=`echo $file | sed -e "s/namelist_//" | sed -e "s/_cfg//"`
        TEST_NAME="EXP-${TEST_NAME}"
        if [ ! -d ${CONFIG_DIR}/${NEW_CONF}/${TEST_NAME} ] ; then mkdir ${CONFIG_DIR}/${NEW_CONF}/${TEST_NAME} ; fi
        export TEST_NAME="${TEST_NAME}"
         ##
        cd ${SETTE_DIR}
        . ./prepare_exe_dir.sh
        set_valid_dir
        clean_valid_dir
        JOB_FILE=${EXE_DIR}/run_job.sh
        NPROC=1
        if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        cd ${EXE_DIR}
        rm namelist_*_*_*_*
        cp -pL ${CONFIG_DIR}/${NEW_CONF}/EXP00/$file namelist_cfg
	set_namelist namelist_cfg nn_it000 1
	set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        cd ${SETTE_DIR}
        . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        cd ${SETTE_DIR}
        . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
       ##
     done
fi

# --------------
#  LOCK_EXCHANGE
# --------------
if [ ${config} == "LOCK_EXCHANGE" ] ;  then
    SETTE_CONFIG="LOCK_EXCHANGE"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=120
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    #
    clean_config LOCK_EXCHANGE ${SETTE_CONFIG} 'tests'
    #
    sync_config  LOCK_EXCHANGE ${SETTE_CONFIG} 'tests'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a LOCK_EXCHANGE -j ${CMPL_CORES} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "LOCK_EXCHANGE" ] && [ ${DO_RESTART} == "1" ] ;  then
    ## Restartability tests for LOCK_EXCHANGE
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=1
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"LOCK_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"LOCK_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"LOCK_LONG_${ITRST}_restart\"
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    ln -sf ../LONG/LOCK_LONG_${ITRST}_restart.nc .

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "LOCK_EXCHANGE" ] && [ ${DO_PHYOPTS} == "1" ] ;  then
    ## Test for all advection, vector form, flux form: test runability and complete all time steps
    ## Needed namelist-xxxx for every type of run tested
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=61200
    fi
    cd ${CONFIG_DIR}/${NEW_CONF}/EXP00

    for file in $(echo `ls namelist_*_cfg `) ; do
        echo ''
        TEST_NAME=`echo $file | sed -e "s/namelist_//" | sed -e "s/_cfg//"`
        TEST_NAME="EXP-${TEST_NAME}"
        `mkdir ${CONFIG_DIR}/${NEW_CONF}/${TEST_NAME}`
        export TEST_NAME="${TEST_NAME}"
        ##  
        cd ${SETTE_DIR}
        . ./prepare_exe_dir.sh
        set_valid_dir
        clean_valid_dir
        JOB_FILE=${EXE_DIR}/run_job.sh
        NPROC=1
        if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        cd ${EXE_DIR}
        rm namelist_*_*_*_*
        cp -pL ${CONFIG_DIR}/${NEW_CONF}/EXP00/$file namelist_cfg
	set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        cd ${SETTE_DIR}
        . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        cd ${SETTE_DIR}
        . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        ##
        echo ''
   done
fi

# ---------
# VORTEX
# ---------
if [ ${config} == "VORTEX" ] ;  then
    SETTE_CONFIG="VORTEX"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=240
    fi
    ITRST=$(   printf "%08d" $(( ${ITEND} / 2 )) )
    ITRST_1=$( printf "%08d" $(( ${ITEND} * 3 / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    #
    clean_config VORTEX ${SETTE_CONFIG} 'tests'
    #
    sync_config  VORTEX ${SETTE_CONFIG} 'tests'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a VORTEX -j ${CMPL_CORES}  add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "VORTEX" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests for VORTEX
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"VORTEX_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.	
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_namelist 1_namelist_cfg cn_exp \"VORTEX_LONG\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
	
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"VORTEX_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"VORTEX_LONG_${ITRST}_restart\"
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_namelist 1_namelist_cfg cn_exp \"VORTEX_SHORT\"
    set_namelist 1_namelist_cfg nn_it000 $(( ${ITEND} * 3 / 2 + 1 ))
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 1_namelist_cfg ln_rstart .true.
    set_namelist 1_namelist_cfg nn_rstctl 2
    set_namelist 1_namelist_cfg cn_ocerst_in \"VORTEX_LONG_${ITRST_1}_restart\"
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
      
    set_xio_using_server iodef.xml ${USING_MPMD}
    if [ $NPROC -eq 1 ] ;  then
        ln -sf ../LONG/VORTEX_LONG_${ITRST}_restart.nc .
        ln -sf ../LONG/1_VORTEX_LONG_${ITRST_1}_restart.nc .
    else
        for (( i=1; i<=$NPROC; i++)) ; do
            L_NPROC=$(( $i - 1 ))
            L_NPROC=`printf "%04d\n" ${L_NPROC}`
            ln -sf ../LONG/VORTEX_LONG_${ITRST}_restart_${L_NPROC}.nc .
            ln -sf ../LONG/1_VORTEX_LONG_${ITRST_1}_restart_${L_NPROC}.nc .
        done
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} == "VORTEX" ] && [ ${DO_REPRO} == "1" ] ;  then

## Reproducibility tests for VORTEX
    export TEST_NAME="REPRO_2_3"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"VORTEX_23\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock ${ITEND}
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #if [ ${USING_TIMING} == "yes" ]  ; then set_namelist namelist_cfg ln_timing .true. ; fi
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
   
    set_namelist 1_namelist_cfg cn_exp \"VORTEX_23\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg jpni 2
    set_namelist 1_namelist_cfg jpnj 3
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_3_2"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}

    set_namelist namelist_cfg cn_exp \"VORTEX_32\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock ${ITEND}
    set_namelist namelist_cfg jpni 3
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_namelist 1_namelist_cfg cn_exp \"VORTEX_32\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg jpni 3
    set_namelist 1_namelist_cfg jpnj 2
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi


# ---------
# ICE_AGRIF
# ---------
if [ ${config} == "ICE_AGRIF" ] ;  then
    SETTE_CONFIG="ICE_AGRIF"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=10
    else
	ITEND=200
    fi
    ITRST=$(   printf "%08d" $(( ${ITEND} / 2 )) )
    ITRST_1=$( printf "%08d" $(( ${ITEND} * 3 / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    #
    clean_config ICE_AGRIF ${SETTE_CONFIG} 'tests'
    #
    sync_config  ICE_AGRIF ${SETTE_CONFIG} 'tests'
    #
    # ICE_AGRIF uses linssh so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a ICE_AGRIF -j ${CMPL_CORES}  add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "ICE_AGRIF" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests for ICE_AGRIF
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ICE_AGRIF_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_LONG\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ICE_AGRIF_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"ICE_AGRIF_LONG_${ITRST}_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"ICE_AGRIF_LONG_${ITRST}_restart_ice\"
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_SHORT\"
    set_namelist 1_namelist_cfg nn_it000 $(( ${ITEND} * 3 / 2 + 1 ))
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 1_namelist_cfg ln_rstart .true.
    set_namelist 1_namelist_cfg nn_rstctl 2
    set_namelist 1_namelist_cfg cn_ocerst_in \"ICE_AGRIF_LONG_${ITRST_1}_restart\"
    set_namelist 1_namelist_ice_cfg cn_icerst_in \"ICE_AGRIF_LONG_${ITRST_1}_restart_ice\"
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    
    set_xio_using_server iodef.xml ${USING_MPMD}
    if [ $NPROC -eq 1 ] ;  then
        ln -sf ../LONG/ICE_AGRIF_LONG_${ITRST}_restart.nc .
        ln -sf ../LONG/ICE_AGRIF_LONG_${ITRST}_restart_ice.nc .
        ln -sf ../LONG/1_ICE_AGRIF_LONG_${ITRST_1}_restart.nc .
        ln -sf ../LONG/1_ICE_AGRIF_LONG_${ITRST_1}_restart_ice.nc .
    else
        for (( i=1; i<=$NPROC; i++)) ; do
            L_NPROC=$(( $i - 1 ))
            L_NPROC=`printf "%04d\n" ${L_NPROC}`
            ln -sf ../LONG/ICE_AGRIF_LONG_${ITRST}_restart_${L_NPROC}.nc .
            ln -sf ../LONG/ICE_AGRIF_LONG_${ITRST}_restart_ice_${L_NPROC}.nc .
            ln -sf ../LONG/1_ICE_AGRIF_LONG_${ITRST_1}_restart_${L_NPROC}.nc .
            ln -sf ../LONG/1_ICE_AGRIF_LONG_${ITRST_1}_restart_ice_${L_NPROC}.nc .
        done
    fi

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "ICE_AGRIF" ] && [ ${DO_REPRO} == "1" ] ;  then

## Reproducibility tests for ICE_AGRIF
    export TEST_NAME="REPRO_2_3"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ICE_AGRIF_23\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_23\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg jpni 2
    set_namelist 1_namelist_cfg jpnj 3
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_3_2"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}

    set_namelist namelist_cfg cn_exp \"ICE_AGRIF_32\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 3
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_32\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
    set_namelist 1_namelist_cfg jpni 3
    set_namelist 1_namelist_cfg jpnj 2
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

# ------
# ISOMIP+
# ------
if [ ${config} == "ISOMIP+" ] ;  then
    SETTE_CONFIG="ISOMIP+"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=1200
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    #
    clean_config ISOMIP+ ${SETTE_CONFIG} 'tests'
    #
    sync_config  ISOMIP+ ${SETTE_CONFIG} 'tests'
    #
    # ISOMIP+ uses ln_hpg_isf so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a ISOMIP+ -j ${CMPL_CORES} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "ISOMIP+" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=27
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP+_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 9
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP+_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg jpni 9
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg cn_ocerst_in \"ISOMIP+_LONG_${ITRST}_restart\"
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/ISOMIP+_LONG_${ITRST}_restart_${L_NPROC}.nc .
    done

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "ISOMIP+" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=600
    fi
    export TEST_NAME="REPRO_9_3"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=27
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP+_93\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 9
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_8_4"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP+_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi


# ---------
# SWG
# ---------
if [ ${config} == "SWG" ] && [ ${USING_QCO} == "yes" ] ;  then
    SETTE_CONFIG="SWG"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12
    else
	ITEND=1728
    fi
    ITRST=$(   printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    #
    clean_config SWG ${SETTE_CONFIG} 'tests'
    #
    sync_config  SWG ${SETTE_CONFIG} 'tests'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a SWG -j ${CMPL_CORES}  add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "SWG" ] && [ ${DO_RESTART} == "1" ] && [ ${USING_QCO} == "yes" ] ;  then
## Restartability tests for SWG
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=1
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SWG_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.	
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
	
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SWG_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"SWG_LONG_${ITRST}_restart\"
      
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    if [ $NPROC -eq 1 ] ;  then
        ln -sf ../LONG/SWG_LONG_${ITRST}_restart.nc .
    else
        for (( i=1; i<=$NPROC; i++)) ; do
            L_NPROC=$(( $i - 1 ))
            L_NPROC=`printf "%04d\n" ${L_NPROC}`
            ln -sf ../LONG/SWG_LONG_${ITRST}_restart_${L_NPROC}.nc .
        done
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} == "SWG" ] && [ ${DO_REPRO} == "1" ] && [ ${USING_QCO} == "yes" ] ;  then

## Reproducibility tests for SWG
    export TEST_NAME="REPRO_2_3"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SWG_23\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock ${ITEND}
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 3
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_prtctl .true.
   

    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_3_2"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=6
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}

    set_namelist namelist_cfg cn_exp \"SWG_32\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock ${ITEND}
    set_namelist namelist_cfg jpni 3
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_prtctl .true.

    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi



#----
done
#
# Return to SETTE_DIR (last fcm_job.sh will have moved to EXE_DIR)
cd ${SETTE_DIR}
