#!/bin/bash
############################################################
# Author : Simona Flavoni for NEMO
# Contact: sflod@locean-ipsl.upmc.fr
# 2013   : A.C. Coward added options for testing with XIOS in dettached mode
#
# sette.sh   : principal script of SET TEsts for NEMO (SETTE)
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
# Principal script is sette.sh, that calls 
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
#  set_valid_dir       : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#
#  clean_valid_dir    : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#                       ( not doing it could lead to false positive )
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
# example use: ./sette.sh 
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
# Description of available configurations:
# GYRE_PISCES       :
# ORCA2_ICE_PISCES  :
# ORCA2_OFF_PISCES  :
# AMM12             :
# SAS               : aka ORCA2_SAS_ICE
# ORCA2_ICE_OBS     :
# AGRIF             : AGRIF_DEMO: test AGRIF in a double zoom configuration in the nordic seas + 1 zoom in the eq. Pacific and
#                     AGRIF_DEMO_NOAGRIF: check that key_agrif without zoom = no key_agrif
# WED025            : regional configuration including sea-ice and tides (Spitzbergen)

. ./all_functions.sh
for config in ${TEST_CONFIGS[@]}
do

# -----------
# GYRE_PISCES
# -----------
if [ ${config} == "GYRE_PISCES" ] ;  then
    SETTE_CONFIG="GYRE_PISCES"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12    # 1 day
    else
	ITEND=1080  # 90 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  GYRE_PISCES ${SETTE_CONFIG} 'cfgs'
    clean_config GYRE_PISCES ${SETTE_CONFIG} 'cfgs'
    #
    # GYRE uses linssh so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r GYRE_PISCES -j ${CMPL_CORES} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "GYRE_PISCES" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests for GYRE_PISCES
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=8
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}  
    set_namelist namelist_cfg cn_exp \"GYREPIS_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg ln_linssh .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYREPIS_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_linssh .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2
    set_namelist namelist_cfg cn_ocerst_in \"GYREPIS_LONG_${ITRST}_restart\"
    set_namelist namelist_top_cfg cn_trcrst_in \"GYREPIS_LONG_${ITRST}_restart_trc\"
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/GYREPIS_LONG_${ITRST}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/GYREPIS_LONG_${ITRST}_restart_trc_${L_NPROC}.nc .
    done
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "GYRE_PISCES" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests for GYRE_PISCES
    export TEST_NAME="REPRO_2_4"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=8
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYREPIS_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg ln_linssh .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_4_2"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=8
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYREPIS_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg ln_linssh .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

# -----------------
# ORCA2_ICE_PISCES
# -----------------
if [ ${config} == "ORCA2_ICE_PISCES" ] ;  then
    SETTE_CONFIG="ORCA2_ICE_PISCES"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16   # 1 day
    else
	ITEND=992  # 62 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  ORCA2_ICE_PISCES ${SETTE_CONFIG} 'cfgs'
    clean_config ORCA2_ICE_PISCES ${SETTE_CONFIG} 'cfgs'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r ORCA2_ICE_PISCES -j ${CMPL_CORES} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "ORCA2_ICE_PISCES" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests for ORCA2_ICE_PISCES
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3P_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_use_calving .true.
    set_namelist namelist_cfg ln_wave .true.
    set_namelist namelist_cfg ln_cdgw .false.
    set_namelist namelist_cfg ln_sdw  .true.
    set_namelist namelist_cfg ln_stcor .true.
    #
    set_namelist_opt namelist_cfg ln_icebergs ${USING_ICEBERGS} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    # for debugging purposes set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, -55.0
    #
    set_namelist namelist_ice_cfg ln_icediachk .true.
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3P_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg nn_test_icebergs -1
    set_namelist namelist_cfg ln_wave .true.
    set_namelist namelist_cfg ln_cdgw .false.
    set_namelist namelist_cfg ln_sdw  .true.
    set_namelist namelist_cfg ln_stcor .true.
    #
    set_namelist_opt namelist_cfg ln_icebergs ${USING_ICEBERGS} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    # for debugging purposes set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, -55.0
    #
    set_namelist namelist_ice_cfg ln_icediachk .true.
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2
    set_namelist namelist_cfg cn_ocerst_in \"O2L3P_LONG_${ITRST}_restart\"
    set_namelist namelist_cfg cn_icbrst_in \"O2L3P_LONG_${ITRST}_restart_icb\"
    set_namelist namelist_top_cfg cn_trcrst_in \"O2L3P_LONG_${ITRST}_restart_trc\"
    set_namelist namelist_ice_cfg cn_icerst_in \"O2L3P_LONG_${ITRST}_restart_ice\"
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/O2L3P_LONG_${ITRST}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/O2L3P_LONG_${ITRST}_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/O2L3P_LONG_${ITRST}_restart_ice_${L_NPROC}.nc .
        if [ ${USING_ICEBERGS} == "yes" ]
            then
             ln -sf ../LONG/O2L3P_LONG_${ITRST}_restart_icb_${L_NPROC}.nc O2L3P_LONG_${ITRST}_restart_icb_${L_NPROC}.nc
        fi
    done
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "ORCA2_ICE_PISCES" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests for ORCA2_ICE_PISCES
    export TEST_NAME="REPRO_4_8"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3P_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_wave .true.
    set_namelist namelist_cfg ln_cdgw .false.
    set_namelist namelist_cfg ln_sdw  .true.
    set_namelist namelist_cfg ln_stcor .true.

    set_namelist_opt namelist_cfg ln_icebergs ${USING_ICEBERGS} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    # for debugging purposes set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, -55.0

    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
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
    set_namelist namelist_cfg cn_exp \"O2L3P_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_wave .true.
    set_namelist namelist_cfg ln_cdgw .false.
    set_namelist namelist_cfg ln_sdw  .true.
    set_namelist namelist_cfg ln_stcor .true.

    set_namelist_opt namelist_cfg ln_icebergs ${USING_ICEBERGS} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    # for debugging purposes set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, -55.0

    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# ----------------
# ORCA2_OFF_PISCES
# ----------------
if [ ${config} == "ORCA2_OFF_PISCES" ] ;  then
    SETTE_CONFIG="ORCA2_OFF_PISCES"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16   # 4 days
    else
	ITEND=380  # 95 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  ${SETTE_CONFIG} ORCA2_OFF_PISCES_ST 'cfgs'
    clean_config ${SETTE_CONFIG} ORCA2_OFF_PISCES_ST 'cfgs'
    #
    # ORCA2_OFF_PISCES uses linssh so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r ORCA2_OFF_PISCES -j ${CMPL_CORES} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "ORCA2_OFF_PISCES" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests for ORCA2_OFF_PISCES
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OFFP_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_qsr_rgb .true.
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OFFP_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_qsr_rgb .true.
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2
    set_namelist namelist_top_cfg cn_trcrst_in \"OFFP_LONG_${ITRST}_restart_trc\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/OFFP_LONG_${ITRST}_restart_trc_${L_NPROC}.nc .
    done
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME}  ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "ORCA2_OFF_PISCES" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests for ORCA2_OFF_PISCES
    export TEST_NAME="REPRO_4_8"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OFFP_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_qsr_rgb .true.
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
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
    set_namelist namelist_cfg cn_exp \"OFFP_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_qsr_rgb .true.
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_hydrofe to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false. 
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# -----
# AMM12
# -----
if [ ${config} == "AMM12" ] ;  then
    SETTE_CONFIG="AMM12"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12   # 3 h
    else
	ITEND=576  # 4 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  AMM12 ${SETTE_CONFIG} 'cfgs'
    clean_config AMM12 ${SETTE_CONFIG} 'cfgs'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r AMM12 -j ${CMPL_CORES} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "AMM12" ] && [ ${DO_RESTART} == "1" ] ;  then
    ## Restartability tests for AMM12
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AMM12_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AMM12_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"AMM12_LONG_${ITRST}_restart\"
    set_namelist namelist_cfg nn_date0 20120102
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/AMM12_LONG_${ITRST}_restart_${L_NPROC}.nc .
    done
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "AMM12" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests for AMM12
    export TEST_NAME="REPRO_8_4"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AMM12_84\"
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
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_4_8"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AMM12_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi


# ---------
# ORCA2_SAS_ICE
# ---------
if [ ${config} == "SAS" ] ;  then
    SETTE_CONFIG="ORCA2_SAS_ICE"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16   # 1 day
    else
	ITEND=256  # 16 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  ORCA2_SAS_ICE ${SETTE_CONFIG} 'cfgs'
    clean_config ORCA2_SAS_ICE ${SETTE_CONFIG} 'cfgs'
    #
    # ORCA2_SAS_ICE uses linssh so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r ORCA2_SAS_ICE -j ${CMPL_CORES} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "SAS" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_ice_cfg ln_icediachk .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg nn_date0 010109
    set_namelist namelist_cfg cn_ocerst_in \"SAS_${ITRST}_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"SAS_${ITRST}_restart_ice\"
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/SAS_${ITRST}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/SAS_${ITRST}_restart_ice_${L_NPROC}.nc .
    done
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "SAS" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16  # 1 day
    else
	ITEND=80  # 5 days
    fi
    export TEST_NAME="REPRO_4_8"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
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
    set_namelist namelist_cfg cn_exp \"SAS_84\"
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
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi


# --------------
# ORCA2_ICE_OBS
# --------------
## Test assimilation interface code, OBS and ASM for reproducibility
## Restartability not tested (ASM code not restartable while increments are being applied)
if [ ${config} == "ORCA2_ICE_OBS" ] ;  then
    SETTE_CONFIG="ORCA2_ICE_OBS"${SETTE_STG}
## Reproducibility tests
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16  # 1 day
    else
	ITEND=80  # 5 days
    fi
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  ORCA2_ICE_PISCES ${SETTE_CONFIG} 'cfgs'
    clean_config ORCA2_ICE_PISCES ${SETTE_CONFIG} 'cfgs'
    #
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r ORCA2_ICE_PISCES -d "OCE ICE"  -j ${CMPL_CORES} add_key "key_asminc ${ADD_KEYS}" del_key "key_top ${DEL_KEYS}"
fi
if [ ${config} == "ORCA2_ICE_OBS" ] && [ ${DO_RESTART} == "1" ] ;  then
## Reproducibility tests
    export TEST_NAME="REPRO_4_8"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3OBS_48\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg ln_read_cfg .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_diaobs .true.
    set_namelist namelist_cfg ln_t3d .true.
    set_namelist namelist_cfg ln_s3d .true.
    set_namelist namelist_cfg ln_sst .true.
    set_namelist namelist_cfg ln_sla .true.
    set_namelist namelist_cfg ln_sic .true.
    set_namelist namelist_cfg ln_vel3d .true.
    set_namelist namelist_cfg ln_bkgwri .true.
    set_namelist namelist_cfg ln_trainc .true.
    set_namelist namelist_cfg ln_dyninc .true.
    set_namelist namelist_cfg ln_sshinc .true.
    set_namelist namelist_cfg ln_asmiau .true.
    #remove all useless options for pisces (due to ORCA2_ICE_PISCES reference configuration)
    set_namelist namelist_top_cfg ln_trcdta .false. 
    set_namelist namelist_top_cfg ln_trcbc  .false. 
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_OBS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
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
    set_namelist namelist_cfg cn_exp \"O2L3OBS_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg ln_read_cfg .true.
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_diaobs .true.
    set_namelist namelist_cfg ln_t3d .true.
    set_namelist namelist_cfg ln_s3d .true.
    set_namelist namelist_cfg ln_sst .true.
    set_namelist namelist_cfg ln_sla .true.
    set_namelist namelist_cfg ln_sic .true.
    set_namelist namelist_cfg ln_vel3d .true.
    set_namelist namelist_cfg ln_bkgwri .true.
    set_namelist namelist_cfg ln_trainc .true.
    set_namelist namelist_cfg ln_dyninc .true.
    set_namelist namelist_cfg ln_sshinc .true.
    set_namelist namelist_cfg ln_asmiau .true.
    #remove all useless options for pisces (due to ORCA2_ICE_PISCES reference configuration)
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_trcbc  .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_ironice .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_ICE_OBS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# ------------
# AGRIF ICE
# -----------
if [ ${config} == "AGRIF" ] ;  then
    SETTE_CONFIG="AGRIF_DEMO"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=4   # 6h
    else
	ITEND=20  # 1d and 6h
    fi
    ITRST=$(   printf "%08d" $(( ${ITEND} / 2 )) )
    ITRST_1=$( printf "%08d" $(( ${ITEND} / 2 )) )
    ITRST_2=$( printf "%08d" $(( ${ITEND} * 4 / 2 )) )
    ITRST_3=$( printf "%08d" $(( ${ITEND} * 4 * 3 / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  AGRIF_DEMO ${SETTE_CONFIG} 'cfgs'
    clean_config AGRIF_DEMO ${SETTE_CONFIG} 'cfgs'
    #
    # AGRIF_DEMO does not yet support nn_hls=2 => key_loop_fusion can not be used
#    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r AGRIF_DEMO -j ${CMPL_CORES} add_key "${ADD_KEYS/key_loop_fusion}" del_key "${DEL_KEYS}"
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r AGRIF_DEMO -j ${CMPL_CORES} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "AGRIF" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AGRIF_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist 1_namelist_cfg cn_exp \"AGRIF_LONG\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend ${ITEND}
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 2_namelist_cfg cn_exp \"AGRIF_LONG\"
    set_namelist 2_namelist_cfg nn_it000 1
    set_namelist 2_namelist_cfg nn_itend $(( ${ITEND} * 4 ))
    set_namelist 2_namelist_cfg nn_stock $(( ${ITEND} * 4 / 2 ))
    set_namelist 2_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 3_namelist_cfg cn_exp \"AGRIF_LONG\"
    set_namelist 3_namelist_cfg nn_it000 1
    set_namelist 3_namelist_cfg nn_itend $(( ${ITEND} * 4 * 3 ))
    set_namelist 3_namelist_cfg nn_stock $(( ${ITEND} * 4 * 3 / 2 ))
    set_namelist 3_namelist_cfg sn_cfctl%l_runstat .true.

    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 2_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 2_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 2_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 3_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 3_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 3_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AGRIF_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2 
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist 1_namelist_cfg cn_exp \"AGRIF_SHORT\"
    set_namelist 1_namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist 1_namelist_cfg nn_itend ${ITEND}
    set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 1_namelist_cfg ln_rstart .true.
    set_namelist 1_namelist_cfg nn_rstctl 2
    set_namelist 1_namelist_top_cfg ln_rsttr .true.
    set_namelist 1_namelist_cfg ln_init_chfrpar .false.
    set_namelist 1_namelist_top_cfg nn_rsttr 2 
    set_namelist 2_namelist_cfg cn_exp \"AGRIF_SHORT\"
    set_namelist 2_namelist_cfg nn_it000 $(( ${ITEND} * 4 / 2 + 1 ))
    set_namelist 2_namelist_cfg nn_itend $(( ${ITEND} * 4 ))
    set_namelist 2_namelist_cfg nn_stock $(( ${ITEND} * 4 / 2 ))
    set_namelist 2_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 2_namelist_cfg ln_rstart .true.
    set_namelist 2_namelist_cfg nn_rstctl 2
    set_namelist 2_namelist_cfg ln_init_chfrpar .false.
    set_namelist 2_namelist_top_cfg ln_rsttr .true.
    set_namelist 2_namelist_top_cfg nn_rsttr 2 
    set_namelist 3_namelist_cfg cn_exp \"AGRIF_SHORT\"
    set_namelist 3_namelist_cfg nn_it000 $(( ${ITEND} * 4 * 3 / 2 + 1 ))
    set_namelist 3_namelist_cfg nn_itend $(( ${ITEND} * 4 * 3 ))
    set_namelist 3_namelist_cfg nn_stock $(( ${ITEND} * 4 * 3 / 2 ))
    set_namelist 3_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist 3_namelist_cfg ln_rstart .true.
    set_namelist 3_namelist_cfg nn_rstctl 2
    set_namelist 3_namelist_cfg ln_init_chfrpar .false.
    set_namelist 3_namelist_top_cfg ln_rsttr .true.
    set_namelist 3_namelist_top_cfg nn_rsttr 2 
    set_namelist namelist_cfg cn_ocerst_in \"AGRIF_LONG_${ITRST}_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"AGRIF_LONG_${ITRST}_restart_ice\"
    set_namelist namelist_top_cfg cn_trcrst_in \"AGRIF_LONG_${ITRST}_restart_trc\"
    set_namelist 1_namelist_cfg cn_ocerst_in \"AGRIF_LONG_${ITRST_1}_restart\"
    set_namelist 1_namelist_ice_cfg cn_icerst_in \"AGRIF_LONG_${ITRST_1}_restart_ice\"
    set_namelist 1_namelist_top_cfg cn_trcrst_in \"AGRIF_LONG_${ITRST_1}_restart_trc\"
    set_namelist 2_namelist_cfg cn_ocerst_in \"AGRIF_LONG_${ITRST_2}_restart\"
    set_namelist 2_namelist_ice_cfg cn_icerst_in \"AGRIF_LONG_${ITRST_2}_restart_ice\"
    set_namelist 2_namelist_top_cfg cn_trcrst_in \"AGRIF_LONG_${ITRST_2}_restart_trc\"
    set_namelist 3_namelist_cfg cn_ocerst_in \"AGRIF_LONG_${ITRST_3}_restart\"
    set_namelist 3_namelist_ice_cfg cn_icerst_in \"AGRIF_LONG_${ITRST_3}_restart_ice\"
    set_namelist 3_namelist_top_cfg cn_trcrst_in \"AGRIF_LONG_${ITRST_3}_restart_trc\"
#
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 2_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 2_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 2_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist_opt 3_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 3_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 3_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/AGRIF_LONG_${ITRST}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/AGRIF_LONG_${ITRST}_restart_ice_${L_NPROC}.nc .
        ln -sf ../LONG/AGRIF_LONG_${ITRST}_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/1_AGRIF_LONG_${ITRST_1}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/1_AGRIF_LONG_${ITRST_1}_restart_ice_${L_NPROC}.nc .
        ln -sf ../LONG/1_AGRIF_LONG_${ITRST_1}_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/2_AGRIF_LONG_${ITRST_2}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/2_AGRIF_LONG_${ITRST_2}_restart_ice_${L_NPROC}.nc .
        ln -sf ../LONG/2_AGRIF_LONG_${ITRST_2}_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/3_AGRIF_LONG_${ITRST_3}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/3_AGRIF_LONG_${ITRST_3}_restart_ice_${L_NPROC}.nc .
        ln -sf ../LONG/3_AGRIF_LONG_${ITRST_3}_restart_trc_${L_NPROC}.nc .
    done
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "AGRIF" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests
    export TEST_NAME="REPRO_2_8"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AGRIF_28\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist 1_namelist_cfg cn_exp \"AGRIF_28\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend ${ITEND}
    set_namelist 1_namelist_cfg jpni 2
    set_namelist 1_namelist_cfg jpnj 8
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist 2_namelist_cfg cn_exp \"AGRIF_28\"
    set_namelist 2_namelist_cfg nn_it000 1
    set_namelist 2_namelist_cfg nn_itend $(( ${ITEND} * 4 ))
    set_namelist 2_namelist_cfg jpni 2
    set_namelist 2_namelist_cfg jpnj 8
    set_namelist 2_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 2_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 2_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 2_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist 3_namelist_cfg cn_exp \"AGRIF_28\"
    set_namelist 3_namelist_cfg nn_it000 1
    set_namelist 3_namelist_cfg nn_itend $(( ${ITEND} * 4 * 3 ))
    set_namelist 3_namelist_cfg jpni 2
    set_namelist 3_namelist_cfg jpnj 8
    set_namelist 3_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 3_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 3_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 3_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_4_4"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"AGRIF_44\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    set_namelist 1_namelist_cfg cn_exp \"AGRIF_44\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend ${ITEND}
    set_namelist 1_namelist_cfg jpni 4
    set_namelist 1_namelist_cfg jpnj 4
    set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist 2_namelist_cfg cn_exp \"AGRIF_44\"
    set_namelist 2_namelist_cfg nn_it000 1
    set_namelist 2_namelist_cfg nn_itend $(( ${ITEND} * 4 ))
    set_namelist 2_namelist_cfg jpni 4
    set_namelist 2_namelist_cfg jpnj 4
    set_namelist 2_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 2_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 2_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 2_namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_namelist 3_namelist_cfg cn_exp \"AGRIF_44\"
    set_namelist 3_namelist_cfg nn_it000 1
    set_namelist 3_namelist_cfg nn_itend $(( ${ITEND} * 4 * 3 ))
    set_namelist 3_namelist_cfg jpni 4
    set_namelist 3_namelist_cfg jpnj 4
    set_namelist 3_namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt 3_namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt 3_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt 3_namelist_cfg ln_tile ${USING_TILING} .true. .false.

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "AGRIF" ] && [ ${DO_CORRUPT} == "1" ] ;  then
## test code corruption with AGRIF (phase 1) ==> Compile with key_agrif but run with no zoom
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=16   # 1d
    else
	ITEND=150  # 5d and 9h 
    fi
    export TEST_NAME="ORCA2"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ORCA2\"
    # Use "original" parent grid bathymetry
    set_namelist namelist_cfg cn_domcfg "'ORCA_R2_zps_domcfg.nc'"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.

#   Set the number of fine grids to zero:    
    sed -i "1s/.*/0/" ${EXE_DIR}/AGRIF_FixedGrids.in

    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi


## test code corruption with AGRIF (phase 2) ==> Compile without key_agrif (to be compared with AGRIF_DEMO_ST/ORCA2)
if [ ${config} == "AGRIF" ] && [ ${DO_CORRUPT} == "1" ] ;  then
    SETTE_CONFIG="AGRIF_DEMO_NOAGRIF"${SETTE_STG}
    export TEST_NAME="ORCA2"
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  AGRIF_DEMO ${SETTE_CONFIG} 'cfgs'
    clean_config AGRIF_DEMO ${SETTE_CONFIG} 'cfgs'
    #
    # AGRIF_DEMO does not yet support nn_hls=2 => key_loop_fusion can not be used
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r AGRIF_DEMO -j ${CMPL_CORES} add_key "${ADD_KEYS/key_loop_fusion}" del_key "key_agrif ${DEL_KEYS}"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ORCA2\"
    # Use "original" parent grid bathymetry
    set_namelist namelist_cfg cn_domcfg "'ORCA_R2_zps_domcfg.nc'"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
    #set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
#
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

# -------
# WED025
# -------
if [ ${config} == "WED025" ] ;  then
    SETTE_CONFIG="WED025"${SETTE_STG}
    if [ $( echo ${CMP_NAM} | grep -ic debug ) -eq 1 ]
    then
	ITEND=12   # 4h
    else
	ITEND=720  # 10 days
    fi
    ITRST=$( printf "%08d" $(( ${ITEND} / 2 )) )
    cd ${MAIN_DIR}
    #
    # syncronisation if target directory/file exist (not done by makenemo)
    sync_config  WED025 ${SETTE_CONFIG} 'cfgs'
    clean_config WED025 ${SETTE_CONFIG} 'cfgs'
    #
    # WED025 uses ln_hpg_isf so remove key_qco if added by default
    . ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -r WED025 -j ${CMPL_CORES} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}"
fi
if [ ${config} == "WED025" ] && [ ${DO_RESTART} == "1" ] ;  then
## Restartability tests
    export TEST_NAME="LONG"
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"WED025_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg nn_date0 20000115
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    #set_namelist namelist_ice_cfg ln_icediachk .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"WED025_SHORT\"
    set_namelist namelist_cfg nn_it000 $(( ${ITEND} / 2 + 1 ))
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist namelist_cfg cn_ocerst_in \"WED025_LONG_${ITRST}_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"WED025_LONG_${ITRST}_restart_ice\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/WED025_LONG_${ITRST}_restart_${L_NPROC}.nc .
        ln -sf ../LONG/WED025_LONG_${ITRST}_restart_ice_${L_NPROC}.nc .
    done
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

if [ ${config} == "WED025" ] && [ ${DO_REPRO} == "1" ] ;  then
## Reproducibility tests
    export TEST_NAME="REPRO_5_6"
    cd ${MAIN_DIR}
    cd ${SETTE_DIR}
    . ./prepare_exe_dir.sh
    set_valid_dir
    clean_valid_dir
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"WED025_56\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_date0 20000115
    set_namelist namelist_cfg jpni 6
    set_namelist namelist_cfg jpnj 7
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
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
    set_namelist namelist_cfg cn_exp \"WED025_84\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend ${ITEND}
    set_namelist namelist_cfg nn_date0 20000115
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg sn_cfctl%l_runstat .true.
    set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
    set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 2 1
    set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
    set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
    set_xio_using_server iodef.xml ${USING_MPMD}
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi


done
#
# Return to SETTE_DIR (last fcm_job.sh will have moved to EXE_DIR)
cd ${SETTE_DIR}
