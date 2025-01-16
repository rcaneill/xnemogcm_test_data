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
# USING_XIOS        : flag to control the activation of key_iomput
#                      "yes" to compile using key_iomput and link to the external XIOS library
#                      "no"  to compile without key_iomput and link to the old IOIPSL library
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
#              and links to opa in ${CONFIG_NAME}/EXP00)
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
#                      (solver.stat and ocean.output) in it after execution of test.
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
# example use: ./sette.sh 
#########################################################################################
#
# Compiler among those in NEMOGCM/ARCH
COMPILER=openmpi_NAVITI_MERCATOR
export BATCH_COMMAND_PAR="qsub"
export BATCH_COMMAND_SEQ=$BATCH_COMMAND_PAR
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"
export USING_XIOS="yes"
#
export DEL_KEYS="key_iomput"
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
MAIN_DIR=$(dirname $SETTE_DIR)
CONFIG_DIR=${MAIN_DIR}/CONFIG
TOOLS_DIR=${MAIN_DIR}/TOOLS
COMPIL_DIR=${TOOLS_DIR}/COMPILE

CMP_NAM=${1:-$COMPILER}
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/${JOB_PREFIX}-${COMPILER} job_batch_template || exit
# Description of configuration tested:
# GYRE            : 1 &  2
# ORCA2_LIM_PISCES: 3 &  4
# ORCA2_OFF_PISCES: 5 &  6
# ORCA2_LIM3      : 7 &  8
# AMM12           : 9 & 10
# SAS             :11 & 12
# ISOMIP          :13 & 14
# ORCA2_LIM_OBS   :15
# ORCA2_AGRIF_LIM :16 & 17 
#                  18 & 19 
for config in  11

do

# TESTS FOR GYRE CONFIGURATION
if [ ${config} -eq 1 ] ;  then
    ## Restartability tests for GYRE
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n GYRE_LONG -r GYRE -j 8 del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}  
    set_namelist namelist_cfg cn_exp \"GYRE_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 60
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_SHORT\"
    set_namelist namelist_cfg nn_it000 61
    set_namelist namelist_cfg nn_itend 120
    set_namelist namelist_cfg nn_stock 60
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg cn_ocerst_in \"GYRE_LONG_00000060_restart\"
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/GYRE_LONG_00000060_restart_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 2 ] ;  then
    ## Reproducibility tests for GYRE
    export TEST_NAME="REPRO_1_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n GYRE_4 -r GYRE -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_14\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 60
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_bench 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 1
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_2"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"GYRE_22\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 60
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

# TESTS FOR ORCA2_LIM_PISCES CONFIGURATION
if [ ${config} -eq 3 ] ;  then
    ## Restartability tests for ORCA2_LIM_PISCES
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2LIMPIS_LONG -r ORCA2_LIM_PISCES -j 8 del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2LP_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2LP_SHORT\"
    set_namelist namelist_cfg nn_it000 76
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_top_cfg ln_diatrc .false.
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2
    set_namelist namelist_cfg cn_ocerst_in \"O2LP_LONG_00000075_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"O2LP_LONG_00000075_restart_ice\"
    set_namelist namelist_top_cfg cn_trcrst_in \"O2LP_LONG_00000075_restart_trc\"
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/O2LP_LONG_00000075_restart_${L_NPROC}.nc .
        ln -sf ../LONG/O2LP_LONG_00000075_restart_trc_${L_NPROC}.nc .
        ln -sf ../LONG/O2LP_LONG_00000075_restart_ice_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 4 ] ;  then
    ## Reproducibility tests for ORCA2_LIM_PISCES
    export TEST_NAME="REPRO_4_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2LIMPIS_16 -r ORCA2_LIM_PISCES -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# TESTS FOR ORCA2_OFF_PISCES CONFIGURATION
if [ ${config} -eq 5 ] ;  then
    ## Restartability tests for ORCA2_OFF_PISCES
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2OFFPIS_LONG -r ORCA2_OFF_PISCES -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OFFP_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 40
    set_namelist namelist_cfg nn_stock 20
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"OFFP_SHORT\"
    set_namelist namelist_cfg nn_it000 21
    set_namelist namelist_cfg nn_itend 40
    set_namelist namelist_cfg nn_stock 20
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_top_cfg ln_diatrc .false.
    set_namelist namelist_top_cfg ln_rsttr .true.
    set_namelist namelist_top_cfg nn_rsttr 2
    set_namelist namelist_top_cfg cn_trcrst_in \"OFFP_LONG_00000020_restart_trc\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/OFFP_LONG_00000020_restart_trc_${L_NPROC}.nc .
    done
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME}  ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 6 ] ;  then
    ## Reproducibility tests for ORCA2_OFF_PISCES
    export TEST_NAME="REPRO_4_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2OFFPIS_16 -r ORCA2_OFF_PISCES -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 40
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 40
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_top_cfg ln_trcdta .false.
    set_namelist namelist_top_cfg ln_diatrc .false.
    # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
    # if not you need input files, and for tests is not necessary
    set_namelist namelist_pisces_cfg ln_presatm .false.
    set_namelist namelist_pisces_cfg ln_varpar .false.
    set_namelist namelist_pisces_cfg ln_dust .false.
    set_namelist namelist_pisces_cfg ln_solub .false.
    set_namelist namelist_pisces_cfg ln_river .false.
    set_namelist namelist_pisces_cfg ln_ndepo .false.
    set_namelist namelist_pisces_cfg ln_ironsed .false.
    set_namelist namelist_pisces_cfg ln_hydrofe .false.
    # put ln_pisdmp to false : no restoring to global mean value
    set_namelist namelist_pisces_cfg ln_pisdmp .false. 
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi


# TESTS FOR ORCA2_LIM3 CONFIGURATION
if [ ${config} -eq 7 ] ;  then
    ## Restartability tests for ORCA2_LIM3
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2LIM3_LONG -r ORCA2_LIM3 -j 8 del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM3.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2L3_SHORT\"
    set_namelist namelist_cfg nn_it000 76
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg cn_ocerst_in \"O2L3_LONG_00000075_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"O2L3_LONG_00000075_restart_ice\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/O2L3_LONG_00000075_restart_${L_NPROC}.nc .
        ln -sf ../LONG/O2L3_LONG_00000075_restart_ice_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM3.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 8 ] ;  then
    ## Reproducibility tests for ORCA2_LIM3
    export TEST_NAME="REPRO_4_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2LIM3_16 -r ORCA2_LIM3 -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM3.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi

    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM3.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi


# TESTS FOR AMM12 CONFIGURATION
if [ ${config} -eq 9 ] ;  then
    ## Restartability tests for AMM12
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n AMM12_LONG -r AMM12 -j 8 add_key "key_tide" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 576
    set_namelist namelist_cfg nn_stock 288
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 289
    set_namelist namelist_cfg nn_itend 576
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg cn_ocerst_in \"AMM12_00000288_restart_oce_out\"
    set_namelist namelist_cfg nn_date0 20120102
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/AMM12_00000288_restart_oce_out_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 10 ] ;  then
## Reproducibility tests for AMM12
    export TEST_NAME="REPRO_8_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n AMM12_32 -r AMM12 -j 8 add_key "key_mpp_rep key_tide" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 576
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_4_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 576
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi


# TESTS FOR ORCA2_SAS_LIM CONFIGURATION
if [ ${config} -eq 11 ] ;  then
    ## Restartability tests for SAS
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n SAS_LONG -r ORCA2_SAS_LIM -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    \rm $JOB_FILE
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 240
    set_namelist namelist_cfg nn_stock 120
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 121
    set_namelist namelist_cfg nn_itend 240
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg nn_date0 010109
    set_namelist namelist_cfg cn_ocerst_in \"SAS_00000120_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"SAS_00000120_restart_ice\"
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/SAS_00000120_restart_${L_NPROC}.nc .
        ln -sf ../LONG/SAS_00000120_restart_ice_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

if [ ${config} -eq 12 ] ;  then
## Reproducibility tests for ORCA2_SAS_LIM
    export TEST_NAME="REPRO_8_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n SAS_32 -r ORCA2_SAS_LIM -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=32
    \rm ${JOB_FILE}
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 51
    set_namelist namelist_cfg nn_itend 100
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 8
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE}  ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_4_8"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"SAS\"
    set_namelist namelist_cfg nn_it000 51
    set_namelist namelist_cfg nn_itend 100
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 32
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_SAS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi
# TESTS FOR ISOMIP CONFIGURATION
if [ ${config} -eq 13 ] ;  then
    ## Restartability tests for ISOMIP
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ISOMIP_LONG -u ISOMIP -j 8 del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 96
    set_namelist namelist_cfg nn_stock 48
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}

    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP_SHORT\"
    set_namelist namelist_cfg nn_it000 49
    set_namelist namelist_cfg nn_itend 96
    set_namelist namelist_cfg nn_stock 48
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg cn_ocerst_in \"ISOMIP_LONG_00000048_restart\"
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/ISOMIP_LONG_00000048_restart_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi
if [ ${config} -eq 14 ] ;  then
    ## Reproducibility tests for ISOMIP
    export TEST_NAME="REPRO_1_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ISOMIP_4 -u ISOMIP -j 8 add_key "key_mpp_rep" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP_14\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 48
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg nn_bench 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 1
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_2"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"ISOMIP_22\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 48
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ISOMIP.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

## Test assimilation interface code, OBS and ASM for reproducibility
## Restartability not tested (ASM code not restartable while increments are being applied)
if [ ${config} -eq 15 ] ; then
   ## Reproducibility tests for ORCA2_LIM_OBS
    export TEST_NAME="REPRO_4_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2_LIM_OBS -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_diaobs key_asminc" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2   
    set_namelist namelist_cfg ln_t3d .true.
    set_namelist namelist_cfg ln_s3d .true.
    set_namelist namelist_cfg ln_profb .true.
    set_namelist namelist_cfg ln_sst .true.
    set_namelist namelist_cfg ln_sstfb .true.
    set_namelist namelist_cfg ln_sla .true.
    set_namelist namelist_cfg ln_slafb .true.
    set_namelist namelist_cfg ln_seaice .false.
    set_namelist namelist_cfg ln_bkgwri .true.
    set_namelist namelist_cfg ln_trainc .true.
    set_namelist namelist_cfg ln_dyninc .true.
    set_namelist namelist_cfg ln_sshinc .true.
    set_namelist namelist_cfg ln_asmiau .true.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_OBS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

   cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
    set_namelist namelist_cfg ln_t3d .true.
    set_namelist namelist_cfg ln_s3d .true.
    set_namelist namelist_cfg ln_profb .true.
    set_namelist namelist_cfg ln_sst .true.
    set_namelist namelist_cfg ln_sstfb .true.
    set_namelist namelist_cfg ln_sla .true.
    set_namelist namelist_cfg ln_slafb .true.
    set_namelist namelist_cfg ln_seaice .false.
    set_namelist namelist_cfg ln_bkgwri .true.
    set_namelist namelist_cfg ln_trainc .true.
    set_namelist namelist_cfg ln_dyninc .true.
    set_namelist namelist_cfg ln_sshinc .true.
    set_namelist namelist_cfg ln_asmiau .true.
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_OBS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi
# TEST FOR ORCA2_LIM_AGRIF : simple test of running AGRIF (no restartability neither reproducibility tests)
if [ ${config} -eq 16 ] ;  then
    ## ORCA2_LIM with Agulhas AGRIF zoom in MPI
    export TEST_NAME="SHORT"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2AGUL_1_2 -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_agrif" del_key "key_zdftmx" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=2
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 1
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 2
#
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend 150
    set_namelist 1_namelist_cfg ln_ctl .false.
    set_namelist 1_namelist_cfg ln_clobber .true.

    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

# test code corruption with AGRIF
# Compile and run with or without AGRIF ORCA2_LIM
if [ ${config} -eq 17 ] ;  then
    # First run same as 16 but without zoom
    export TEST_NAME="SHORT_NOZOOM"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2AGUL_2_2 -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_agrif" del_key "key_zdftmx" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
# 
#   Set the number of fine grids to zero:    
    sed -i "1s/.*/0/" ${EXE_DIR}/AGRIF_FixedGrids.in

    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    export TEST_NAME="SHORT_NOAGRIF"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2AGUL_2_2_NAG -r ORCA2_LIM -j 8 add_key "key_mpp_rep" del_key "key_zdftmx" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
#
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

fi

## Restartability tests for ORCA2_LIM_AGRIF 
if [ ${config} -eq 18 ] ;  then
    export TEST_NAME="LONG"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2AGUL_LONG -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_agrif" del_key "key_zdftmx" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=4
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2LP_LONG\"
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2
    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
#
    set_namelist 1_namelist_cfg cn_exp \"O2LP_LONG\"
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend 300
    set_namelist 1_namelist_cfg nn_stock 150
    set_namelist 1_namelist_cfg ln_ctl .false.
    set_namelist 1_namelist_cfg ln_clobber .true.
#
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    
    cd ${SETTE_DIR}
    export TEST_NAME="SHORT"
    . ./prepare_exe_dir.sh
    cd ${EXE_DIR}
    set_namelist namelist_cfg cn_exp \"O2LP_SHORT\"
    set_namelist namelist_cfg nn_it000 76
    set_namelist namelist_cfg nn_itend 150
    set_namelist namelist_cfg nn_stock 75
    set_namelist namelist_cfg ln_rstart .true.
    set_namelist namelist_cfg nn_rstctl 2
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 2

    set_namelist namelist_cfg jpnij 4
    set_namelist namelist_cfg nn_solv 2
    set_namelist 1_namelist_cfg cn_exp \"O2LP_SHORT\"
    set_namelist 1_namelist_cfg nn_it000 151
    set_namelist 1_namelist_cfg nn_itend 300
    set_namelist 1_namelist_cfg nn_stock 150
    set_namelist 1_namelist_cfg ln_rstart .true.
    set_namelist 1_namelist_cfg nn_rstctl 2
    set_namelist 1_namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg cn_ocerst_in \"O2LP_LONG_00000075_restart\"
    set_namelist namelist_ice_cfg cn_icerst_in \"O2LP_LONG_00000075_restart_ice\"

    set_namelist 1_namelist_cfg cn_ocerst_in \"O2LP_LONG_00000150_restart\"

    for (( i=1; i<=$NPROC; i++)) ; do
        L_NPROC=$(( $i - 1 ))
        L_NPROC=`printf "%04d\n" ${L_NPROC}`
        ln -sf ../LONG/O2LP_LONG_00000075_restart_${L_NPROC}.nc .
        ln -sf ../LONG/O2LP_LONG_00000075_restart_ice_${L_NPROC}.nc .
        ln -sf ../LONG/1_O2LP_LONG_00000150_restart_${L_NPROC}.nc .
    done
    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

## Reproducibility tests for ORCA2_LIM_AGRIF
if [ ${config} -eq 19 ] ;  then
    export TEST_NAME="REPRO_4_4"
    cd ${CONFIG_DIR}
    . ./makenemo -m ${CMP_NAM} -n ORCA2AGUL_16 -r ORCA2_LIM -j 8 add_key "key_mpp_rep key_agrif" del_key "key_zdftmx" del_key ${DEL_KEYS}
    cd ${SETTE_DIR}
    . ./param.cfg
    . ./all_functions.sh
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 4
    set_namelist namelist_cfg jpnj 4
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
#
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend 150
    set_namelist 1_namelist_cfg ln_ctl .false.
    set_namelist 1_namelist_cfg ln_clobber .true.

    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

    cd ${SETTE_DIR}
    export TEST_NAME="REPRO_2_8"
    . ./prepare_exe_dir.sh
    JOB_FILE=${EXE_DIR}/run_job.sh
    NPROC=16
    if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    cd ${EXE_DIR}
    set_namelist namelist_cfg nn_it000 1
    set_namelist namelist_cfg nn_itend 75
    set_namelist namelist_cfg ln_ctl .false.
    set_namelist namelist_cfg ln_clobber .true.
    set_namelist namelist_cfg nn_fwb 0
    set_namelist namelist_cfg jpni 2
    set_namelist namelist_cfg jpnj 8
    set_namelist namelist_cfg jpnij 16
    set_namelist namelist_cfg nn_solv 2
#
    set_namelist 1_namelist_cfg nn_it000 1
    set_namelist 1_namelist_cfg nn_itend 150
    set_namelist 1_namelist_cfg ln_ctl .false.
    set_namelist 1_namelist_cfg ln_clobber .true.

    if [ ${USING_MPMD} == "yes" ] ; then
       set_xio_using_server iodef.xml true
    else
       set_xio_using_server iodef.xml false
    fi
    cd ${SETTE_DIR}
    . ./prepare_job.sh input_ORCA2_LIM_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS}
    cd ${SETTE_DIR}
    . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
fi

done
