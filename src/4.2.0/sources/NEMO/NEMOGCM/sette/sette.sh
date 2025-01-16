#!/bin/sh
# initialise user dependent variable
export cmd=$0 ; export cmdargs=$@
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)
export CMPL_CORES=8            # Number of threads to use for compiling
export SETTE_STG="_ST"         # Base suffix to append to configuration name
dry_run=0
NO_REPORT=0
#
# controls for some common namelist, run-time options:
#
export USING_TIMING='yes'      # Default: yes => set ln_timing=.true.   ; use -T to disable
export USING_ICEBERGS='yes'    # Default: yes => set ln_icebergs=.true. ; use -i to disable
export USING_EXTRA_HALO='yes'  # Default: yes => set nn_hls=2           ; use -e to set nn_hls=1
export USING_COLLECTIVES='yes' # Default: yes => set nn_comm=2          ; use -C to set nn_comm=1
export USING_NOGATHER='yes'    # Default: yes => set ln_nnogather=.true.; use -N to set ln_nnogather=.false.
export USING_TILING='yes'      # Default: yes => set ln_tile=.true.     ; use -t to disable
                               #    Note: yes also ensures nn_hls=2 but -t will not alter nn_hls 
#
# controls for some common compile-time keys:
#
export USING_QCO='yes'         # Default: yes => add key_qco            ; use -q to delete key_qco
export USING_RK3='no'          # Default: yes => add key_RK3 & key_qco  ; use -Q to delete key_RK3
export USING_LOOP_FUSION='yes' # Default: yes => add key_loop_fusion    ; use -F to delete key_loop_fusion
export USING_XIOS='yes'        # Default: yes => add key_xios           ; use -X to delete key_xios
                               #    Note: changing USING_XIOS may require a change in arch file
#
# controls for some common batch-script, run-time options:
#
export USING_MPMD='yes'        # Default: yes => run with detached XIOS servers ; use -A to run in attached (SPMD) mode
                               #    Note: yes also ensures key_xios but -A will not remove it
export USER_INPUT='yes'        # Default: yes => request user input on decisions. For example:
                               #                 1. regarding mismatched options
                               #                 2. regardin incompatible options
                               #                 3. regarding creation of directories
#
# Check that git branch is usable
export DETACHED_HEAD="no"
git branch --show-current >& /dev/null
if [[ $? == 0 ]] ; then
  # subdirectory below NEMO_VALIDATION_DIR defaults to branchname
  export SETTE_SUB_VAL="$(git branch --show-current)"
  if [ -z $SETTE_SUB_VAL ] ; then
   # Probabably on a detached HEAD (possibly testing an old commit).
   # Verify this and try to recover original commit
   MORE_INFO="$(git branch -a | head -1l | sed -e's/.*(//' -e 's/)//' )"
   if [[ "${MORE_INFO}" == *"detached"* ]] ; then
     export DETACHED_HEAD="yes"
     export DETACHED_CMIT=$( echo \\${MORE_INFO} | awk '{print $NF}' )
     # There is no robust way to recover a branch name in a detached state
     # so just use the commit with a prefix
     export SETTE_SUB_VAL="detached_"${DETACHED_CMIT}
   else
     export SETTE_SUB_VAL="Unknown"
   fi
  fi
  export SETTE_THIS_BRANCH=${SETTE_SUB_VAL}
else
  # subdirectory below NEMO_VALIDATION_DIR defaults to "MAIN"
  export SETTE_SUB_VAL="MAIN"
  export SETTE_THIS_BRANCH="Unknown"
fi

# Parse command-line arguments
if [ $# -gt 0 ]; then
  while getopts n:x:v:g:cdrshTqQteiACFNXu option; do 
     case $option in
        c) export SETTE_CLEAN_CONFIGS='yes'
           export SETTE_SYNC_CONFIGS='yes'
           echo "-c: Configuration ${SETTE_TEST_CONFIGS[@]} will be cleaned; this option enforces also synchronisation"
           echo "";;
        d) dry_run=1
           echo "";;
        r) NO_REPORT=1
           echo "";;
        s) export SETTE_SYNC_CONFIGS='yes'
           echo "-s: MY_SRC and EXP00 in ${SETTE_TEST_CONFIGS[@]} will be synchronised with the MY_SRC and EXPREF from the reference configuration"
           echo "";;
        n) OPTSTR="$OPTARG"
           OPTSTR="${OPTSTR/ORCA2_SAS_ICE/SAS}"              # Permit either shortened (expected) or full name for SAS
           OPTSTR="${OPTSTR/AGRIF_DEMO/AGRIF}"               # Permit either shortened (expected) or full name for AGRIF
           export SETTE_TEST_CONFIGS=(${OPTSTR})
           echo "=================================="
           if [ ${#SETTE_TEST_CONFIGS[@]} -gt 1 ]; then
             echo "-n: Configurations ${SETTE_TEST_CONFIGS[@]} will be tested if they are available"
           else
             echo "-n: Configuration ${SETTE_TEST_CONFIGS[@]} will be tested if it is available"
           fi
           echo "";;
        g) case $OPTARG in
             [0-9,a-z,A-Z] ) echo "-g: Using ${SETTE_STG}${OPTARG} as the configuration suffix";;
             * ) echo "-g only accepts a single, alphanumeric character. Processing halted"; exit 42;;
           esac
           export SETTE_STG=${SETTE_STG}${OPTARG}
           echo "";;
        x) export SETTE_TEST_TYPES=(${OPTARG})
           echo "-x: ${SETTE_TEST_TYPES[@]} tests requested"
           echo "";;
        v) export SETTE_SUB_VAL=($OPTARG)
           echo "-v: $SETTE_SUB_VAL validation sub-directory requested"
           echo "";;
        T) export USING_TIMING='no'
           echo "-T: ln_timing will be set to false"
           echo "";;
        t) export USING_TILING='no'
           echo "-t: ln_tile will be set to false"
           echo "";;
        e) export USING_EXTRA_HALO='no'
           echo "-e: nn_hls will be set to 1"
           echo "";;
        i) export USING_ICEBERGS='no'
           echo "-i: ln_icebergs will be set to false"
           echo "";;
        C) export USING_COLLECTIVES='no'
           echo "-C: nn_comm will be set to 1"
           echo "";;
        N) export USING_NOGATHER='no'
           echo "-N: ln_nnogather will be set to false"
           echo "";;
        q) export USING_QCO='no'
           echo "-q: key_qco and key_linssh will NOT be activated"
           echo "";;
        Q) export USING_RK3='no'
           echo "-Q: key_qco and key_RK3 will not be activated"
           echo "    This is the curent default for now since RK3 is not ready"
           echo "";;
        F) export USING_LOOP_FUSION='no'
           echo "-F: key_loop_fusion will not be activated"
           echo "";;
        X) export USING_XIOS='no'
           echo "-X: key_xios will not be activated"
           echo "";;
        A) export USING_MPMD='no'
           echo "-A: Tasks will be run in attached (SPMD) mode"
           echo "";;
        u) export USER_INPUT='no'
           echo "-u: sette.sh will not expect any user interaction == no safety net!" 
           echo "";;
        h | *) echo 'sette.sh with no arguments (in this case all configuration will be tested with default options)'
               echo '-T to set ln_timing false for all non-AGRIF configurations (default: true)'
               echo '-t set ln_tile false in all tests that support it (default: true)'
               echo '-e set nn_hls=1 (default: nn_hls=2)'
               echo '-i set ln_icebergs false (default: true)'
               echo '-C set nn_comm=1 (default: nn_comm=2 ==> use MPI3 collective comms)'
               echo '-N set ln_nnogather false for ORCA2 configurations (default: true)'
               echo '-q to remove the key_qco key (default: added)'
               echo '-X to remove the key_xios key (default: added)'
               echo '-F to remove the key_loop_fusion key (default: added)'
               echo '-Q to remove the key_RK3 key (currently a null-op since key_RK3 is not used)'
               echo '-A to run tests in attached (SPMD) mode (default: MPMD with key_xios)'
               echo '-n "CFG1_to_test CFG2_to_test ..." to test some specific configurations'
               echo '-x "TEST_type TEST_type ..." to specify particular type(s) of test(s) to run after compilation'
               echo '              TEST_type choices are: RESTART REPRO CORRUPT PHYSICS - anything else will COMPILE only'
               echo '-v "subdir" optional validation record subdirectory to be created below NEMO_VALIDATION_DIR'
               echo '-g "group_suffix" single character suffix to be appended to the standard _ST suffix used'
               echo '                  for SETTE-built configurations (needed if sette.sh invocations may overlap)'
               echo '-r to execute without waiting to run sette_rpt.sh at the end (useful for chaining sette.sh invocations)'
               echo '-d to perform a dryrun to simply report what settings will be used'
               echo '-c to clean each configuration'
               echo '-s to synchronise the sette MY_SRC and EXP00 with the reference MY_SRC and EXPREF'
               echo '-u to run sette.sh without any user interaction. This means no checks on creating'
               echo '          directories etc. i.e. no safety net!' ; exit 42 ;;
     esac
  done
  shift $((OPTIND - 1))
fi
#
# Option dependency tests
#
if [ ${USING_TILING} == "yes" ] ; then 
 if [ ${USING_EXTRA_HALO} == "no" ] ; then
  if [ ${USER_INPUT} == "yes" ] ; then
   while true; do
       read -p "Tiling requires the extra halo but you have used -e to deselect it. Would you like to reselect it? (y/n)?: " yn
       case $yn in
           [Yy]* ) echo "Ok, ignoring the -e option"; USING_EXTRA_HALO="yes"; break;;
           [Nn]* ) echo "Ok, exiting instead"; exit 42;;
           * ) echo "Please answer yes or no.";;
       esac
   done
  else
   # Without user input, the best option is to disable tiling
   echo "Tiling requires the extra halo but you have used -e to deselect it. Tiling will not be used."
   USING_TILING="no"
  fi
 fi
fi
if [ ${USING_LOOP_FUSION} == "yes" ] ; then 
 if [ ${USING_EXTRA_HALO} == "no" ] ; then
  if [ ${USER_INPUT} == "yes" ] ; then
   while true; do
       read -p "Loop fusion requires the extra halo but you have used -e to deselect it. Would you like to reselect it? (y/n)?: " yn
       case $yn in
           [Yy]* ) echo "Ok, ignoring the -e option"; USING_EXTRA_HALO="yes"; break;;
           [Nn]* ) echo "Ok, exiting instead"; exit 42;;
           * ) echo "Please answer yes or no.";;
       esac
   done
  else
   # Without user input, the best option is to disable loop fusion
   echo "Loop fusion requires the extra halo but you have used -e to deselect it. Loop fusion will not be used."
   USING_LOOP_FUSION="no"
  fi
 fi
fi
#
# Get SETTE parameters
. ./param.cfg

#
# Set the common compile keys to add or delete based on command-line arguments:
#
export ADD_KEYS="" ; export DEL_KEYS=""
if [ ${USING_XIOS} == "yes" ] ; then export ADD_KEYS="${ADD_KEYS}key_xios " ; fi
if [ ${USING_XIOS} == "no" ]  ; then export DEL_KEYS="${DEL_KEYS}key_xios " ; fi
#
if [ ${USING_LOOP_FUSION} == "yes" ] ; then export ADD_KEYS="${ADD_KEYS}key_loop_fusion " ; fi
if [ ${USING_LOOP_FUSION} == "no" ]  ; then export DEL_KEYS="${DEL_KEYS}key_loop_fusion " ; fi
#
if [ ${USING_QCO} == "yes" ] ; then export ADD_KEYS="${ADD_KEYS}key_qco " ; fi
if [ ${USING_QCO} == "no" ]  ; then export DEL_KEYS="${DEL_KEYS}key_qco key_linssh " ; fi
#
if [ ${USING_RK3} == "yes" ] ; then export ADD_KEYS="${ADD_KEYS}key_qco key_RK3 " ; fi
if [ ${USING_RK3} == "no" ]  ; then export DEL_KEYS="${DEL_KEYS}key_RK3 " ; fi

#
# Set validation record sub-directories (if required)
#
if [ ! -d $NEMO_VALIDATION_DIR ] ; then
 if [ ${dry_run} -eq 0 ] ; then
  if [ ${USER_INPUT} == "yes" ] ; then
   while true; do
       read -p "$NEMO_VALIDATION_DIR does not exist. Do you wish to create it? " yn
       case $yn in
           [Yy]* ) echo "Ok, creating $NEMO_VALIDATION_DIR"; mkdir $NEMO_VALIDATION_DIR; break;;
           [Nn]* ) echo "Ok, exiting instead"; exit 42;;
           * ) echo "Please answer yes or no.";;
       esac
   done
  else
       # Without user input, carry on regardless
       echo "$NEMO_VALIDATION_DIR does not exist. It will be created"
       mkdir $NEMO_VALIDATION_DIR
  fi
 else
  echo "$NEMO_VALIDATION_DIR does not exist"
  echo "but this is a dry run so it will not be created"
 fi
fi
if [ ! -d $NEMO_VALIDATION_DIR/$SETTE_SUB_VAL ] && [ ${dry_run} -eq 0 ] ; then
   mkdir $NEMO_VALIDATION_DIR/$SETTE_SUB_VAL
fi
export NEMO_VALIDATION_DIR=$NEMO_VALIDATION_DIR/$SETTE_SUB_VAL

if [ ${#SETTE_TEST_CONFIGS[@]} -eq 0 ]; then
   echo "=================================="
   echo "Configurations $TEST_CONFIGS will be tested if they are available"
fi
echo "Carrying out the following tests  : ${TEST_TYPES[@]}"
echo "requested by the command          : "$cmd $cmdargs
echo "on branch                         : "$SETTE_THIS_BRANCH
printf "%-33s : %s\n" USING_TIMING $USING_TIMING
printf "%-33s : %s\n" USING_ICEBERGS $USING_ICEBERGS
printf "%-33s : %s\n" USING_EXTRA_HALO $USING_EXTRA_HALO
printf "%-33s : %s\n" USING_TILING $USING_TILING
printf "%-33s : %s\n" USING_COLLECTIVES $USING_COLLECTIVES
printf "%-33s : %s\n" USING_NOGATHER $USING_NOGATHER
printf "%-33s : %s\n" USING_QCO $USING_QCO
printf "%-33s : %s\n" USING_LOOP_FUSION $USING_LOOP_FUSION
printf "%-33s : %s\n" USING_XIOS $USING_XIOS
printf "%-33s : %s\n" USING_MPMD $USING_MPMD
printf "%-33s : %s\n" USING_RK3 $USING_RK3
printf "%-33s : %s\n" USER_INPUT $USER_INPUT
printf "%-33s : %s\n" "Common compile keys to be added" "$ADD_KEYS"
printf "%-33s : %s\n" "Common compile keys to be deleted" "$DEL_KEYS"
echo "Validation records to appear under: "$NEMO_VALIDATION_DIR
echo "=================================="
echo ""
#
# Option compatibility tests
#
if [ ${USING_MPMD} == "yes" ] && [ ${USING_XIOS} == "no" ] ; then echo "Incompatible choices. MPMD mode requires the XIOS server" ; exit ; fi

if [ ${dry_run} -eq 1 ] ; then echo "dryrun only: no tests performed" ; exit ; fi

# run sette on reference configuration
. ./sette_reference-configurations.sh
if [[ $? != 0 ]]; then
   echo ""
   echo "--------------------------------------------------------------"
   echo "./sette_cfg-ref.sh didn't finish properly, need investigations"
   echo "--------------------------------------------------------------"
   echo ""
   exit 42
fi

# run sette on test cases
. ./sette_test-cases.sh
if [[ $? != 0 ]]; then
   echo ""
   echo "-----------------------------------------------------------------"
   echo "./sette_test-cases.sh didn't finish properly, need investigations"
   echo "-----------------------------------------------------------------"
   echo ""
   exit 42
fi

if [ ${NO_REPORT} -ne 0 ] ; then exit ; fi
# run sette report
echo ""
echo "-------------------------------------------------------------"
echo "./sette_rpt.sh (script will wait all nemo_sette run are done)"
echo "-------------------------------------------------------------"
echo ""
NRUN=999
NIT=0
while [[ $NRUN -ne 0 && $nit -le 1080 ]]; do
   nit=$((nit+1))
   NRUN=$( ${BATCH_STAT} | grep ${BATCH_NAME} | wc -l ) 
   if [[ $NRUN -ne 0 ]]; then 
      printf "%-3d %s\r" $NRUN 'nemo_sette runs still in queue or running ...';
   else
      printf "%-50s\n" " "
      . ./sette_rpt.sh
      exit
   fi
   sleep 10
done
printf "\n"
echo ""
echo "Something wrong happened, it tooks more than 3 hours to run all the sette tests"
echo ""
