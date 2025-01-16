#!/bin/bash
# set -vx
# Simple script to robustly run a full suite of SETTE tests
#
########################################
function wait_on_q()
{
# SETTE testing on ARCHER2 uses the test queue in which users are limited to
# 16 queued jobs (including a maximum of 4 running). To prevent sette testing
# breaching this limit each configuration is processed separately and 
# processing only begins when the user has no more than 12 jobs already queued.
# The supposition here is that each config forks no more than 4 tests - may need
# re-revaluating if used for PHYSICS tests.
#
# This function checks the queue usage and waits if necessary until the queue
# has drained sufficiently for the next test.
NRUN=999
NIT=0
BATCH_STAT="squeue -u $USER"
BATCH_NAME=sette
echo "Checking queues"
while [[ $NRUN -gt 12 && $nit -le 1080 ]]; do
   nit=$((nit+1))
   NRUN=$( ${BATCH_STAT} | grep ${BATCH_NAME} | wc -l )
   if [[ $NRUN -gt 12 ]]; then
      printf "%-3d %s\r" $NRUN 'nemo_sette runs still in queue or running ...';
   else
      printf "%-50s\n" "Queues sufficiently drained"
      return 99
   fi
   sleep 10
done
echo "Something has gone wrong. Excessive wait time has been exceeded"
exit
}
#
########################################
# Start of main script
########################################
FULLSET=( ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 AGRIF WED025 GYRE_PISCES SAS ORCA2_ICE_OBS SWG ICE_AGRIF OVERFLOW LOCK_EXCHANGE VORTEX ISOMIP+ )
#
GROUP_SETS=( "-r" )
#
# These groups sets correspond to the following test regimes:
#
# A. complete sets with various combinations of options:
#
  printf "%-93s %s\n" "Full tests - <branch_name> (using *_ST config dirs) : "  "${GROUP_SETS[0]}"
#
# A. Full tests 
for gs in 0
do
 for n in `seq 0 1 $(( ${#FULLSET[@]} - 1 ))`
 do
   confstr="${FULLSET[$n]}"
   # compile seperately since the final link sometimes has a bus error on ARCHER2 (which never happens on the 2nd attempt)
   echo ./sette.sh ${GROUP_SETS[$gs]} -x "COMPILE" -n "$confstr"
        ./sette.sh ${GROUP_SETS[$gs]} -x "COMPILE" -n "$confstr"
   # Now run the test (and finish linking if necessary)
   echo ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO CORRUPT" -n "$confstr"
        ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO CORRUPT" -n "$confstr"
   wait_on_q
 done
done
exit
