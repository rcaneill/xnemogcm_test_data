#!/bin/bash -f
#set -vx
#
# simple SETTE report generator.
#
# This version should be run in the SETTE directory. 
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../cfgs directory)
#
#########################################################################################
######################### Start of function definitions #################################
##

function restfile() {
# Rebuild ice restart for SAS CONFIG, and restartability checks. Expects LONG and SHORT run directories.
# For Stand Alone Surface configuration ocean is not running, just run ice model; so no outputs ocean files.
# Compares LONG rebuild restart ice file with equivalent entry from the SHORT rebuild restart ice file.
#
  vdir=$1
  nam=$2
  pass=$3
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    rep1=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -2l | head -1 `
    rep2=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -1l`
    cd ${SAS_RESTART_DIR}/LONG
    #SF add here compilation of rebuild_tools to rebuild restart files, and add comparison of restart files
    cd ${TOOLS_DIR}
    ./maketools -n REBUILD_NEMO -m ${mach} > /dev/null 2>&1
    cd ${TOOLS_DIR}/REBUILD_NEMO
    #SF echo "REBUILD LONG restart SAS files, without standard output"
    ./rebuild_nemo -t 4 ../../cfgs/SAS_LONG/LONG/SAS_00000100_restart_ice  $NPROC > /dev/null 2>&1
    #SF echo "REBUILD SHORT restart SAS files, without standard output"
    ./rebuild_nemo -t 4 ../../cfgs/SAS_LONG/SHORT/SAS_00000100_restart_ice $NPROC >&-
    cd ${SAS_RESTART_DIR}/LONG
    #SF echo "COPY rebuild restart files"
    cp SAS_00000100_restart_ice.nc $vdir/$nam/$mach/$dorv/LONG/.
    cp ../SHORT/SAS_00000100_restart_ice.nc $vdir/$nam/$mach/$dorv/SHORT/.

    f1o=$vdir/$nam/$mach/$dorv/LONG/SAS_00000100_restart_ice.nc
    f2o=$vdir/$nam/$mach/$dorv/SHORT/SAS_00000100_restart_ice.nc
    if  [ ! -f $f1o ] &&  [ ! -f $f2o ] ; then
      printf "%-20s %s\n" $nam " REBUILD SAS restart ice DOES NOT exists; incomplete test";
      return;
    fi
    #
    done_oce=0
    #
    if  [  -f $f1o ] && [  -f $f2o ]; then
      cmp -s $f1o $f2o 
      #SF  cmp SAS_00000100_restart_ice.nc  ../SHORT/SAS_00000100_restart_ice.nc  > diff_restart.txt
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "%-20s %s %s\n" $nam  " SAS restart files are IDENTICAL :  passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " SAS restart files are DIFFERENT : FAILED : " $dorv 
        #
	# Offer view of differences on the second pass
	#
        if [ $pass == 1 ]; then
          echo "BE CAREFUL:  NEED cdo to see differences!!!!! "
          echo "DO which cdo and replace cdo PATH to the cdo command in SETTE_rpt.sh "
          echo "IF cdo is not available you need to do difference of netcdf file by hand"
          echo "<return> to view restart_ice.nc differences"
          read y
#SF           cdo -diffv $f1o $f2o
          /smplocal/pub/cdo/1.5.9/bin/cdo -diffv $f1o $f2o
          done_oce=1
          #echo "<return> to continue"
          #read y
        fi
      fi
    fi
#
fi
}

function resttest() { 
#
# Restartability checks. Expects LONG and SHORT run directories
# Compares end of LONG stat files with equivalent entries from the SHORT stat files.
#
  vdir=$1
  nam=$2
  pass=$3
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    f1o=$vdir/$nam/$mach/$dorv/LONG/ocean.output
    f1s=$vdir/$nam/$mach/$dorv/LONG/run.stat
    f1t=$vdir/$nam/$mach/$dorv/LONG/tracer.stat
    f2o=$vdir/$nam/$mach/$dorv/SHORT/ocean.output
    f2s=$vdir/$nam/$mach/$dorv/SHORT/run.stat
    f2t=$vdir/$nam/$mach/$dorv/SHORT/tracer.stat

    if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then 
      printf "%-20s %s\n" $nam " incomplete test";
      return; 
    fi
    if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then 
      printf "%-20s %s\n" $nam " incomplete test";
      return; 
    fi
#
    done_oce=0

    if  [  -f $f1s ] && [  -f $f2s ]; then 
      nl=(`wc -l $f2s`)
      tail -${nl[0]} $f1s > f1.tmp$$
      cmp -s f1.tmp$$ $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "%-20s %s %s\n" $nam  " run.stat restartability  passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " run.stat restartability  FAILED : " $dorv 
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff f1.tmp$$ $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [  -f $f1t ] && [  -f $f2t ]; then
      nl=(`wc -l $f2t`)
      tail -${nl[0]} $f1t > f1.tmp$$
      cmp -s f1.tmp$$ $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "%-20s %s %s\n" $nam  " tracer.stat restartability  passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " tracer.stat restartability  FAILED : " $dorv 
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff f1.tmp$$ $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    rm f1.tmp$$
  fi
}

function reprotest(){
#
# Reproducibility checks. Expects REPRO_N_M and REPRO_I_J run directories
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
  pass=$3
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    rep1=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -2l | head -1 `
    rep2=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -1l`
    f1o=$vdir/$nam/$mach/$dorv/$rep1/ocean.output
    f1s=$vdir/$nam/$mach/$dorv/$rep1/run.stat
    f1t=$vdir/$nam/$mach/$dorv/$rep1/tracer.stat
    f2o=$vdir/$nam/$mach/$dorv/$rep2/ocean.output
    f2s=$vdir/$nam/$mach/$dorv/$rep2/run.stat
    f2t=$vdir/$nam/$mach/$dorv/$rep2/tracer.stat

    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then 
      printf "%-20s %s\n" $nam " incomplete test";
      return; 
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then 
      printf "%-20s %s\n" $nam " incomplete test";
      return; 
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "%-20s %s %s\n" $nam  " run.stat reproducibility passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " run.stat reproducibility FAILED : " $dorv 
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff f1.tmp$$ $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then           printf "%-20s %s %s\n" $nam  " tracer.stat reproducibility passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " tracer.stat reproducibility  FAILED : " $dorv
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
  fi
}
########################### END of function definitions #################################
##                                                                                     ##
##    Main script                                                                      ##
##                                                                                     ##
#########################################################################################
#
  mach=`grep "COMPILER=" ./sette.sh | sed -e 's/COMPILER=//'`
  NEMO_VALID=`grep "NEMO_VALIDATION_DIR=" ./param.cfg | sed -e 's/NEMO_VALIDATION_DIR=//'`
# Directory to run the tests
 SETTE_DIR=$(cd $(dirname "$0"); pwd)
 MAIN_DIR=$(dirname $SETTE_DIR)
 CONFIG_DIR0=${MAIN_DIR}/cfgs
 TOOLS_DIR=${MAIN_DIR}/tools
 NPROC=32

  SAS_RESTART_DIR=${CONFIG_DIR0}/SAS_LONG
#
  if [ ! -d $NEMO_VALID ]; then
    echo "$NEMO_VALID validation directory not found"
    exit
  fi
#

# The script also needs the date or revision tag. Currently this is taken from the latest sub-directory found in each directory
#  
for pass in  0 1 
do
#
 if [ $pass == 1 ]; then echo "---------------2nd pass------------------";fi
#

# Rebuild and restartability test for SAS
#
 for restart_file in WSAS_LONG
 do
 #  restfile $SAS_RESTART_DIR LONG $pass
   restfile $NEMO_VALID $restart_file $pass
 done
#
# Restartability test
#
 for restart_test in WGYREPIS_LONG WORCA2ICEPIS_LONG WORCA2OFFPIS_LONG WAMM12_LONG WISOMIP_LONG WORCA2AGUL_LONG
 do
   resttest $NEMO_VALID $restart_test $pass
 done
#
# Reproducibility tests
#
 for repro_test in WGYREPIS_32 WORCA2ICEPIS_32 WORCA2OFFPIS_32 WAMM12_32 WISOMIP_32 WORCA2_ICE_OBS WORCA2AGUL_1_2 WORCA2AGUL_16 WORCA2AGUL_2_2_NAG
 do
   reprotest $NEMO_VALID $repro_test $pass
 done
#
done
exit
