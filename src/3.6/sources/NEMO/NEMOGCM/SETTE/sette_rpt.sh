#!/bin/bash -f
#
# simple SETTE report generator.
#
# This version should be run in the SETTE directory. 
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../CONFIG directory)
#
#########################################################################################
######################### Start of function definitions #################################
##
function resttest() { 
#
# Restartability checks. Expects LONG and SHORT run directories
# Compares end of LONG stat files with equivalent entries from the SHORT stat files.
#
  vdir=$1
  nam=$2
  pass=$3
  mach=$4
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    f1o=$vdir/$nam/$mach/$dorv/LONG/ocean.output
    f1s=$vdir/$nam/$mach/$dorv/LONG/solver.stat
    f1t=$vdir/$nam/$mach/$dorv/LONG/tracer.stat
    f2o=$vdir/$nam/$mach/$dorv/SHORT/ocean.output
    f2s=$vdir/$nam/$mach/$dorv/SHORT/solver.stat
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
          printf "%-20s %s %s\n" $nam  " solver.stat restartability  passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " solver.stat restartability  FAILED : " $dorv 
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view solver.stat differences"
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
  mach=$4
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    rep1=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -2l | head -1 `
    rep2=`ls -1rt $vdir/$nam/$mach/$dorv/ | tail -1l`
    f1o=$vdir/$nam/$mach/$dorv/$rep1/ocean.output
    f1s=$vdir/$nam/$mach/$dorv/$rep1/solver.stat
    f1t=$vdir/$nam/$mach/$dorv/$rep1/tracer.stat
    f2o=$vdir/$nam/$mach/$dorv/$rep2/ocean.output
    f2s=$vdir/$nam/$mach/$dorv/$rep2/solver.stat
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
          printf "%-20s %s %s\n" $nam  " solver.stat reproducibility passed : " $dorv
        fi
      else
        printf "%-20s %s %s\n" $nam  " solver.stat reproducibility FAILED : " $dorv 
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view solver.stat differences"
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

function xiostest() { 
#
# XIOS functionality checks. Expects ATTACHED_MULTIPLE, ATTACHED_ONE, DETACHED_MULTIPLE and
# DETACHED_ONE run directories.
# Compares stat files from ATTACHED_MULTIPLE with equivalent entries from the others
#
  vdir=$1
  nam=$2
  pass=$3
  mach=$4
#
  if [ -d $vdir/$nam ]; then
    dorv=`ls -1rt $vdir/$nam/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    f1o=$vdir/$nam/$mach/$dorv/ATTACHED_MULTIPLE/ocean.output
    f1s=$vdir/$nam/$mach/$dorv/ATTACHED_MULTIPLE/solver.stat
    f1t=$vdir/$nam/$mach/$dorv/ATTACHED_MULTIPLE/tracer.stat
    if [ $pass == 0 ]; then printf "%-20s %s %s\n" $nam " checking against " "ATTACHED_MULTIPLE" ; fi
    for OTHER in ATTACHED_ONE DETACHED_MULTIPLE DETACHED_ONE
    do
      f2o=$vdir/$nam/$mach/$dorv/$OTHER/ocean.output
      f2s=$vdir/$nam/$mach/$dorv/$OTHER/solver.stat
      f2t=$vdir/$nam/$mach/$dorv/$OTHER/tracer.stat

      if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then 
        printf "%-20s %s %s\n" $nam " incomplete test " $OTHER;
        return; 
      fi
      if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then 
        printf "%-20s %s %s\n" $nam " incomplete test " $OTHER;
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
            printf "%-20s %s %s %s\n" $nam  " solver.stat restartability  passed : " $dorv $OTHER
          fi
        else
          printf "%-20s %s %s %s\n" $nam  " solver.stat restartability  FAILED : " $dorv  $OTHER
#
# Offer view of differences on the second pass
#
          if [ $pass == 1 ]; then
            echo "<return> to view solver.stat differences"
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
            printf "%-20s %s %s %s\n" $nam  " tracer.stat restartability  passed : " $dorv $OTHER
          fi
        else
          printf "%-20s %s %s %s\n" $nam  " tracer.stat restartability  FAILED : " $dorv  $OTHER
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
    done
  fi
}

########################### END of function definitions #################################
##                                                                                     ##
##    Main script                                                                      ##
##                                                                                     ##
#########################################################################################
#
  mach1=`grep "COMPILER=" ./sette.sh | sed -e 's/COMPILER=//'`
  mach2=`grep "COMPILER=" ./sette_xios.sh | sed -e 's/COMPILER=//'`
  NEMO_VALID=`grep "NEMO_VALIDATION_DIR=" ./param.cfg | sed -e 's/NEMO_VALIDATION_DIR=//'`
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
# Restartability test
#
 for restart_test in WGYRE_LONG WISOMIP_LONG WORCA2LIMPIS_LONG WORCA2OFFPIS_LONG WAMM12_LONG WORCA2LIM3_LONG WSAS_LONG WORCA2AGUL_LONG
 do
   resttest $NEMO_VALID $restart_test $pass $mach1
 done
#
# sette_xios tests (allow for the possibility of a different compiler base)
#
 for xios_test in WGYRE_XIOS_LR WGYRE_XIOS_HR
 do
   xiostest $NEMO_VALID $xios_test $pass $mach2
 done
#
# Reproducibility tests
#
 for repro_test in WGYRE_4 WISOMIP_4 WORCA2LIMPIS_16 WORCA2OFFPIS_16 WAMM12_32 WORCA2LIM3_16 WORCA2_LIM_OBS WSAS_32 WORCA2AGUL_1_2 WORCA2AGUL_16 WORCA2AGUL_2_2_NAG
 do
   reprotest $NEMO_VALID $repro_test $pass $mach1
 done
#
done
exit
