#!/bin/bash -f
# set -vx
# simple SETTE report generator.
#
# This version should be run in the SETTE directory. 
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../cfgs directory)
#
#########################################################################################
######################### Start of function definitions #################################
##

function get_dorv() {
  if [ $lastchange == 'old' ] ; then 
    dorv=`ls -1rt $vdir/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    dorv2=`ls -1rt $vdir/$mach/ 2>/dev/null | tail -1l `
    dorv2=`echo $dorv2 | sed -e 's:.*/::'`
  else
    dorv=$lastchange
    dorv2=$lastchange
  fi
}

function get_ktdiff() {
  ktdiff=`diff ${1} ${2} | head -2 | grep it | awk '{ print $4 }'`
}

function get_ktdiff2() {
  ktdiff=`diff ${1} ${2} |  head -2 | tail -1l | awk '{print $2}'`
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
# get $dorv
  get_dorv
#
# check if directory is here
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "%-27s %s %s\n" $nam  " directory                  MISSING : " $dorv
    return
  fi

  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # check ocean output
    runtest $vdir $nam $pass RST
    #
    # run restartibility test
    f1o=$vdir/$mach/$dorv/$nam/LONG/ocean.output
    f1s=$vdir/$mach/$dorv/$nam/LONG/run.stat
    f1t=$vdir/$mach/$dorv/$nam/LONG/tracer.stat
    f2o=$vdir/$mach/$dorv/$nam/SHORT/ocean.output
    f2s=$vdir/$mach/$dorv/$nam/SHORT/run.stat
    f2t=$vdir/$mach/$dorv/$nam/SHORT/tracer.stat

    if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then 
      printf "%-27s %s\n" $nam " incomplete test";
      return; 
    fi
    if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then 
      printf "%-27s %s\n" $nam " incomplete test";
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
          printf "%-27s %s %s\n" $nam  " run.stat    restartability  passed : " $dorv
        fi
      else
        get_ktdiff f1.tmp$$ $f2s
        printf "\e[38;5;196m%-27s %s %s %s %-5s %s\e[0m\n" $nam  " run.stat    restartability  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
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
          printf "%-27s %s %s\n" $nam  " tracer.stat restartability  passed : " $dorv
        fi
      else
        get_ktdiff2 f1.tmp$$ $f2t
        printf "\e[38;5;196m%-27s %s %s %s %-5s %s\e[0m\n" $nam  " tracer.stat    restartability  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
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
# get $dorv
  get_dorv
#
# check if directory is here
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "%-27s %s %s\n" $nam  " directory                  MISSING : " $dorv
    return
  fi
#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # check ocean output
    runtest $vdir $nam $pass REPRO
    #
    # check reproducibility
    rep1=`ls -1rt $vdir/$mach/$dorv/$nam/ | grep REPRO | tail -2l | head -1 `
    rep2=`ls -1rt $vdir/$mach/$dorv/$nam/ | grep REPRO | tail -1l`
    if [ $rep1 == $rep2 ]; then
       rep2=''
    fi
    f1o=$vdir/$mach/$dorv/$nam/$rep1/ocean.output
    f1s=$vdir/$mach/$dorv/$nam/$rep1/run.stat
    f1t=$vdir/$mach/$dorv/$nam/$rep1/tracer.stat
    f2o=$vdir/$mach/$dorv/$nam/$rep2/ocean.output
    f2s=$vdir/$mach/$dorv/$nam/$rep2/run.stat
    f2t=$vdir/$mach/$dorv/$nam/$rep2/tracer.stat

    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then 
      printf "%-27s %s\n" $nam " incomplete test";
      return; 
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then 
      printf "%-27s %s\n" $nam " incomplete test";
      return; 
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "%-27s %s %s\n" $nam  " run.stat    reproducibility passed : " $dorv
        fi
      else
        get_ktdiff $f1s $f2s
        printf "\e[38;5;196m%-27s %s %s %s %-5s %s\e[0m\n" $nam  " run.stat    reproducibility FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
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
        if [ $pass == 0 ]; then           printf "%-27s %s %s\n" $nam  " tracer.stat reproducibility passed : " $dorv
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "\e[38;5;196m%-27s %s %s %s %-5s %s\e[0m\n" $nam  " tracer.stat reproducibility FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
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
function runcmpres(){
#
# compare *.stat file with reference file from a previous sette test or previous version
# store in NEMO_VALID_REF at revision NEMO_REV_REF
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
  pass=$5
#
# get $dorv
  get_dorv
#
# check if reference directory is present
  if [ ! -d $vdirref/$mach/$dorvref/$nam ]; then
    printf "%-27s %s\n" $nam " REFERENCE directory at $dorvref is MISSING"
    return
  fi
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "%-27s %s\n" $nam " VALID     directory at $dorv is MISSING"
    return
  fi

#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    f1s=$vdir/$mach/$dorv/$nam/LONG/run.stat
    f1t=$vdir/$mach/$dorv/$nam/LONG/tracer.stat
    f2s=$vdirref/$mach/$dorvref/$nam/LONG/run.stat
    f2t=$vdirref/$mach/$dorvref/$nam/LONG/tracer.stat
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
          printf "%-20s %s %s\n" $nam  " run.stat    files are identical "
        fi
      else
        get_ktdiff $f1s $f2s
        printf "%-20s %s %s %-5s %s\n" $nam  " run.stat    files are DIFFERENT (results are different after " $ktdiff " time steps)"
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    # Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then          
          printf "%-20s %s %s\n" $nam  " tracer.stat files are identical "
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "%-20s %s %s %-5s %s\n" $nam  " tracer.stat files are DIFFERENT (results are different after " $ktdiff " time steps) "
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
        fi
      fi
    fi
  fi
}

function runcmptim(){
#
# compare timing.output file with reference file from a previous sette test or previous version
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
  pass=$5
#
# get $dorv
  get_dorv
#
# check if reference directory is present
  if [ ! -d $vdirref/$mach/$dorvref/$nam ]; then
    return
  fi
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    return
  fi

#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    f1a=$vdir/$mach/$dorv/$nam/LONG/timing.output
    f2a=$vdirref/$mach/$dorvref/$nam/LONG/timing.output
#
# Report average CPU time differences (if available)
#
    if  [ -f $f1a ] && [ -f $f2a ] ; then
      tnew=$(grep 'Average ' $f1a  | awk '{print $5}')
      tref=$(grep 'Average ' $f2a  | awk '{print $5}')
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          tdif=$( echo ${tnew} ${tref} | awk '{print $1 - $2}')
          if (( $(echo "$tnew > $tref" |bc -l) )); then
            printf "%-20s %14s %10s %14s %10s %14s \\e[41;33;196m%10s\\e[0m\n" $nam  " ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          else
            printf "%-20s %14s %10s %14s %10s %14s \\e[42;01;196m%10s\\e[0m\n" $nam  " ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          fi
        fi
      fi
    fi
  fi
}

function runtest(){
#
# Run checks.
# Check presence of E R R O R in ocean.output from each
#
  vdir=$1
  nam=$2
  pass=$3
  ttype=$4
  [[ $ttype == 'RST' ]] && ttype="LONG|SHORT"
#
# get $dorv
  get_dorv
#
# no print needed if the repository is not here (already catch before)
#
  if [ -d $vdir/$mach/$dorv/$nam/ ]; then
    #
    # apply check for all ttype directory
    rep1=$(ls -rt $vdir/$mach/$dorv/$nam/ | grep -E $ttype)
    for tdir in $rep1 ; do
       f1o=$vdir/$mach/$dorv/$nam/$tdir/ocean.output
       if  [ ! -f $f1o ] ; then
          if [ $pass == 0 ]; then printf "%-27s %s %s\n" $nam " ocean.output               MISSING : " $dorv ; fi
          return;
       else 
          nerr=`grep 'E R R O R' $f1o | wc -l`
          if [[ $nerr > 0 ]]; then
             printf "\e[38;5;196m%-27s %s %s %s\e[0m\n" $nam " run                         FAILED : " $dorv " ( E R R O R in ocean.output) " 
             if [ $pass == 1 ]; then
                echo "<return> to view end of ocean.output"
                read y
                tail -100 $f1o
                echo ''
                echo "full ocean.output available here: $f1o"
             fi
             return;
          fi
       fi
    done
  else
    if [ $pass == 0 ]; then printf "%-27s %s %s\n" $nam  " directory                  MISSING : " $dorv ; fi
  fi
}

function identictest(){
#
# Checks AGRIF does not corrupt results with no AGRIF zoom by comparing run.stat files
#
  vdir=$1
  nam=$2
  nam2=$3
  pass=$4
#
  get_dorv
#
  if [ -d $vdir/$mach/$dorv/$nam ] ; then
   rep=`ls -1rt $vdir/$mach/$dorv/$nam/ |  tail -1l`
   f1s=${vdir}/${mach}/${dorv}/${nam}/${rep}/run.stat
   f2s=${vdir}/${mach}/${dorv2}/${nam2}/${rep}/run.stat
#
   if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
          if [ $pass == 0 ]; then 
	      printf "%-5s %s %-5s %s %s %s\n" $rep "AGRIF vs" $rep "NOAGRIF run.stat    unchanged  -    passed : " $dorv $dorv2
          fi
      else
          get_ktdiff $f1s $f2s
          printf "\e[38;5;196m%-5s %s %-5s %s %s %s %s %-5s %s\e[0m\n" $rep "AGRIF vs" $rep "NOAGRIF run.stat    changed  -     FAILED : " $dorv $dorv2 " (results are different after " $ktdiff " time steps)"
#
# Offer view of differences on the second pass
#
          if [ $pass == 1 ]; then
	      echo "<return> to view run.stat differences"
	      read y
	      sdiff $f1s $f2s
	      echo "<return> to continue"
	      read y
          fi
      fi
   else
      printf "%-27s %-27s %s\n" $nam $nam2 " incomplete test"
   fi
  else
      printf "%-27s %-27s %s\n" " " " " " non-existent test directory"
  fi
}
########################### END of function definitions #################################
##                                                                                     ##
##    Main script                                                                      ##
##                                                                                     ##
#########################################################################################
#
# LOAD param variable (COMPILER, NEMO_VALIDATION_DIR )
  SETTE_DIR=$(cd $(dirname "$0"); pwd)
  MAIN_DIR=$(dirname $SETTE_DIR)
  . ./param.cfg
  if [ -z $USER_INPUT ] ; then USER_INPUT='yes' ; fi        # Default: yes => request user input on decisions.
                                                            # (but may br inherited/imported from sette.sh)

  mach=${COMPILER}
# overwrite revision (later) or compiler
  if [ $# -gt 0 ]; then
    while getopts r:R:c:v:V:uh option; do 
       case $option in
          c) mach=$OPTARG;;
          r) rev=$OPTARG;;
          R) refrev=$OPTARG;;
          v) SETTE_SUB_VAL=$OPTARG;;
          V) SETTE_SUB_VAL2=$OPTARG
             if [ -d ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} ] ; then
               export NEMO_VALIDATION_REF=${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2}
             else
               echo "Requested comparison subdirectory: ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} does not exist"
             fi
             ;;
          u) USER_INPUT='no';;
          h | *) echo ''
                 echo 'sette_rpt.sh : ' 
                 echo '     display result for the latest change'
                 echo ' -c COMPILER_name :'
                 echo '     display result for the specified compiler'
                 echo ' -r REVISION_number :'
                 echo '     display sette results for the specified revision (set old for the latest revision available for each config)'
                 echo ' -R REFERENCE REVISION_number :'
                 echo '     compare sette results against the specified revision (use to over-ride value set in param.cfg)'
                 echo ' -v sub_dir :'
                 echo '     validation sub-directory below NEMO_VALIDATION_DIR'
                 echo ' -V sub_dir2 :'
                 echo '     2nd validation sub-directory below NEMO_VALIDATION_DIR'
                 echo '     if set the comparison is between two subdirectory trees beneath NEMO_VALIDATION_DIR'
                 echo ' -u to run sette_rpt.sh without any user interaction'
                 echo ''
                 exit 42;;
       esac
    done
    shift $((OPTIND - 1))
  fi
# if $1 (remaining arguments)
  if [[ ! -z $1 ]] ; then rev=$1 ; fi

  # Check that git branch is usable
  git branch --show-current >&/dev/null
  if [[ $? == 0 ]] ; then
    # subdirectory below NEMO_VALIDATION_DIR defaults to branchname
    NAM_MAIN="$(git branch --show-current)"
  else
    # subdirectory below NEMO_VALIDATION_DIR defaults to "MAIN"
    NAM_MAIN="MAIN"
  fi
  if [ ! -z $SETTE_SUB_VAL ] ; then
   export NEMO_VALIDATION_DIR=$NEMO_VALIDATION_DIR/$SETTE_SUB_VAL
   if [ -d $NEMO_VALIDATION_REF/$SETTE_SUB_VAL ] && [ -z $SETTE_SUB_VAL2 ] && [ ${USER_INPUT} == "yes" ] ; then
    while true; do
        read -p "$NEMO_VALIDATION_REF/$SETTE_SUB_VAL exists. Do you wish to use it as a reference? " yn
        case $yn in
            [Yy]* ) export NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/$SETTE_SUB_VAL; break;;
            [Nn]* ) echo "Ok, continuing with ${NEMO_VALIDATION_REF}/${NAM_MAIN} as the reference directory"
                    export NEMO_VALIDATION_REF=${NEMO_VALIDATION_REF}/${NAM_MAIN}
                    break
                    ;;
            * ) echo "Please answer yes or no.";;
        esac
    done
   elif [ -d $NEMO_VALIDATION_REF/$SETTE_SUB_VAL ] && [ -z $SETTE_SUB_VAL2 ] ; then
    # No user input: make a best guess as to intent
    export NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/$SETTE_SUB_VAL
   elif [ -z $SETTE_SUB_VAL2 ] ; then
    # No user input: default to branchname or MAIN
    export NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/${NAM_MAIN}
   fi
  else
   export NEMO_VALIDATION_DIR=${NEMO_VALIDATION_DIR}/${NAM_MAIN}
   if [ -z $SETTE_SUB_VAL2 ] ; then
    export NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/${NAM_MAIN}
   fi
  fi
  NEMO_VALID=${NEMO_VALIDATION_DIR}
  NEMO_VALID_REF=${NEMO_VALIDATION_REF}
  if [ ! -z $refrev ] ; then
    NEMO_REV_REF=${refrev}
  fi
#
  if [ ! -d $NEMO_VALID ]; then
    echo "$NEMO_VALID validation directory not found"
    exit
  fi
#
#
# Show current revision tag and branch name
#
echo ""
localchanges=`git status --short -uno | wc -l`
# Check that git branch is usable and use it to detect detached HEADs
git branch --show-current >& /dev/null
if [[ $? == 0 ]] ; then
  branchname="$(git branch --show-current)"
  if [ -z $branchname ] ; then
   # Probabably on a detached HEAD (possibly testing an old commit).
   # Verify this and try to recover original commit
   MORE_INFO="$(git branch -a | head -1l | sed -e's/.*(//' -e 's/)//' )"
   if [[ "${MORE_INFO}" == *"detached"* ]] ; then
     revision=$( echo \\${MORE_INFO} | awk '{print $NF}' )
     # There is no robust way to recover a branch name in a detached state
     # so just use the commit with a prefix
     branchname="detached_"${revision}
   else
     branchname="Unknown"
   fi
  else
   revision=`git rev-list --abbrev-commit origin | head -1l`
  fi
else
  branchname="Unknown"
fi
rev_date0=`git log -1 | grep Date | sed -e 's/.*Date: *//' -e's/ +.*$//'`
rev_date=`${DATE_CONV}"${rev_date0}" +"%y%j"`
revision=${rev_date}_${revision}
if [[ $localchanges > 0 ]] ; then
 echo "Current code is : $branchname @ $revision  ( with local changes )"
 lastchange=${revision}+
else
 echo "Current code is : $branchname @ $revision"
 lastchange=$revision
fi

# by default use the current lastchanged revision
lastchange=${rev:-$lastchange}

echo ""
echo "SETTE validation report generated for : "
echo ""
if [[ $localchanges > 0 ]] ; then
 echo "       $branchname @ $revision (with local changes)"
else
 echo "       $branchname @ $revision"
fi
echo ""
echo "       on $COMPILER arch file"
echo ""

#
# The script also needs the date or revision tag. Currently this is taken from the latest sub-directory found in each directory
#  
for pass in  $RPT_PASSES 
do
#
 if [ $pass == 0 ]; then 
   echo "" 
   echo "!!---------------1st pass------------------!!"
 fi
 if [ $pass == 1 ]; then
    echo ""
    echo "!!---------------2nd pass------------------!!"
 fi
#

# Restartability test
 echo ""
 echo "   !----restart----!   "
 for restart_test in GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 ORCA2_SAS_ICE AGRIF_DEMO WED025 ISOMIP+ OVERFLOW LOCK_EXCHANGE VORTEX ICE_AGRIF SWG
 do
   resttest $NEMO_VALID $restart_test $pass
 done
#
# Reproducibility tests
 echo ""
 echo "   !----repro----!   "
 for repro_test in GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 ORCA2_SAS_ICE ORCA2_ICE_OBS AGRIF_DEMO WED025 ISOMIP+ VORTEX ICE_AGRIF SWG
 do
   reprotest $NEMO_VALID $repro_test $pass
 done

# AGRIF special check to ensure results are unchanged with and without key_agrif
 echo ""
 echo "   !----agrif check----!   "
 dir1=AGRIF_DEMO_NOAGRIF
 dir2=AGRIF_DEMO
 identictest $NEMO_VALID $dir1 $dir2 $pass 
#
# before/after tests
 if [ $lastchange == 'old' ] ; then
    echo ""
    echo "   !---- 'old' specified as revision => no comparison with reference results ----!   "
    echo ""
 else
   echo ""
   echo "   !----result comparison check----!   "
   if [ $NEMO_VALID_REF != "/path/to/reference/sette/results" ]; then
     echo ''
     echo 'check result differences between :'
     echo "VALID directory : $NEMO_VALID at rev $lastchange"
     echo 'and'
     echo "REFERENCE directory : $NEMO_VALID_REF at rev $NEMO_REV_REF"
     echo ''
     checklist=(GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 ORCA2_SAS_ICE AGRIF_DEMO WED025 ISOMIP+ VORTEX ICE_AGRIF OVERFLOW LOCK_EXCHANGE SWG)
     for repro_test in ${checklist[@]}
     do
       runcmpres $NEMO_VALID $repro_test $NEMO_VALID_REF $NEMO_REV_REF $pass
     done
     echo ''
     echo 'Report timing differences between REFERENCE and VALID (if available) :'
     for repro_test in ${checklist[@]}
     do
       runcmptim $NEMO_VALID $repro_test $NEMO_VALID_REF $NEMO_REV_REF $pass
     done
   else
     echo ''
     echo ' No path for comparison specified. Result are not compare with any other revision. '
     echo ' To do it please fill NEMO_VALID_REF and NEMO_REV_REF in param.cfg. '
     echo ''
   fi
 fi
done
#
exit
