#!/bin/bash 
# set -vx
# simple SETTE result evaluator
#
# This version should be run in the SETTE directory. 
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../cfgs directory)
#
#########################################################################################
######################### Start of function definitions #################################
##
errcnt=0
nmatch=0
nrmiss=0
nvmiss=0
TESTD_ROOT=LONG

function get_testd() {
  sroot=$1/${TESTD_ROOT}
  ls -1d "$sroot"* &> /dev/null
  if [ $? -eq 0 ] ; then
   TESTD=`ls -1d "$sroot"* | head -1l`
   TESTD=`basename $TESTD`
   if [ ! -d $1/$TESTD ] ; then
     echo "TEST directory not found with rootname: "$TESTD_ROOT
     exit
   fi
  else
   TESTD=$TESTD_ROOT
  fi
}

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
  silent=$5
#
# get $dorv
  get_dorv
#
# check if reference directory is present
  if [ ! -d $vdirref/$mach/$dorvref/$nam ] || [ ! -d $vdir/$mach/$dorv/$nam ] ; then
   if [ ! -d $vdirref/$mach/$dorvref/$nam ]; then
     if [ $silent -eq 0 ] ; then
      printf "%-27s %s\n" $nam " REFERENCE directory at $dorvref is MISSING"
     else
      nrmiss=$(( $nrmiss + 1 ))
     fi
   fi
   if [ ! -d $vdir/$mach/$dorv/$nam ]; then
     if [ $silent -eq 0 ] ; then
      printf "%-27s %s\n" $nam " VALID     directory at $dorv is MISSING"
     else
      nvmiss=$(( $nvmiss + 1 ))
     fi
   fi
   return
  fi
  nmatch=$(( $nmatch + 1 ))

#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    get_testd $vdir/$mach/$dorv/$nam
    if  [ ! -d $vdir/$mach/$dorv/$nam/$TESTD ] ; then
      printf "%-20s %s (%s)\n" $nam " not tested" $TESTD;
      return;
    fi
    f1s=$vdir/$mach/$dorv/$nam/$TESTD/run.stat
    f1t=$vdir/$mach/$dorv/$nam/$TESTD/tracer.stat
    f2s=$vdirref/$mach/$dorvref/$nam/$TESTD/run.stat
    f2t=$vdirref/$mach/$dorvref/$nam/$TESTD/tracer.stat
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
        if [ $silent == 0 ]; then
          printf "%-20s %s (%s)\n" $nam  " run.stat    files are identical " $TESTD
        fi
      else
        get_ktdiff $f1s $f2s
        if [ $silent == 0 ]; then
         printf "%-20s %s %s %-5s (%s)\n" $nam  " run.stat    files are DIFFERENT (results are different after " $ktdiff " time steps) " $TESTD
        else
         errcnt=$(( $errcnt + 1 ))
        fi
#
      fi
    fi
    # Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $silent == 0 ]; then          
          printf "%-20s %s (%s)\n" $nam  " tracer.stat files are identical " $TESTD
        fi
      else
        get_ktdiff2 $f1t $f2t
        if [ $silent == 0 ]; then          
         printf "%-20s %s %s %-5s (%s)\n" $nam  " tracer.stat files are DIFFERENT (results are different after " $ktdiff " time steps) " $TESTD
        else
         errcnt=$(( $errcnt + 1 ))
        fi
      fi
#
    fi
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
  quiet=0
  . ./param.cfg
  USER_INPUT='yes'        # Default: yes => request user input on decisions.

  mach=${COMPILER}
# overwrite revision (later) or compiler
  if [ $# -gt 0 ]; then
    while getopts r:R:c:v:V:T:quh option; do 
       case $option in
          c) mach=$OPTARG;;
          r) rev=$OPTARG;;
          R) refrev=$OPTARG;;
          q) quiet=1;;
          v) SETTE_SUB_VAL=$OPTARG;;
          V) SETTE_SUB_VAL2=$OPTARG
             if [ -d ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} ] ; then
               export NEMO_VALIDATION_REF=${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2}
             else
               echo "Requested comparison subdirectory: ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} does not exist"
             fi
             ;;
          T) TESTD_ROOT=$OPTARG;;
          u) USER_INPUT='no';;
          h | *) echo ''
                 echo 'sette_eval.sh : ' 
                 echo '     display result for the latest change'
                 echo ' -c COMPILER_name :'
                 echo '     display result for the specified compiler'
                 echo ' -r REVISION_number :'
                 echo '     display sette results for the specified revision (set old for the latest revision available for each config)'
                 echo ' -R REFERENCE REVISION_number :'
                 echo '     compare sette results against the specified revision (use to over-ride value set in param.cfg)'
                 echo ' -T test_rootname :'
                 echo '     root of test name to be checked. Valid choices ares: LONG, SHORT, REPRO. [default: LONG]'
                 echo ' -v sub_dir :'
                 echo '     validation sub-directory below NEMO_VALIDATION_DIR'
                 echo ' -V sub_dir2 :'
                 echo '     2nd validation sub-directory below NEMO_VALIDATION_DIR'
                 echo '     if set the comparison is between two subdirectory trees beneath NEMO_VALIDATION_DIR'
                 echo ' -u to run sette_eval.sh without any user interaction'
                 echo ' -q : Activate quiet mode - only the number of differing results is returned'
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
if [ ${quiet} -eq 0 ] ; then echo "" ; fi
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
 if [ ${quiet} -eq 0 ] ; then  echo "Current code is : $branchname @ $revision  ( with local changes )" ; fi
 lastchange=${revision}+
else
 if [ ${quiet} -eq 0 ] ; then echo "Current code is : $branchname @ $revision" ; fi
 lastchange=$revision
fi

# by default use the current lastchanged revision
lastchange=${rev:-$lastchange}

if [ ${quiet} -eq 0 ] ; then 
 echo ""
 echo "SETTE evaluation for : "
 echo ""
 if [[ $localchanges > 0 ]] ; then
  echo "       $branchname @ $revision (with local changes)"
 else
  echo "       $branchname @ $revision"
 fi
 echo ""
 echo "       on $COMPILER arch file"
 echo ""
fi

#
# The script also needs the date or revision tag. Currently this is taken from the latest sub-directory found in each directory
#  
# before/after tests
 if [ $lastchange == 'old' ] ; then
    echo ""
    echo "   !---- 'old' specified as revision => no comparison with reference results ----!   "
    echo ""
 else
   if [ ${quiet} -eq 0 ] ; then
    echo ""
    echo "   !----result comparison check----!   "
   fi
   if [ $NEMO_VALID_REF != "/path/to/reference/sette/results" ]; then
     if [ ${quiet} -eq 0 ] ; then
      echo ''
      echo 'check result differences between :'
      echo "VALID directory : $NEMO_VALID at rev $lastchange"
      echo 'and'
      echo "REFERENCE directory : $NEMO_VALID_REF at rev $NEMO_REV_REF"
      echo ''
     fi
     checklist=(GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 ORCA2_SAS_ICE ORCA2_ICE_OBS AGRIF_DEMO WED025 ISOMIP+ VORTEX ICE_AGRIF OVERFLOW LOCK_EXCHANGE SWG) 
     for repro_test in ${checklist[@]}
     do
        runcmpres $NEMO_VALID $repro_test $NEMO_VALID_REF $NEMO_REV_REF $quiet
     done
     if [ ${quiet} -eq 0 ] ; then 
      echo ''
     else
      if [ $(( $nrmiss + $nvmiss )) -gt 0 ] ; then
       echo $errcnt " differences from "$nmatch" matches. "$nrmiss" missing from REFERENCE "$nvmiss" missing from VALID"
      else
       echo $errcnt " differences from "$nmatch" matches. "
      fi 
     fi
   else
     echo ''
     echo ' No path for comparison specified. Result are not compare with any other revision. '
     echo ' To do it please fill NEMO_VALID_REF and NEMO_REV_REF in param.cfg. '
     echo ''
   fi
 fi
#
exit
