#!/bin/bash -f
# set -vx

SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)
USE_REF=0

. ./param.cfg

if [ $# -gt 0 ]; then
  while getopts c:v:Rh option; do 
     case $option in
        c) COMPILER=$OPTARG;;
        v) SETTE_SUB_VAL=$OPTARG;;
        R) USE_REF=1;;
        h | *) echo ''
               echo 'sette_list_avail_rev.sh : ' 
               echo '     list all sette directory and available revisions created with the compiler specified in param.cfg or in the startup file)'
               echo '-c COMPILER_name :'
               echo '     list all sette directory and available revisions created with the compiler specified'
               echo ' -v sub_dir :'
               echo '     validation sub-directory below NEMO_VALIDATION_DIR'
               echo ''
               exit 42;;
     esac
  done
  shift $((OPTIND - 1))
fi
if [ ! -z $SETTE_SUB_VAL ] ; then
 NEMO_VALIDATION_DIR=$NEMO_VALIDATION_DIR/$SETTE_SUB_VAL
 NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/$SETTE_SUB_VAL
else
 NEMO_VALIDATION_DIR=$NEMO_VALIDATION_DIR/MAIN
 NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/MAIN
fi

#
lst_rev () {
    # get the list of revision available for a configuration
    # base directory
    VALSUB=$1
    # config name
    CONFIG=$2
    # list of all revision available
    ALLLST=${@:3}
    # display
    printf "\n %-28s : " $CONFIG
    for rev in $ALLLST
    do
       if [ -d ${VALSUB}/$rev/${CONFIG} ]  ; then
          printf "%-14s  " $rev
       else
          printf "%-14s  " "------------ " 
       fi
    done
}


  NEMO_VALID=${NEMO_VALIDATION_DIR}/
  if [ ${USE_REF} == 1 ] ; then 
    NEMO_VALID=${NEMO_VALIDATION_REF}/
  fi
 
 # list of all revision available
 DIRLIST=`find ${NEMO_VALID}/${COMPILER} -maxdepth 1 -mindepth 1 -type d | sort -u`
 DIRLIST=`basename -a $DIRLIST`

 # display header
 echo ""
 echo " Compiler used is : $COMPILER"
 echo ""
 printf " List of all avail. rev. in   :"${NEMO_VALID}"\n"
 printf "                         is   : "
 for dir in `echo $DIRLIST`; do printf "%-14s  " $dir ; done
 printf "\n"

 # start checking configuration revision
 echo " Availability for each config.: "
 echo -n " ------------------------------"
 for CONFIG in GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 WED025 ORCA2_ICE_OBS ORCA2_SAS_ICE AGRIF_DEMO SWG ISOMIP+ OVERFLOW LOCK_EXCHANGE VORTEX ICE_AGRIF 
 do
    DIR=${NEMO_VALID}/${COMPILER}/
    lst_rev $DIR $CONFIG $DIRLIST
 done
 printf "\n"
 printf "\n"
#
exit
