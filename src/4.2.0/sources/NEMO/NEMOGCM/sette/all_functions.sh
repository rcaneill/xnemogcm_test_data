######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
# Some scripts called by sette.sh
# all_functions.sh   : all functions used by sette.sh  
######################################################
#set -x
#set -o posix
#set -u
#set -e
#+
#
# ================
# all_functions.sh
# ================
#
# ----------------------------------------------
# Set of functions used by sette.sh (NEMO tests) 
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./set_namelist INPUT_NAMELIST VARIABLE VALUE 
#  $ post_test_tidyup 
#
#
# DESCRIPTION
# ===========
#
# function superegrep
#   input variable value
#
# function set_namelist
#   input namelist_name variable value
#   output namelist
#
# function post_test_tidyup
#   creates nemo_validation tree, and save output & debug files
#   this function creates tree of validation in NEMO_VALIDATION_DIR as follows : 
#
# NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/REVISION_NUMBER(or DATE)/TEST_NAME
# 
# NEMO_VALIDATION_DIR           : is choosen in param.cfg
#
# WCONFIG_NAME                  : set by makenemo at the moment of compilation
#
# WCOMPILER_NAME                : set by makenemo at the moment of compilation
#
# REVISION_NUMBER(or DATE)      : revision number by svn info, if problems with svn date is taken
#
# TEST_NAME                     : set in sette.sh for each configuration to be tested (directory TEST_NAME is created under ${NEW_CONF} directory )
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./set_namelist namelist          nn_itend        75
#  $ ./set_namelist namelist_ice      cn_icerst_in  \"00101231_restart_ice\"
#  $ post_test_tidyup 
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: all_functions.sh 14981 2021-06-11 13:47:19Z acc $
#
#   * creation
#-
# function to find namelists parameters
supergrep () {
            grep "^ *$1 *=" $2 | sed -e "s% *\!.*%%"
    }

usage=" Usage : set_namelist input_namelist variable_name value"
usage=" if value is a string ths is neede syntax : ./set_namelist namelist_name var_name \"new_value\" "

# sync MYSRC files (input CFG and CFG_STg; where g is an optional, single, alphanumeric character)
sync_config() {
   if [ ${SYNC_CONFIGS} == "yes" ]; then

      lREF=$3/$1    # reference 
      lCFG=$3/$2    # target

      echo '-------------------------------------------------------------------------------'
      echo '                    SOURCE AND CONFIG FILES SYNCHRONISATION                    '
      echo ''
      echo "configuration $lCFG/MY_SRC will be synchronised with $lREF/MY_SRC"
      echo ''
      # synchronise MY_SRC if $lCFG directory exist
      if [ -d $lREF/MY_SRC ] && [ -d $lCFG ] ; then

         if [ -d $lCFG/MY_SRC ] ; then

            # Manage case number of file in MY_SRC changes
            # find number of file only in $lCFG/MY_SRC
            FCFG=`diff -r $lREF/MY_SRC $lCFG/MY_SRC | grep "Only in $lCFG/MY_SRC" | awk '{print $4}'`

            # if more than 0, trigger a cleaning as Makenemo do not properly manage this case.
            if [ ! -z "$FCFG" ]; then 
               echo ''
               echo "$lCFG contains a different file list than $lREF :"
               diff -r $lREF/MY_SRC $lCFG/MY_SRC | grep "Only in $lCFG/MY_SRC" | awk '{print $4}'
               echo "Synchronisation is not enough because Makenemo does not handle well case where a file in MY_SRC is removed"
               echo "Therefore, we need to clean $lCFG before starting runing it"
               echo ''
               CLEAN_CONFIGS="yes"
            fi
         fi

         # synchronisation $lREF/MY_SRC directory (and delete file in target location if needed)
         rsync -a --delete $lREF/MY_SRC/ $lCFG/MY_SRC

         # rsync keep preserve the modification time stamp.
         # To avoid case where a file in MY_SRC is replace by an older file, we touch the file
         touch --no-create $lCFG/MY_SRC/*
      fi

      echo ''
      echo "configuration $lCFG/EXP00 will be synchronised with $lREF/EXPREF"
      echo "(links are skipped)"
      echo ''
      # synchronize EXPREF
      if [ -d $lREF/EXPREF ] && [ -d $lCFG/EXP00 ] ; then
         rsync -a --no-links $lREF/EXPREF/ $lCFG/EXP00/.
      fi
      echo '-------------------------------------------------------------------------------'
   fi
}

# clean _STg config (input CFG CFG_STg TYPE (test or ref))
clean_config() {
   if [ ${CLEAN_CONFIGS} == "yes" ]; then
      lREF=$1
      lCFG=$2
      lTYP=$3
      echo ''
      echo '-------------------------------------------------------------------------------'
      echo '                         CLEANING CONFIGURATION                                '
      echo ''
      echo "./makenemo -n $lCFG -a/-r $lREF clean"
      echo ''
      if [ ${lTYP} == 'tests' ]; then
         ./makenemo -n $lCFG -a $lREF clean
      elif [ ${lTYP} == 'cfgs' ]; then
         ./makenemo -n $lCFG -r $lREF clean
      else
         echo 'ERROR in the cleaning process'; exit 42
      fi
      echo ''
      echo "$lCFG configuration has been cleaned"
      echo ''
      echo '-------------------------------------------------------------------------------'
   fi
}

# define validation dir
set_valid_dir () {
    if [ ${DETACHED_HEAD} == "no" ] ; then
      REVISION_NB=`git -C ${MAIN_DIR} rev-list --abbrev-commit origin | head -1l`
    else
      REVISION_NB=${DETACHED_CMIT}
    fi
    REV_DATE0="`git -C ${MAIN_DIR} log -1 | grep Date | sed -e 's/.*Date: *//' -e's/ +.*$//'`"
    REV_DATE=`${DATE_CONV}"${REV_DATE0}" +"%y%j"`
    REVISION_NB=${REV_DATE}_${REVISION_NB}
    if [ ${#REVISION_NB} -eq 0 ]
    then
        echo "some problems with git rev-list command"
        echo "some problems with git rev-list command" >> ${SETTE_DIR}/output.sette
        REVISION_NB=`date +%Y%m%d`
        echo "put in ${REVISION_NB} date"
        echo "put in ${REVISION_NB} date" >> ${SETTE_DIR}/output.sette
    else
    echo "value of revision number of NEMOGCM: ${REVISION_NB}"
    fi
    localchanges=`git -C ${MAIN_DIR} status --short -uno | wc -l`
    if [[ $localchanges > 0 ]] ; then
     REVISION_NB=${REVISION_NB}+
    fi
    # remove last _ST followed by zero or more alphanumeric characters
    NEW_CONF1=$( echo $NEW_CONF | sed -e 's/_ST\([0-9a-zA-Z]*\)$//' )
    export NEMO_VALID=${NEMO_VALIDATION_DIR}/${CMP_NAM}/${REVISION_NB}/${NEW_CONF1}/${TEST_NAME}
}

# clean valid dir (move old ocean_output/run.stat and tracer to avoid checking them in case something wrong happen.
clean_valid_dir () {
#   set_valid_dir # already done in sette_ref/sette_test
   echo "validation directory is : $NEMO_VALID"
   if [ -d $NEMO_VALID ] ; then
      [ -f ${NEMO_VALID}/ocean.output ] && mv ${NEMO_VALID}/ocean.output ${NEMO_VALID}/ocean.output_old
      [ -f ${NEMO_VALID}/run.stat ]     && mv ${NEMO_VALID}/run.stat     ${NEMO_VALID}/run.stat_old
      [ -f ${NEMO_VALID}/tracer.stat ]  && mv ${NEMO_VALID}/tracer.stat  ${NEMO_VALID}/tracer.stat_old
   fi
}

# set_namelist_opt: function to set namelists parameters based on a yes/no selection
# Mandatory arguments are, in order:
# 1. namelist to be edited
# 2. variable to be set
# 3. yes or no switch setting
# 4. value to set variable to if switch is yes
# 5. value to set variable to if switch is no
set_namelist_opt () {
	minargcount=5
	if [ ${#} -lt ${minargcount} ]
	then
		echo "not enough arguments for set_namelist_opt" >> ${SETTE_DIR}/output.sette
		echo "Usage: set_namelist_opt namelist varname yes_or_no value_if_yes value_if_no" >> ${SETTE_DIR}/output.sette
		exit 1
	fi
	unset minargcount
        if [ $3 != 'yes' ] && [ $3 != 'no' ] ; then
                echo 'option switch must be "yes" or "no"' >> ${SETTE_DIR}/output.sette
                echo "${usage}" >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ $3 == 'yes' ] ; then
                set_namelist $1 $2 $4
        else
                set_namelist $1 $2 $5
        fi
}
        
# set_namelist: function to set namelists parameters
set_namelist () {
	minargcount=3
	if [ ${#} -lt ${minargcount} ]
	then
		echo "not enough arguments for set_namelist"
		echo "${usage}"
		exit 1
	fi
	unset minargcount
	if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette
      
	VAR_NAME=$( supergrep $2 ${EXE_DIR}/$1 )
	if [ ${#VAR_NAME} -eq 0 ] 
	then
		echo "doing \"set_namelist $@\". "  >> ${SETTE_DIR}/output.sette
		echo "variable: \"$2\" not found in \"${EXE_DIR}/$1\" "  >> ${SETTE_DIR}/output.sette
                NAMREF=$( basename $1 _cfg )_ref
		echo "doing more : search in ${EXE_DIR}/$NAMREF " >> ${SETTE_DIR}/output.sette
                VAR_NAME=$( supergrep $2 ${EXE_DIR}/$NAMREF )
	        if [ ${#VAR_NAME} -eq 0 ] 
	        then
                    echo " variable $VAR_NAME not found in ${EXE_DIR}/$1 nor in ${EXE_DIR}/$NAMREF "
                    echo " check your variable name "
        		echo "exit"
	        	echo "error in executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
	        	echo "....." >> ${SETTE_DIR}/output.sette
        		exit 1
                fi
                LINEVAR=$( grep -s -n "$VAR_NAME" ${EXE_DIR}/$NAMREF | awk -F: '{ { print $1} }' )
                echo " $VAR_NAME found in ${EXE_DIR}/$NAMREF at line $LINEVAR " >> ${SETTE_DIR}/output.sette

#   search for namelist group name
                NAMGRP=$( head -n$LINEVAR ${EXE_DIR}/$NAMREF | grep --line-buffered "^&nam" | tail -1 | awk -F" " '{ { print $1} }' ) 
                echo " variable $VAR_NAME will be added in $NAMGRP namelist-group of namelist file ${EXE_DIR}/$1 " >> ${SETTE_DIR}/output.sette

# check if namelist group present in namelist_cfg
# if missing group is added at the end of namelist_cfg
                NGRP=$(grep ${NAMGRP} ${EXE_DIR}/$1 | wc -l )
                if [ ${NGRP} -eq 0 ]; then
                   echo ''                                                                          >> ${SETTE_DIR}/output.sette
                   echo "+++++ Group ${NAMGRP} containing ${2} is missing in ${EXE_DIR}/$1 +++++ "  >> ${SETTE_DIR}/output.sette
                   echo "+++++ Group ${NAMGRP}                 is added   in ${EXE_DIR}/$1 +++++ "  >> ${SETTE_DIR}/output.sette
                   echo ''                                                                          >> ${SETTE_DIR}/output.sette
                   echo "${NAMGRP}" >> ${EXE_DIR}/$1
                   echo "/"         >> ${EXE_DIR}/$1
                fi

# Add $VARNAME in namelist file ${EXE_DIR}/$1 in namelist group $NAMGRP
# on mac osx, replace sed --posix by gsed (available with mac port)
                sed --posix "/${NAMGRP} /a\ ${VAR_NAME} " ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp || gsed --posix "/${NAMGRP} /a\ ${VAR_NAME} " ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp

# if file not empty replace ${EXE_DIR}/$1
               if [ -s ${EXE_DIR}/$1.tmp ] ; then
                   mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1 
                else
                echo "file ${EXE_DIR}/$1.tmp is empty. sed command went wrong "; exit 200
                fi
	fi

        ARGS_LST="${@:3}"
        sed -e "s;${VAR_NAME}.*;${VAR_NAME};" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1
        sed -e "s;${VAR_NAME};$2=${ARGS_LST};"  ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}


# function to tidy up after each test and populate the NEMO_VALIDATION store
post_test_tidyup () {
# Save current exit status of caller script 
    RUN_STATUS=$? 
    echo "Exit status: ${RUN_STATUS}" 
#
# requires the following variables defined and exported from the calling script:
#  SETTE_DIR
#  INPUT_DIR
#  EXE_DIR
#  CONFIG_DIR
#  NEMO_VALIDATION_DIR
#  NEW_CONF
#  CMP_NAM
#  TEST_NAME
echo "SETTE directory is : ${SETTE_DIR}"
echo "INPUT directory is : ${INPUT_DIR}"
echo "EXECUTION directory is : ${EXE_DIR}"
echo "CONFIG directory is : ${CONFIG_DIR}"
echo "VALIDATION directory is : ${NEMO_VALID}"
echo "NEW CONFIGURATION is : ${NEW_CONF}"
echo "COMPILER is : ${CMP_NAM}"
echo "TEST is : ${TEST_NAME}"
echo "TOOLS directory is : ${TOOLS_DIR}"
################################################################
# SMALL DEBUG
    EXIT_STATUS=${RUN_STATUS}
    if [ ! -r ${EXE_DIR}/ocean.output ]
        then
        grep "E R R O R" ${EXE_DIR}/ocean.output && echo "Some ERRORS at execution time, see ${EXE_DIR}/ocean.output"
        EXIT_STATUS=2
        # exit 2 Error now catch in the report 
    fi

    if [ ! -r ${EXE_DIR}/time.step ]
        then
        echo "file time.step does not exist"   >> ${SETTE_DIR}/output.sette
        echo "some problems during execution of model"  >> ${SETTE_DIR}/output.sette 
        EXIT_STATUS=1
        # exit 1 Error now catch in the report
    else
        echo "file time.step exists"  >> ${SETTE_DIR}/output.sette
        echo "execution of model time step loop started"   >> ${SETTE_DIR}/output.sette
    fi

################################################################

################################################################
#
# Creation of NEMO_VALIDATION tree
#    set_valid_dir already done in sette_reference_config
    mkdir -p ${NEMO_VALIDATION_DIR}
    if [ -d ${NEMO_VALIDATION_DIR} ] ; then
	echo "created ${NEMO_VALIDATION_DIR} directory"   >> ${SETTE_DIR}/output.sette
    else 
	echo "problems in creating ${NEMO_VALIDATION_DIR} directory"   >> ${SETTE_DIR}/output.sette
	echo "EXIT,"
	exit 1
    fi
# 
# Exit before populating validation directory if the model run has 
# returned a non-zero exit status 
# On CRAY NEMO exit is not the expected 999 or 123456 (let this bloc in case useful later on).
#    case ${EXIT_STATUS} in
#        0|123456|999) echo " NEMO finished with exit code $EXIT_STATUS " ; post_test_tidyup ;;
#        *) echo " NEMO abort on an unexpected error (segmentation fault or whatever) $EXIT_STATUS "
#    esac

    [ ${EXIT_STATUS} -ne 0 ] && exit ${EXIT_STATUS}  
#
# Save output & debug files in NEMO_VALIDATION tree
    echo "saving ocean & ice output, run.stat, tracer.stat files ...." >> ${SETTE_DIR}/output.sette
    echo "            " >> ${SETTE_DIR}/output.sette
    [ -f ${EXE_DIR}/ocean.output ] && cp ${EXE_DIR}/*ocean.output ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/run.stat ] && cp ${EXE_DIR}/*run.stat ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/output.namelist.dyn ] && cp ${EXE_DIR}/*output.nam* ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/namelist_cfg ] && cp ${EXE_DIR}/*nam*_cfg ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/tracer.stat ] && cp ${EXE_DIR}/*tracer.stat ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/timing.output ] && cp ${EXE_DIR}/*timing.output ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/sette_config ] && cp ${EXE_DIR}/sette_config ${NEMO_VALIDATION_DIR}/.

    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*run*)" ] ; then
	echo "moved run.stat in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved run.stat in ${NEMO_VALIDATION_DIR} directory"  
    else
	echo "problem in looking for run.stat file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "run.stat IS NOT in ${NEMO_VALIDATION_DIR} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*ocean.output*)" ] ; then
	echo "moved ocean.output in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved ocean.output in ${NEMO_VALIDATION_DIR} directory" 
    else
	echo "problem in looking for ocean.output file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "ocean.output IS NOT in ${NEMO_VALIDATION_DIR} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*tracer.stat*)" ] ; then
        echo "moved tracer.stat in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "moved tracer.stat in ${NEMO_VALIDATION_DIR} directory"
    else
        echo "problem in looking for tracer.stat file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "tracer.stat IS NOT in ${NEMO_VALIDATION_DIR} directory"
    fi
}

#############################################################
# extra functions to manipulate settings in the iodef.xml file
#
# Examples:
#   set_xio_file_type    iodef.xml one_file
#   set_xio_using_server iodef.xml true
#   set_xio_buffer_size  iodef.xml 50000000
#   set_xio_field_defs   iodef.xml
#
#############################################################

usage2=" Usage : set_xio_file_type input_iodef.xml one_file||multiple_file"
usage3=" Usage : set_xio_using_server input_iodef.xml true||false"
usage4=" Usage : set_xio_buffer_size input_iodef.xml int_buffer_size"
usage5=" Usage : set_xio_field_defs input_iodef.xml"

set_xio_file_type () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_file_type"
                echo "${usage2}"
                exit 1
        fi
        if [ $2 != "one_file" ] && [ $2 != "multiple_file" ]
        then
                echo "unrecognised argument for set_xio_file_type"
                echo "${usage2}"
                echo $2
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*file_definition.*type.*=" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_file_type $@\". "
                echo "xml_tag: file_definition with variable: type is empty"
                echo "confirm that an appropriate file_definition is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ $2 == "one_file" ] 
        then
           sed -e "s:multiple_file:one_file:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        else
           sed -e "s:one_file:multiple_file:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        fi
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_using_server () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_using_server"
                echo "${usage2}"
                exit 1
        fi
        inarg=$2
        if [ ${inarg} == "yes" ] ; then inarg="true" ; fi
        if [ ${inarg} == "no" ]  ; then inarg="false" ; fi
        if [ ${inarg} != "true" ] && [ ${inarg} != "false" ]
        then
                echo "unrecognised argument for set_xio_using_server"
                echo "${usage2}"
                echo ${inarg}
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*variable id.*=.*using_server.*=.*bool" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_using_server $@\". "
                echo "xml_tag: "variable id=using_server" with variable: bool is empty"
                echo "confirm that an appropriate variable id is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ ${inarg} == "false" ]
        then
           sed -e "/using_server/s:true:false:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
           export USING_MPMD=no
        else
           sed -e "/using_server/s:false:true:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
           export USING_MPMD=yes
        fi
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_buffer_size () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_buffer_size"
                echo "${usage4}"
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*variable id.*=.*buffer_size.*=.*integer" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_buffer_size $@\". "
                echo "xml_tag: "variable id=buffer_size" with variable: integer is empty"
                echo "confirm that an appropriate variable id is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        sed -e "/buffer_size/s:>.*<:>$2<:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_field_defs () {
        minargcount=1
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_field_defs"
                echo "${usage5}"
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_field_defs $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette


        [ -f ${EXE_DIR}/field_def_nemo-oce.xml ] || sed -i '/field_def_nemo-oce/d' $1
        [ -f ${EXE_DIR}/field_def_nemo-ice.xml ] || sed -i '/field_def_nemo-ice/d' $1
        [ -f ${EXE_DIR}/field_def_nemo-pisces.xml ] || sed -i '/field_def_nemo-pisces/d' $1

        echo "finished script : set_xio_field_defs $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}
