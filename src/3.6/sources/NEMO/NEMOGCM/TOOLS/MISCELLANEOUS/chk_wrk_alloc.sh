#!/bin/bash
#
# purpose:
#   small script to check in all *90 files of the current directory and subdirectories 
#   if all lines with wrk_alloc have their corresponding lines with wrk_dealloc
#
# use:
#   call chk_wrk_alloc.sh from the directory you want to check
#
# example:
#   cd ~/dev_NEMO_MERGE_2011/NEMOGCM/NEMO
#   ../TOOLS/MISCELLANEOUS/chk_wrk_alloc.sh
#
set -u
#
echo "check for all *90 files contained in "$( pwd )" and its subdirectories"
#
for ff in $( grep -il "^ *use  *wrk_nemo" $( find . -name "*90" )  $( find . -name "*h90" ) )
do
    ierr=0
    
    # number of lines with wrk_alloc
    n1=$( grep -ic "call *wrk_alloc *(" $ff )
    # number of lines with wrk_dealloc
    nn1=$( grep -ic "call *wrk_dealloc *(" $ff )  
    
    if [ $(( $n1 + $nn1 )) -ne 0 ]
    then
	# replace wrk_alloc with wrk_dealloc and count the lines
	n2=$( sed -e "s/wrk_alloc/wrk_dealloc/" $ff | grep -ic "call *wrk_dealloc *(" )
	# we should get n2 = 2 * n1...
	if [ $(( 2 * $n1 )) -ne $n2 ]
	then
	    ierr=1
	    echo "problem with wrk_alloc in $ff" 
	fi
	# same story but for wrk_dealloc
	nn2=$( sed -e "s/wrk_dealloc/wrk_alloc/" $ff | grep -ic "call *wrk_alloc *(" )
	if [ $(( 2 * $nn1 )) -ne $nn2 ]
	then
	    ierr=1
	    echo "problem with wrk_dealloc in $ff" 
	fi

	if [ $ierr -eq 0 ] # check that wrk_alloc block is the same as wrk_dealloc block
	then
	    grep -i "call *wrk_alloc *("   $ff | sed -e "s/ //g" | sed -e "s/!.*//g" > txt1$$
	    grep -i "call *wrk_dealloc *(" $ff | sed -e "s/wrk_dealloc/wrk_alloc/"  | sed -e "s/ //g" | sed -e "s/!.*//g" > txt2$$
	    cmp txt1$$ txt2$$
	    if [ $? -ne 0 ]
	    then
		echo "different syntax in wrk_alloc and wrk_dealloc in $ff"
		echo
		for ll in $( seq 1 $n1 )  # compare each line
		do
		    sed -n ${ll}p txt1$$ > ll1$$
		    sed -n ${ll}p txt2$$ > ll2$$
		    cmp ll1$$ ll2$$ > /dev/null
		    if [ $? -ne 0 ]
		    then
			grep -i "call *wrk_alloc *("   $ff | sed -n ${ll}p
			grep -i "call *wrk_dealloc *(" $ff | sed -n ${ll}p
			echo
		    fi
		    rm -f ll1$$ ll2$$
		done
	    fi
	    rm -f txt1$$ txt2$$
	else
	    grep -i "call *wrk_alloc *(" $ff
	    echo
	    grep -i "call *wrk_dealloc *(" $ff	
	    echo
	fi
	
    fi
    
done
