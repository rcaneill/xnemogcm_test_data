#!/bin/sh
#set -x

# Available manuals
manuals='NEMO SI3 TOP'

# Initialization of the options
x_p='' ; x_r=''

# Choice of the options ---
while getopts hpr: option; do
        case $option in
                ('h') cat <<EOF
Usage:
------
./manual_build.sh [-p] [-r version ] manual
Mandatory
          Chose one model manual among ${manuals} or all (if not provided compile "NEMO")
Optional
   -p     Produce manual with figures and disable draft watermark
   -r     Specify the release version number of the manual(s) instead of the branch name
EOF
                                 exit 0 ;;
            ('p') x_p=0                 ;;
            ('r') x_r=${OPTARG}         ;;
        esac
done
shift $(( $OPTIND - 1 ));

VER_NUM=${x_r}
DRAFT=${x_p}
CMP_NAM=$1

## Initialisation
##---------------

# Model
if [ "${CMP_NAM}" == 'all' ] ; then
    models="$manuals"
elif [ "${CMP_NAM}" ==  '' ]; then
    models='NEMO'
elif [[ "${manuals}" == *"${CMP_NAM}"* ]] ; then
    models="${CMP_NAM}"
else
    echo "No entry for manual ${CMP_NAM}"
    exit
fi

# Version
version=`git symbolic-ref -q --short HEAD || git describe --tags --exact-match || "orphan"`
if [ "x${VER_NUM}" != 'x' ] ; then
    version=${VER_NUM}
fi

# Draft mode
ifdraft=1 ; draft_opt="using draft mode"
if [ "x${DRAFT}" != 'x' ] ; then
    ifdraft=0 ; draft_opt=""
fi

echo "Compiling ${models} manual(s) at version ${version} ${draft_opt} \n"

# Set version
version=`echo $version | sed -e 's;_;\\\\\\\_;g'`
prefile="latex/global/preamble.tex"
sed -i '' -e "s;\\\def\\\ver.*;\\\def\\\ver{$version};" ${prefile}

# Set draft mode
docfile="latex/global/document.tex"
pkgfile="latex/global/packages.tex"

draft_opt=`grep draft $docfile`
pkg_dft=`grep draftwatermark $pkgfile | sed -e "s;%;;g"`
if [ $ifdraft == 1 ] && [ "x$draft_opt" == "x" ] ; then
   sed -i '' -e "s;,abstract;,abstract,draft;" $docfile
   sed -i '' -e "s;.*draftwatermark.*;\\$pkg_dft;" $pkgfile
elif [ $ifdraft == 0 ] && [ "x$draft_opt" != "x" ] ; then
   sed -i '' -e "s;abstract,draft;abstract;" $docfile
   sed -i '' -e "s;.*draftwatermark.*;%\\$pkg_dft;" $pkgfile
fi

# Echo current font path
fontawesome=`grep defaultfontfeatures latex/global/packages.tex | sed -e 's/.*=\(.*\)}/\1/'`
echo "Fontawesome path is $fontawesome \n"

# Source shared functions
. tools/shr_func.sh


## Check dependancies
##-------------------

## LaTeX installation, find latexmk should be enough
[ -z "$( which latexmk )" ] && { echo 'latexmk not installed => QUIT'; exit 2; }

## Pygments package for syntax highlighting of source code (namelists & snippets)
[ -n "$( ./tools/check_pkg.py pygments )" ] && { echo 'Python pygments is missing => QUIT'; exit 2; }

## Loop on the models
##-------------------

for model in $models; do
    echo "Start compiling manual for $model"
#    clean $model
    build $model
    printf "\t¤ End of building run\n"
    echo
done

exit 0
