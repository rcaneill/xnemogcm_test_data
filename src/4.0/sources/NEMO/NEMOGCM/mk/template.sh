#!/bin/bash


#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#
# This template for Bash coding pair with Emacs Lisp package 'bash-font-lock.el'
# to enhance default shell syntax fontification. This package is based on regexs
# to match examples of concsyntax showed below.
# All this is not mandatory and probably not optimized with other correct rules
# Feel free to add your tips, modify it at your convenience or give your feedback
# to improve it.
#
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


# To automatically use it with a bash script, copy it to your ~/.emacs.d and 
# add '(load "~/.emacs.d/bash-font-lock.el")' in your .emacs configuration file
#--------------------------------------------------------------------------------
 # If a command  is not highlighted, add its name to 'bash-builtins'  list
 # "  " function ""  "       "     ,  "   "   ""  "" 'bash-functions'  "" 

# UNIX built-in commands sample
#-------------------------------
.; alias; bg; bind; builtin; caller; compgen; complete; declare; dirs; disown
enable; fc; fg; help; history; jobs; kill; let; local; popd; printf; pushd
shopt; source; suspend; typeset; unalias; eval; export; getopts; newgrp; pwd
read; readonly; times; ulimit; command; hash; test; type; cd; echo; eval; set
shift; umask; unset; wait

# Common bash built-in commands already added to 'bash-builtins' list
#---------------------------------------------------------------------
awk basename cat cp cut date diff dirname env find grep head ls make mkdir mv rm
sed sort svn tail tee touch uniq xargs

# Variables
#-----------
 # Specials    parameters (see Bash manual for details, `man bash`)
$0 $# $* $@ $? $! $_ $$ $-
 # Positionnal parameters (ordered arguments given to run script "$0")
$1 $2 $3 ...                
$* ==  "$1${IFS}$2${IFS}$3..." #   Single word , recommended use for string
$@ ==  "$1"    "$2"    "$3"...  # Separate words, recommended use for array
 # Identify locals against GLOBALS or ENVIRONMENT variables with case sensitive
TEMP=${temp_0123}; temp=${TEMP_0123}; export TEMP=${TEMP_0123}
 # Possibles variable assignation syntax
temp='temp'; temp=$1 # Simple
length_temp=${#temp} # Length of string
temp=$(( 1 + 1 ))    # Integer arithmetic evaluation
temp=${........}     # String operations
temp=$(test ....)    # Regular syntax
temp=$( test ... )   # Highlight sub-shell '( ... )' & command or function call
temp=`test .....`    # Backquotes not recommended to avoid complete highlighting

# Arrays
#--------
 # Initialisation
declare -a array                 # Explicit
array=([0]='zero' [1]='one' ...) # Implicit with index assignement
array[0]='zero'; array[9]='ten'  # Implicit or add element to array at an index
 # Curly brackets are essential to work with arrays
 # Last index of an array
IDX=${#temp_0123[@]}; idx=${#TEMP_0123[@]}
 # Get last element of an array (${#array[@]})
LAST_ELMNT=array[${#array[@]}]; last_elmnt=ARRAY[${#ARRAY[@]}]
 # Remove an element or entire array
unset array[9]; unset array[@]

# Strings
#---------
 # Single quotes are recommended to identified entire characters string instead
 # of doubles quotes or initialize variable
echo 'The name of the script is '$0' with following arguments '$*
 # Doubles quotes should only be used when it's necessary to interpret escaping
 # character or to perform parameter substitution
printf "The value of PI is %8.6f.\n" $PI; sed "s/3.1415/$PI/" temp.txt

# Function
#----------
 # 'function' word is not not mandatory at declaration if you have double
 # brackets '()' right after name. A function has to be declared before its call
 # so should be placed at the beginning of the main script. A clever solution is
 # to gather similar functions in kind of a 'module' file which will be sourced
 # from main script.
 # 2 possibles syntax:
function fake_func { local temp=''; ...; return ...; }
fake_func() {
    local temp='' # Declare variable as local, if not his attribute is global
    ...
    return ...   # Function can only return an integer (stderr by default)
                  # export result by a global variable to bypass it 
}
 # Function call (with or without argument)
temp=$( fake_func $1 $2 ); fake_func $1 $2; fake_func

# Tests operators differs with type of test (arithmetic or string comparison
# for number/characters, file attributs), see manual for test with `man test`
#----------------------------------------------------------------------------
 # Possible syntax : literal 'test' or compact syntax with brackets/parenthesis
 # at the ends
 # '[ ... ]' & '[[ ... ]]' are almost identical (simple and extended test)
 # With ' != ' & ' == ' operators, right string is considered as          regex
 #  ""             '~='     "    ,   "     ""   "      ""     "  extended   "
[ $temp ~= "..." ] && [[ ! -e temp.txt ]] || (( $temp >= 0 ))

# For compound commands, prefer the use of command block '{ ...; }' instead of
# a sub-shell '( ... )', keep in mind that despite sub-shell inherit from its
# run script all variables declared as locals are lost at the end of execution

# To cut a long sequence, put the escape character '\' at the end of line and
# continue on next line (possible on several lines)
printf "This is a very very very long sentence that I have to cut in order to  \
        be less than 80 characters for a line of code but I don't have to call \
        the same command several times.\n                                       "
# A pipe ' | ' cannot be put at the end of a line, even if you have a '\'
cat temp.txt | cut -d' ' -f-5 | sort -kr3n | uniq -c | sort | head -n25 \
| awk '$3 >= 1024 {print $4}'                                           \
| xargs -t -i() mv () $HOST@$HOSTNAME:${REP_STORAGE}

# 'if ...; then ...; fi'
#------------------------
 # Very short syntax with commands block '{ ...; }'
[ ... ] && { ...; ...; }
 #      Short syntax with commands block '{ ...; }'
[ ... ] && { ...; \
             ...;  }
 #    Regular syntax
if [ ... ]; then
    ...  
fi

# 'if ...; then ...;  else ...; fi'
#----------------------------------
 #   Short syntax with commands block '{ ... }'
{ [ ... ] && ...; } || { ...; ...; }
 # Regular syntax
if [ ... ]; then
    ...
else
    ...
fi

# 'case ... in ...) ... ;; ... esac'
#-----------------------------------
case ... in
 # Very short syntax
    ...) ...;;     ...) ...;;    ...) ...;;
 #      Short syntax
    ...) ...; ...; ...;;
 #    Regular syntax
    ...)
	...
	;;
esac

# List font lock faces with effective highlighting (can be customized)
#---------------------------------------------------------------------
 font-lock-warning-face
 # for a construct that is peculiar, or that greatly changes the meaning of
  #other text
 font-lock-function-name-face
 # for the name of a function being defined or declared
 font-lock-variable-name-face
 # for the name of a variable being defined or declared
 font-lock-keyword-face
 # for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
 font-lock-comment-face
 # for comments
 font-lock-comment-delimiter-face
 # for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals, this
  #inherits from font-lock-comment-face
 font-lock-type-face
 # for the names of user-defined data types
 font-lock-constant-face
 # for the names of constants, like ‘NULL’ in C
 font-lock-builtin-face
 # for the names of built-in functions
 font-lock-preprocessor-face
 # for preprocessor commands. This inherits, by default, from
  #font-lock-builtin-face
 font-lock-string-face
 # for string constants
 font-lock-doc-face
 # for documentation strings in the code. This inherits, by default, from
  #font-lock-string-face
 font-lock-negation-char-face
 # for easily-overlooked negation characters
