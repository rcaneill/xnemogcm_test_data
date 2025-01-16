#!/bin/sh

export opts='-shell-escape -pdf'
model='NEMO'

check_python_module() {
    python2 -c "
import sys
try:
    import $1
    print('\nModule $1 is installed')
except ImportError:
    print('\nModule $1 is NOT installed')
    print('')
    sys.exit(42)"
}

clean() {
    ## Delete latex build files
    find latex -regextype posix-extended                                              \
         -regex ".*\.(aux|bbl|blg|dvi|fdb|fls|idx|ilg|ind|log|maf|mtc|out|pdf|toc).*" \
         -exec rm {} \;

    ## Remove 'minted' directories
    find latex -type d -name '_minted*' -exec rm -r {} \;

    ## HTML exports
    find latex -type d -name 'html*'    -exec rm -r {} \;
}

build() {
    cd latex/$1/main
    latexmk $opts $1'_manual' > /dev/null
    mv            $1'_manual'.pdf ../../..
    cd -
}

check_python_module pygments
if [ $? -ne 0 ]; then echo 'Required python module pygments to correctly build the documentation is missing; exit 42'; echo ''; exit 42; fi

clean

[ ! -d figures ] && svn co http://forge.ipsl.jussieu.fr/nemo/svn/utils/figures@10471

build $model

exit 0
