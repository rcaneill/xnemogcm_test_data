#!/bin/bash

./inc/clean.sh
./inc/build.sh

cd tex_main
sed -i -e 's#\\documentclass#%\\documentclass#' -e '/{document}/ s/^/%/' \
	../tex_sub/*.tex
sed -i    's#\\subfile{#\\include{#g' \
	NEMO_manual.tex
latex2html -local_icons -no_footnode -split 4 -link 2 -mkdir -dir ../html_LaTeX2HTML 	\
				$* 																								\
	NEMO_manual
sed -i -e 's#%\\documentclass#\\documentclass#' -e '/{document}/ s/^%//' \
	../tex_sub/*.tex
sed -i    's#\\include{#\\subfile{#g' \
	NEMO_manual.tex
cd -

exit 0
