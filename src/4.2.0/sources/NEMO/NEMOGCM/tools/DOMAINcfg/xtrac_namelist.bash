#!/bin/bash
#####################################################################
#
# history: JM Molines (MEOM/IGE/DRAKKAR)    XX/2019: DRAKKAR version
#          P  Mathiot (CryoDyn/IGE/DRAKKAR) 07/2020: added in NEMO
#
# purpose: re-generate the namelist used to build a domain_cfg.nc file
#          assuming namelist_cfg variable has been added into via
#          the dom_doc.exe tools
#
#####################################################################
#
# function to extract namelist
xtrac_namelist()   {
   domain_cfg=$1
   namelist_cfg=$2
   #ncdump -v namelist_cfg $domain_cfg | tr -d '"'  \
   #                                   | tr -d '\\' \
   #                                   | sed -e 's/ *,$//'  \
   #                                         -e 's/;$//' \
   #                                         -e '1,/namelist_cfg =/d' \
   #                                         -e '$d' -e 's/^..//'
   ncdump -v namelist_cfg $domain_cfg \
                                      | tr -d '\\' \
                                      | sed -e 's/^  "//'   \
                                            -e 's/ *",$//'  \
                                            -e 's/;$//' \
                                            -e '1,/namelist_cfg =/d' \
                                            -e '$d'
   }
#
# main script
#
if [ $# != 2 ]; then echo 'usage xtract_namelist.bash [domain_cfg.nc file name] [namelist_output name]'; exit 42; fi

xtrac_namelist $1 > $2

exit $?
