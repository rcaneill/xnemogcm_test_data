#!/bin/bash

REF=reference
REF_PREFIX=ref_
W_DIR=RUN_TEST_SUITE
NC_FILE=*.nc
REF_TAR=reference.tar.gz

mkdir -p ${REF}

for test in $(cd ${W_DIR} ; ls -d test_*/)
do
  mkdir -p ${REF}/${REF_PREFIX}${test%%}
  for test_config in $(cd ${W_DIR} ; ls -d ${test%%/}/CONFIG_*)
  do
    mkdir -p ${REF}/${REF_PREFIX}${test_config%%}
    cp ${W_DIR}/${test_config%%}/${NC_FILE} ${REF}/${REF_PREFIX}${test_config%%}
  done
done

tar -zcvf ${REF_TAR} ${REF}
rm -rf ${REF}
