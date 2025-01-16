#! /bin/bash

mkdir -p containers

for i in $(ls bld/containers/);
do
    echo $i
    cp bld/containers/$i/apptainer-nemo.sif containers/apptainer-nemo-$i.sif
    apptainer sign containers/apptainer-nemo-$i.sif
done
