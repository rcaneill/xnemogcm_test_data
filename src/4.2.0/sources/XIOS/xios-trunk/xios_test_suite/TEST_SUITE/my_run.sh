#!/bin/bash 

echo "running my_run.sh"

export svnR=$(svn info --show-item revision ../../)
svnurl=$(svn info --show-item url ../../)
PWD=$(pwd)

fn=report_${svnR}_${arch}_${mode}.txt
echo "#revision" ${svnR} > ${fn}
echo "#url" ${svnurl} >> ${fn}
echo "#machine" ${xios_machine_name} >> ${fn}
echo "#build_dir" $(pwd)/build_${arch}_${mode} >> ${fn}
echo "#arch" $arch >> ${fn}
echo "#mode" $mode >> ${fn}


${PYTHON} step1.py


if [[ ${xios_machine_name} == "irene" ]]; then
  cmd=$(ccc_msub full_job_${arch}_${mode}.sh)
  jobid="${cmd//[!0-9]/}"

  i=0
  output=$(ccc_mpp | grep ${jobid})
  while [ ! -z "$output" ]
  do
    echo "job" $jobid "pending/running for about" ${i} seconds
    sleep 30
    ((i+=30))
    output=$(ccc_mpp | grep ${jobid})
  done
fi



if [[ ${xios_machine_name} == "jeanzay" ]]; then
  cmd=$(sbatch full_job_${arch}_${mode}.sh)
  jobid="${cmd//[!0-9]/}"
  i=0
  output=$(squeue | grep ${jobid})
  while [ ! -z "$output" ]
  do
    echo "job" $jobid "pending/running for about" ${i} seconds
    sleep 30
    ((i+=30))
    output=$(squeue | grep ${jobid})
  done
fi


${PYTHON} step2.py
