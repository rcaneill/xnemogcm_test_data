import glob
import sys
import subprocess
import os
import json
import itertools
import copy

import netCDF4
from netCDF4 import Dataset
import numpy as np

mode=os.getenv("mode")
arch=os.getenv("arch")
svnr=os.getenv("svnR")
ref_location=os.getenv("ref_location")
ref_file=os.getenv("ref_file")



def OSinfo(runthis):
    red = lambda text: '\033[0;31m' + text + '\033[0m'
    osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True)
    theInfo, theErr = osstdout.communicate()
    if theErr:
        print(red(runthis+" FAILED"))
        print(theErr)
        sys.exit()



def nonblank_lines(f):
    for l in f:
        line = l.rstrip()
        if line and not line.startswith("#"):
            yield line

def main():
    ref_list = glob.glob(ref_location+"/*")
    for i in range(len(ref_list)):
        tmp = ref_list[i].split("/")
        rev = tmp[len(tmp)-1]
        ref_list[i] = int(rev)
    ref_list.sort(reverse=True) #ref_list in descending order
    
    ref_rev = ""
    for ref in ref_list:
        if int(svnr) >= ref :
            ref_rev = str(ref)
            print("corresponding reference = ", ref)
            break
        
    if not ref_rev:
        print("no available reference found ... exit")
        return
    
    OSinfo("cp "+ref_location+"/"+ref_rev+"/"+ref_file+" ./")
    OSinfo("tar -zxvf "+ref_location+"/"+ref_rev+"/"+ref_file)
    OSinfo("rm -f "+ref_file)
    
    
    test_folder_list = glob.glob('test_*')

    for test_folder in test_folder_list:
        config_list = glob.glob(test_folder+"/CONFIG_*")
        
        
        with open(test_folder+"/checkfile.def", "r") as fh:
            checkfiles = list(nonblank_lines(fh))

        with open("report_"+svnr+"_"+arch+"_"+mode+".txt", "a") as report:
            for config in config_list:
                folder_name = list(config.split("/"))[0]
                config_name = list(config.split("/"))[1]
                for checkfile in checkfiles:
                    if os.path.exists(config+"/"+checkfile) and os.path.exists("reference/ref_"+config+"/"+checkfile):
                        #OSinfo("cdo -W diffn "+config+"/"+checkfile+" "+"reference/ref_"+config+"/"+checkfile+"  2>&1 |grep -v 'Found more than one time variable'|grep -v 'cdo diffn: Processed'|grep -v 'cdo    diffn: Processed'|grep -v 'Time variable >time_counter< not found!' > diff_"+checkfile+".txt")
                        #if os.stat("diff_"+checkfile+".txt").st_size==0: # if no diff -> set 0
                        #    report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(1)+"\n")
                        #else: # if cdo diffn returns diff -> set -1
                        #    report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(-1)+"\n")
                        ref = Dataset( "reference/ref_"+config+"/"+checkfile )
                        res = Dataset( config+"/"+checkfile )
                        validated = 1
                        for var in res.variables:
                            if (not (var.startswith('lon_'))) and (not (var.startswith('lat_'))) and (not (var.startswith('time_'))) and (not (var.startswith('atm__'))):
                                ref_interp = ref.variables[var]
                                ref_array = ref_interp[:]
                                res_interp = res.variables[var]
                                res_array = res_interp[:]
                                if (res_array.shape == ref_array.shape):
                                    diff = np.zeros_like( ref_array )
                                    np.divide(ref_array-res_array,ref_array,diff,where=(ref_array[:]>10**-15))
                                    if ( np.max(np.abs(diff)) >  1*10**-9 ):
                                        validated = -1
                                    diff = np.zeros_like( ref_array )
                                    np.divide(ref_array-res_array,res_array,diff,where=(ref_array[:]>10**-15))
                                    if ( np.max(np.abs(diff)) >  1*10**-9 ):
                                        validated = -1
                                else:
                                        validated = -1
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(validated)+"\n")

                    elif os.path.exists(config+"/"+checkfile): # if no ref file -> set 0
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(0)+"\n")
                    elif os.path.exists("reference/ref_"+config+"/"+checkfile): # if no output file -> set -2
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(-2)+"\n")
                    else :
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(-666)+"\n")
                   

if __name__== "__main__":
  main()
