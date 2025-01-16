import glob
import sys
import subprocess
import os
import copy

def OSinfo(runthis):
	red = lambda text: '\033[0;31m' + text + '\033[0m'
	osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
	theInfo = osstdout.communicate()[0].strip()
	if osstdout.returncode!=0:
		print(red(runthis+" FAILED"))
		print >> sys.stderr, osstdout.returncode
		sys.exit()
	# else:
	# 	print(runthis+" OK")

def Sort(sub_li): 
    l = len(sub_li) 
    for i in range(0, l): 
        for j in range(0, l-i-1): 
            if (sub_li[j][0] < sub_li[j+1][0]): 
                tempo = sub_li[j] 
                sub_li[j]= sub_li[j+1] 
                sub_li[j+1]= tempo
            if (sub_li[j][0] == sub_li[j+1][0]):
                if (sub_li[j][3] < sub_li[j+1][3]): 
                    tempo = sub_li[j] 
                    sub_li[j]= sub_li[j+1] 
                    sub_li[j+1]= tempo 
                if (sub_li[j][3] == sub_li[j+1][3]): 
                    if (sub_li[j][2] < sub_li[j+1][2]): 
                        tempo = sub_li[j] 
                        sub_li[j]= sub_li[j+1] 
                        sub_li[j+1]= tempo 
    return sub_li 

def subgenerate(maindir, machine_name):

    compile_list = glob.glob(maindir+"/build_"+machine_name+"/build_*.txt")
    print(maindir)
    print(compile_list)
    if len(compile_list) == 0 :
        return
    machine = machine_name
    revision_list=[]
    machine_list=[]
    arch_list=[]
    mode_list=[]

    bg_color = ["#84c5ff", "#96cdff", "#a1d3ff", "#b3dafd", "#c2e1fd"]


    myBuild=["", "", "", "", ""] #revision, machine, arch, mode, status
    myBuildList=[]
    
    for compile_log in compile_list:
        revision = compile_log[6:10]
        myBuild[4] = "&#10060;"  
        f=open(compile_log, "r")
        for line in f:
            if line.startswith("revision"):
                myText = line.replace("revision ", "").replace("\n", "")
                if not myText in revision_list:
                    revision_list.append(myText)
                myBuild[0] = myText
                

            elif line.startswith("machine"):
                myText = line.replace("machine ", "").replace("\n", "")
                if not myText in machine_list:
                    machine_list.append(myText)
                myBuild[1] = myText

            elif line.startswith("arch"):
                myText = line.replace("arch ", "").replace("\n", "")
                if not myText in arch_list:
                    arch_list.append(myText)
                myBuild[2] = myText

            elif line.startswith("mode"):
                myText = line.replace("mode ", "").replace("\n", "")
                if not myText in mode_list:
                    mode_list.append(myText)
                myBuild[3] = myText

            elif line[0].isdigit():
                myTexts = line.replace("\n", "")
                if myTexts == "0" :
                    myBuild[4] = "&#9989;" 
        myBuildList.append([myBuild[0], myBuild[1], myBuild[2], myBuild[3], myBuild[4]])
        f.close()

    print(Sort(myBuildList)) # first by revision number, then by mode, then by arch. all in descendant order
    print(len(myBuildList))
    
    revision_list = sorted(revision_list, reverse = True)
    print(arch_list) # row
    print(mode_list) # col
    print(machine_list) # col
    print(revision_list) # col


    f=open("compile_"+machine+"_info.js", "w")
    f.write("var "+machine+"_compile_info_list = "+repr(myBuildList)+"\n\n")
    f.write("var "+machine+"_revision_list = "+repr(revision_list)+"\n\n")
    f.close()


def main() :
    repository_name=os.getenv('xios_test_suite_repository')
    machine_name=os.getenv('xios_machine_name')
    subgenerate(repository_name+'/BUILD',machine_name)

if __name__== "__main__":
  main()
