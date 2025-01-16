import glob
import sys
import subprocess
import os

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
                if (sub_li[j][2] < sub_li[j+1][2]): 
                    tempo = sub_li[j] 
                    sub_li[j]= sub_li[j+1] 
                    sub_li[j+1]= tempo 
                if (sub_li[j][1] == sub_li[j+1][1]): 
                    if (sub_li[j][2] < sub_li[j+1][2]): 
                        tempo = sub_li[j] 
                        sub_li[j]= sub_li[j+1] 
                        sub_li[j+1]= tempo 
    return sub_li 

def subgenerate(directory, machine, machine_name):
   
    test_list = glob.glob(directory+"/test_"+machine+"/test_*.txt")
    if len(test_list) == 0 :
        return
    
    revision_list=[]
    relurl_list=[]
    machine_list=[]
    build_dir_list=[]
    arch_list=[]
    mode_list=[]
    
    myTestList=[]    
        
    for test_log in test_list :
        f=open(test_log, "r")
        for line in f :
            if line.startswith("#revision") :
                revision = line.replace("\n","").split(" ")[1]
                if not revision in revision_list :
                    revision_list.append(revision)
            elif line.startswith("#relurl") :
                relurl = line.replace("\n","").split(" ")[1]
                tmp_url = relurl.split("/")
                branch = tmp_url[len(tmp_url)-1]
                if not relurl in relurl_list :
                    relurl_list.append(relurl)
            elif line.startswith("#build_dir") :
                build_dir = line.replace("\n","").split(" ")[1]
                tmp_list = build_dir.split("/")
                short_dir = tmp_list[len(tmp_list)-1]
                if not build_dir in build_dir_list :
                    build_dir_list.append(short_dir)
            elif line.startswith("#arch") :
                arch = line.replace("\n","").split(" ")[1]
                if not arch in arch_list :
                    arch_list.append(arch)
            elif line.startswith("#mode") :
                mode = line.replace("\n","").split(" ")[1]
                if not mode in mode_list :
                    mode_list.append(mode)
        
        myTestList.append([revision, machine, short_dir, branch, machine_name, arch, mode, build_dir])
        f.close()
        
    revision_list = sorted(revision_list, reverse=True)
    print(revision_list)
    
    Sort(myTestList)
    print(myTestList)
  
    f=open("_test_"+machine+"_info.js", "w")
    f.write("var test_"+machine+"_revision_list = "+repr(revision_list)+"\n\n")
    f.write("var test_"+machine+"_info_list = [\n")
    for i in range(len(myTestList)) :
        if i<len(myTestList)-1 :
            f.write("        "+repr(myTestList[i])+",\n")
        else :
            f.write("        "+repr(myTestList[i])+"]\n\n")
            
    myReportDict=dict()
    for i in range(len(myTestList)) :
        myReportList=[] # algo, config, file, status
        print(myTestList[i])
        file_to_open = directory+"/test_"+machine+"/test_"+myTestList[i][0]+"_"+myTestList[i][1]+"_"+myTestList[i][2].replace("build_","")+".txt"
        g=open(file_to_open, "r")
        for line in g :
            if not line.startswith("#") :
                line = line.replace("\n","").split(" ")
                algo = line[0]
                config = line[1].split("@")[1]
                file = line[2].split("@")[2]
                status = line[3]
                myReportList.append([algo, config, file, status])
        g.close()
        print(len(myReportList))
        myReportDict.update({myTestList[i][0]+"_"+myTestList[i][2].replace("build_","") : myReportList})
    
    for i in range(len(myReportDict)) :
        key = list(myReportDict.keys())[i]
        f.write("var test_"+machine+"_"+key+" = [\n")
        for j in range(len(myReportDict[key])) :
            if j<len(myReportDict[key])-1 : 
                f.write("        [\'"+myReportDict[key][j][0]+"\', \'"+myReportDict[key][j][1]+"\', \'"+myReportDict[key][j][2]+"\', "+myReportDict[key][j][3]+"],\n")
            else :
                f.write("        [\'"+myReportDict[key][j][0]+"\', \'"+myReportDict[key][j][1]+"\', \'"+myReportDict[key][j][2]+"\', "+myReportDict[key][j][3]+"]")
        f.write("]\n\n\n")

    for revision in revision_list :
        algo_list = glob.glob(directory+"/def_files/"+revision+"/test_*")
    
        for l in algo_list :
            tmp_algo = l.split("/")
            f.write("var test_"+machine+"_"+revision+"_"+tmp_algo[len(tmp_algo)-1]+"_user_params = [\n")
            if os.path.exists(l+"/user_param.json"):
                g=open(l+"/user_param.json","r")
                for line in g:
                    if not line=="\n" and not line.startswith("#"):
                        f.write("        \'"+line.replace("\n","").replace("'","\\\'")+"\',\n")
                g.close()
            else :
                g=open(l+"/user_params.def","r")
                for line in g:
                    if not line=="\n" and not line.startswith("#"):
                        f.write("        \'"+line.replace("\n","").replace("'","\\\'")+"\',\n")
                g.close()
            f.write("        ]\n\n")

    for revision in revision_list :
        algo_list = glob.glob(directory+"/def_files/"+revision+"/test_*")
    
        for algo in algo_list :
            config_list = glob.glob(algo+"/config_*")
            tmp_algo = algo.split("/") 
            algo_name = tmp_algo[len(tmp_algo)-1]
            for config in config_list :
                tmp_config = config.split("/")
                config_name = tmp_config[len(tmp_config)-1]
                config_name_bis = config_name.replace("=","_")
                f.write("var test_"+machine+"_"+revision+"_"+algo_name+"_"+config_name_bis+"_all_params = [\n")
                g=open(config+"/all_param.def","r")
                for line in g:
                    line = line.replace("&", "& ")
                    if not line=="\n" :
                        f.write("        \'"+line.replace("\n","").replace("'","\\\'")+"\',\n")
                g.close()
                f.write("        ]\n\n")

    f.close()

    
def main():
    repository_name=os.getenv('xios_test_suite_repository')
    machine=os.getenv('xios_machine_name')
    machine_name=os.getenv('xios_full_machine_name')
    
    subgenerate(repository_name+'/RUN',machine, machine_name)


if __name__== "__main__":
  main()
