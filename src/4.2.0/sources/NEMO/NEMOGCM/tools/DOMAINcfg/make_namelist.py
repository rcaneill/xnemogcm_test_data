#!/usr/bin/env python
# coding: utf-8
import argparse
import sys

if len(sys.argv) == 1 :
   namfile='namelist_cfg'
   topofile='GEBCO_2020.nc' 
elif len(sys.argv) == 2 :
   namfile=sys.argv[1]
   topofile='GEBCO_2020.nc' 
elif len(sys.argv) == 3 :
   namfile=sys.argv[1]
   topofile=sys.argv[2] 
else:
   print ("Usage : make_namelist.py namelist_cfg topofile")
   sys.exit (1)


Agrilefile="AGRIF_FixedGrids.in"
grid=[]
nbghostcells  = 4 

with open(Agrilefile) as fp:
   line = fp.readline()
   cnt = 1
   while line:
#       print("Line {}: {}".format(cnt, line.strip()))
     #  print (line.strip(' '))
       if len(line.split()) >=4 :
       	   #print(line.split())
       	   grid.append(line.split()[0:6])
           #for word in line.split() :
               #print(word)
       line = fp.readline()
       cnt += 1

print("Found", len(grid), "grids", ":")

f1="namelist_cfg"



cnt = 1
for g in range(len(grid)) :
    
    f2 = open(str(cnt)+'_'+f1,'w')
    with open(f1) as fp:
        line = fp.readline()
        cnt1 = 1
        while line :
           line = fp.readline()
           if line.strip().startswith('Ni0glo'):
               Ni0glo_parent=line.strip().split()[2]               
           f2.write(line)
           if line.strip().startswith('Nj0glo'):
               Nj0glo_parent=line.strip().split()[2]               
           f2.write(line)
           cnt1 += 1
    f2.close()

    print(int(Ni0glo_parent), int(Nj0glo_parent))

    nbghostcells_x_e = nbghostcells
    nbghostcells_x_w = nbghostcells
    nbghostcells_y_n = nbghostcells
    nbghostcells_y_s = nbghostcells
    if (int(grid[cnt-1][2]) == 1 ):
        nbghostcells_y_s = 1 
    if int(grid[cnt-1][3]) == int(Nj0glo_parent)-1 :
        nbghostcells_y_n = 1 
    if (int(grid[cnt-1][0]) == 1 ):
        nbghostcells_x_w = 1 
    if int(grid[cnt-1][1]) == int(Ni0glo_parent)-1 :
        nbghostcells_x_e = 1 
    if int(grid[cnt-1][3]) == int(Nj0glo_parent) :
        nbghostcells_y_n = 0 
    if int(grid[cnt-1][1]) - int(grid[cnt-1][0]) == int(Ni0glo_parent) :
        nbghostcells_x_w = 0
        nbghostcells_x_e = 0

    Ni0glo = (int(grid[cnt-1][1])-int(grid[cnt-1][0]))*int(grid[cnt-1][4]) + nbghostcells_x_w  + nbghostcells_x_e
    Nj0glo = (int(grid[cnt-1][3])-int(grid[cnt-1][2]))*int(grid[cnt-1][5]) + nbghostcells_y_n  + nbghostcells_y_s
    #print( "Grid "+str(cnt)+" : jpiglo = "+cnt(jpiglo)+ "  jpjglo = "+str(jpjglo) ) 
    print(int(grid[cnt-1][0]), int(grid[cnt-1][1]), int(grid[cnt-1][2]),int(grid[cnt-1][3]))
    print(nbghostcells_x_w, nbghostcells_x_e,  nbghostcells_y_s, nbghostcells_y_n)
    print('Grid {:1d} : Ni0glo = {:3d} , Nj0glo = {:3d}'.format(cnt, Ni0glo, Nj0glo))

    f2 = open(str(cnt)+'_'+namfile,'w')
    with open(namfile) as fp:
        line = fp.readline()
        cnt1 = 1
        while line :
           line = fp.readline()
           if line.strip().startswith('jperio'):
               if int(grid[cnt-1][1]) - int(grid[cnt-1][0]) == int(Ni0glo_parent):
                   line = "   jperio = 1\n"
               else:
                   line = "   jperio = 0\n"
               if nbghostcells_y_n == 0:
                   line = "   jperio = 4\n"
           if line.strip().startswith('nn_bathy'):
            	   line = "   nn_bathy = 2\n"
           if line.strip().startswith('nn_interp'):
           	   line = "   nn_interp = 1\n"
           if line.strip().startswith('cn_topo'):
           	   line = "   cn_topo = '"+topofile+"'\n"
           if line.strip().startswith('cn_bath'):
           	   line = "   cn_bath = 'elevation'\n"
           if line.strip().startswith('cn_lon'):
           	   line = "   cn_lon = 'lon'\n"
           if line.strip().startswith('cn_lat'):
           	   line = "   cn_lat = 'lat'\n"
           if line.strip().startswith('rn_scale'):
           	   line = "   rn_scale = -1\n"
           if line.strip().startswith('Ni0glo'):
           	   line = "   Ni0glo = "+str(Ni0glo)+"\n"
           if line.strip().startswith('Nj0glo'):
           	   line = "   Nj0glo = "+str(Nj0glo)+"\n"
           if line.strip().startswith('jpidta'):
           	   line = "   jpidta = "+str(Ni0glo)+"\n"
           if line.strip().startswith('jpjdta'):
           	   line = "   jpjdta = "+str(Nj0glo)+"\n"
           if line.strip().startswith('cp_cfg'):
           	   line = "   cp_cfg = 'dumb'\n"     
           if line.strip().startswith('ln_read_cfg'):
               line = "   ln_read_cfg = .false.\n"              
           f2.write(line)
           cnt1 += 1
    f2.close()     
    cnt +=1         	
