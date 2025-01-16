#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import subprocess
import re
from netCDF4 import Dataset

#
# BEGIN USER MODIFICATIONS
#

# Directory with domccfg target file
DOMCFG_DIR="../DOMAINcfg"
# Suffix of domcfg files
RAD='domain_cfg.nc'
# Directory with original forcing on native grid
FORCING_DIR='/Users/rblod/DATA/NEMO/NEMO_v4/ORCA2_ICE_v4.0/'
# Forcing file names, interpolation method (default bilin), and weigth file name (optional), lon(optional), lat(optional)  
FILES=[
['chlorophyll.nc'  ,'bilin','','',''],
['geothermal_heating.nc'  ,'bilin','','',''],
['eddy_viscosity_3D.nc'   ,'bilin','','',''],
['resto.nc'               ,'bilin','','',''],
['sss_data.nc'            ,'bilin','','',''],
['q_10.15JUNE2009_fill.nc','bilin','','',''],
['q_10.15JUNE2009_fill.nc','bicub','','','']
]

#
# END USER MODIFICATIONS
#
mycmd="ls "+DOMCFG_DIR+"/?_"+RAD
returned_output = subprocess.check_output(mycmd, shell=True)
listcfg = (returned_output.decode("utf-8")).split()



for i in range(len(listcfg)):
	print ('Computing weights for cfg file %s :' % listcfg[i])
	print()

	for myfile in FILES:
		print('Input file is %s with %s interpolation' % (myfile[0],  myfile[1]))
		if len(myfile[2]) == 0 :
			wfile=str(i+1)+'_'+myfile[1]+'_'+myfile[0]
		else:
			wfile=str(i+1)+'_'+myfile[2]
		print('   Performing weights computation ...')
		if myfile[1]=='namelist_bicub':
			namelist='namelist_bicub'
		else :
			namelist='namelist_bilin'

		myfilename=FORCING_DIR+'/'+myfile[0]
		dataset=Dataset(myfilename)
		
#		if len(myfile[3])==0: 
#			for key in dataset.variables:
#				if len(dataset.variables[key].dimensions) >=2:
#					myvar=key
#					break
#		else:
#			myvar=myfile[3]
#		print('   Interpolation based on variable %s ...' % myvar)		
		
		if len(myfile[3])==0 or len(myfile[4])==0: 
			for key in dataset.variables:
				if re.search('lat',key.lower()):
					mylat=key
				if re.search('lon',key.lower()):
					mylon=key
		else:
			mylon=myfile[3]
			mylat=myfile[4]
		print('   Interpolation based on longitude %s ...' % mylon)		
		print('   Interpolation based on latitude  %s ...' % mylat)		

		f2=open("namelist_new","w+")
		with open(namelist,"r") as f:
			for line in f:
				match = re.search('nemo_file',line)
				match2 = re.search('input_file',line)
				match3 = re.search('input_lon',line)
				match4 = re.search('input_lat',line)
				match5 = re.search('output_file',line)
				match6 = re.search('output_name',line)
				if match != None :
					line='nemo_file=''\''+listcfg[i]+'\''"\n"
				if match2 != None :
					line='input_file=''\''+myfilename+'\''"\n"
				if match3 != None :
					line='input_lon=''\''+mylon+'\''"\n"
				if match4 != None :
					line='input_lat=''\''+mylat+'\''"\n"
				if match5 != None :
					line='output_file=''\''+wfile+'\''"\n"
			#	if match5 != None :
		##			line='output_name=''\''+myvar+'\''"\n"
				f2.write(line)
			f.close()
			f2.close()
		mycheck=subprocess.check_output('./scripgrid.exe namelist_new',shell=True	)
		mycheck=subprocess.check_output('./scrip.exe namelist_new',shell=True	)
		mycheck=subprocess.check_output('./scripshape.exe namelist_new',shell=True	)
		print('   Success ...')
		print('   => weight file is %s' % wfile)

	print()


				

