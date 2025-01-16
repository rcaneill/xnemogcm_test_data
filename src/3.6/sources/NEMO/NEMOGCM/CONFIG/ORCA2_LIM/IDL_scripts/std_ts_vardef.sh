#!/bin/sh 
#
# AUTHOR - date
# ===========
# Sebastien Masson - 04/2011 - LOCEAN
#
# DESCRIPTION
# ===========
# define all varibles needed by std_ts.sh and std_ts.pro
#
# EXAMPLES
# ========
# $ . ./std_ts_vardef.sh
#
#
#===================== User PATHS =====================
#
idl_command=/Applications/itt/idl64/bin/idl
#
PS_DIR=$( pwd )/ts_p4H25a50-testht_ps
PDF_DIR=$( pwd )/ts_p4H25a50-testht_pdf
HTML_DIR=$( pwd )/html
SAXO_DIR=/Users/sflod/SAXO_DIR
#
DIR_DATA=/Users/sflod/idl_PLOTS/DATA_STORE/RUN_CLIMATO/lim3_ada     # path of data in NetCDF format
DIR_CLIMATO=/Users/sflod/idl_PLOTS/CLIMATOLOGIES    # path of climatological data
DIR_MASK=/Users/sflod/idl_PLOTS/MASK  # path of mask files (ex: subbasins)
#
#===================== Model GRID =====================
#
FILE_MESH_MASK=/Users/sflod/idl_PLOTS/MASK/ORL2PISV35_mesh_mask.nc # meshmask
FILE_MASK_SUBDOMAIN=subbasins_orca21_nored.nc              # sub-bassin masks
#
#===================== DATA =====================
#
VAR_TEMP_3D=votemper     ;   FILE_TEMP_3D=potT_annual_mean.nc                         # PHC3
VAR_SAL_3D=vosaline      ;   FILE_SAL_3D=Salt_1y_corr_PHC3WOA09.nc                    # PHC3
VAR_SST=sst              ;   FILE_SST=NewREY_ORCA2_1991_2000_1y.nc                   # Reynolds
VAR_FLUX=qnet            ;   FILE_FLUX=OAFlux_1my_01_12_1984_2004_orca2_qnet.nc      # flux
VAR_MLD=mld              ;   FILE_MLD=mld_DR003_c1m_ORCA2_1y.nc                      # Mixed layer depth
VAR_ICE_EXT_NH=extt_NH   ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Extent North Emisphere
VAR_ICE_EXT_SH=extt_SH   ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Extent South Emisphere
VAR_ICE_area_NH=area_NH  ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Area North Emisphere
VAR_ICE_area_SH=area_SH  ;   FILE_ICE=sea_ice_index_2000.nc                               # Ice Area South Emisphere
#
# Geothermal heating -> define FILE_GEOHEAT to 'NO' if there is not such forcing
#                    -> define VAR_GEOHEAT to a constant if geothermal heating is constant over the domain
VAR_GEOHEAT=heatflow   ;   FILE_GEOHEAT=geothermal_heating.nc 
#
#===================== EXP1 =====================
#
READ_ONLY_FIRST_RECORD=0   # if 0 then read all records in files else read only the first reacord in each file
#
DATE1=20010101  		  ;   DATE2=20501231
#
VAR1_T=thetao          ;   V1T_PREF=p4H25a50      ;   V1T_SUFF=_1Y_grid_T.nc
VAR1_S=so              ;   V1S_PREF=p4H25a50      ;   V1S_SUFF=_1Y_grid_T.nc
VAR1_SSH=zos         ;   V1SSH_PREF=p4H25a50    ;   V1SSH_SUFF=_1Y_grid_T.nc
##VAR1_Q=qt              ;   V1Q_PREF=p4H25a50      ;   V1Q_SUFF=_1Y_grid_T.nc
VAR1_EMP=wfo         ;   V1EMP_PREF=p4H25a50    ;   V1EMP_SUFF=_1Y_grid_T.nc
VAR1_U=uocetr_eff      ;   V1U_PREF=p4H25a50      ;   V1U_SUFF=_1Y_grid_U.nc
VAR1_V=vocetr_eff      ;   V1V_PREF=p4H25a50      ;   V1V_SUFF=_1Y_grid_V.nc
VAR1_ICE=siconc      ;   V1ICE_PREF=p4H25a50    ;   V1ICE_SUFF=_1M_icemod.nc
VAR1_Ithick=sithic    ;   V1It_PREF=p4H25a50     ;   V1It_SUFF=_1M_icemod.nc
VAR1_SNOW=snthic    ;   V1SNOW_PREF=p4H25a50   ;   V1SNOW_SUFF=_1M_icemod.nc
VAR1_IvelV=sivelv    ;   V1IvV_PREF=p4H25a50    ;   V1IvV_SUFF=_1M_icemod.nc 
VAR1_Ivel=sivelo      ;   V1Iv_PREF=p4H25a50     ;   V1Iv_SUFF=_1M_icemod.nc
#
#===================== EXP2 =====================
#
DATE1_2=20010101   	  ;   DATE2_2=20501231
#
VAR2_T=thetao     	  ;   V2T_PREF=testht     ;   V2T_SUFF=_1Y_grid_T.nc
VAR2_S=so        	  ;   V2S_PREF=testht     ;   V2S_SUFF=_1Y_grid_T.nc
VAR2_SSH=zos      	;   V2SSH_PREF=testht   ;   V2SSH_SUFF=_1Y_grid_T.nc
##VAR2_Q=qt       	  ;   V2Q_PREF=testht     ;   V2Q_SUFF=_1Y_grid_T.nc
VAR2_EMP=wfo            ;   V2EMP_PREF=testht   ;   V2EMP_SUFF=_1Y_grid_T.nc
VAR2_U=uocetr_eff   	  ;   V2U_PREF=testht     ;   V2U_SUFF=_1Y_grid_U.nc
VAR2_V=vocetr_eff   	  ;   V2V_PREF=testht     ;   V2V_SUFF=_1Y_grid_V.nc
VAR2_ICE=siconc         ;   V2ICE_PREF=testht   ;   V2ICE_SUFF=_1M_icemod.nc
VAR2_Ithick=sithic       ;   V2It_PREF=testht    ;   V2It_SUFF=_1M_icemod.nc
VAR2_SNOW=snthic       ;   V2SNOW_PREF=testht  ;   V2SNOW_SUFF=_1M_icemod.nc
VAR2_IvelV=sivelv       ;   V2IvV_PREF=testht   ;   V2IvV_SUFF=_1M_icemod.nc 
VAR2_Ivel=sivelo        ;   V2IvV_PREF=testht   ;   V2IvV_SUFF=_1M_icemod.nc 
#
######################### Export Variables ###############################
#
#===================== User PATHS =====================
export PS_DIR PDF_DIR HTML_DIR SAXO_DIR
export DIR_DATA DIR_CLIMATO DIR_MASK
#===================== Model GRID =====================
export FILE_MESH_MASK FILE_MASK_SUBDOMAIN
#===================== DATA =====================
export FILE_TEMP_3D  VAR_TEMP_3D
export FILE_SAL_3D   VAR_SAL_3D
export FILE_SST      VAR_SST
export FILE_GEOHEAT  VAR_GEOHEAT
export FILE_FLUX     VAR_FLUX
export FILE_MLD      VAR_MLD
export FILE_ICE      VAR_ICE_EXT_NH  VAR_ICE_EXT_SH VAR_ICE_area_NH  VAR_ICE_area_SH
export FILE_SNOW     VAR_SNOW_NH     VAR_SNOW_SH    VAR_SNOW_area_NH VAR_SNOW_area_SH
#===================== EXP1 =====================
export READ_ONLY_FIRST_RECORD
#
export DATE1        DATE2
export VAR1_T       V1T_PREF     V1T_SUFF
export VAR1_S       V1S_PREF     V1S_SUFF
export VAR1_SSH     V1SSH_PREF   V1SSH_SUFF
export VAR1_Q       V1Q_PREF     V1Q_SUFF
export VAR1_EMP     V1EMP_PREF   V1EMP_SUFF
export VAR1_U       V1U_PREF     V1U_SUFF
export VAR1_V       V1V_PREF     V1V_SUFF
export VAR1_ICE     V1ICE_PREF   V1ICE_SUFF
export VAR1_Ithick  V1It_PREF    V1It_SUFF
export VAR1_SNOW    V1SNOW_PREF  V1SNOW_SUFF
export VAR1_IvelV   V1IvV_PREF   V1IvV_SUFF
export VAR1_Ivel    V1Iv_PREF    V1Iv_SUFF

#===================== EXP2 =====================
export DATE1_2      DATE2_2
export VAR2_T       V2T_PREF     V2T_SUFF
export VAR2_S       V2S_PREF     V2S_SUFF
export VAR2_SSH     V2SSH_PREF   V2SSH_SUFF
export VAR2_Q       V2Q_PREF     V2Q_SUFF
export VAR2_EMP     V2EMP_PREF   V2EMP_SUFF
export VAR2_U       V2U_PREF     V2U_SUFF
export VAR2_V       V2V_PREF     V2V_SUFF
export VAR2_ICE     V2ICE_PREF   V2ICE_SUFF
export VAR2_Ithick  V2It_PREF    V2It_SUFF
export VAR2_SNOW    V2SNOW_PREF  V2SNOW_SUFF
export VAR2_IvelV   V2IvV_PREF   V2IvV_SUFF
export VAR2_Ivel    V2Iv_PREF    V2Iv_SUFF
#
