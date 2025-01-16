MODULE par_abl
   !!======================================================================
   !!                        ***  par_abl  ***
   !! ABL :   set the Atmospheric Boundary Layer parameters
   !!======================================================================
   !! History :  4.0  !  2019-03  (F. Lemarié & G. Samson)  Original code
   !!----------------------------------------------------------------------
   USE par_kind          ! kind parameters
   USE par_oce           ! to access horizontal domain size & mpp decomposition

   IMPLICIT NONE
   PUBLIC

   !!---------------------------------------------------------------------
   !! Active tracer parameters
   !!---------------------------------------------------------------------
   INTEGER , PUBLIC, PARAMETER ::   jptq   = 2     !: Number of active tracers (=2, i.e. T & q )
   INTEGER , PUBLIC, PARAMETER ::   jp_ta  = 1     !: indice for temperature
   INTEGER , PUBLIC, PARAMETER ::   jp_qa  = 2     !: indice for humidity
   INTEGER , PUBLIC, PARAMETER ::   jptime = 2     !: number of time indices stored in memory

   !!---------------------------------------------------------------------
   !! namABL Namelist options  
   !!---------------------------------------------------------------------   
   INTEGER , PUBLIC            ::   nn_amxl        !: mixing length option 
   INTEGER , PUBLIC            ::   nn_dyn_restore !: restoring option for dynamical ABL variables
   LOGICAL , PUBLIC            ::   ln_geos_winds  !: large-scale restoring of ABL winds toward geostrophic winds
   LOGICAL , PUBLIC            ::   ln_hpgls_frc   !: forcing of ABL winds by large-scale pressure gradient 
   LOGICAL , PUBLIC            ::   ln_smth_pblh   !: smoothing of atmospheric PBL height 
   !LOGICAL , PUBLIC            ::   ln_topbc_neumann = .FALSE.  !: idealised testcases only

   LOGICAL           , PUBLIC  ::   ln_rstart_abl    !: (de)activate abl restart
   CHARACTER(len=256), PUBLIC  ::   cn_ablrst_in     !: suffix of abl restart name (input)
   CHARACTER(len=256), PUBLIC  ::   cn_ablrst_out    !: suffix of abl restart name (output)
   CHARACTER(len=256), PUBLIC  ::   cn_ablrst_indir  !: abl restart input directory
   CHARACTER(len=256), PUBLIC  ::   cn_ablrst_outdir !: abl restart output directory

   !!---------------------------------------------------------------------
   !! ABL parameters for TKE turbulent closure
   !!---------------------------------------------------------------------
   REAL(wp), PUBLIC, PARAMETER ::   tke_min   = 1.e-6_wp          !: minimum TKE
   REAL(wp), PUBLIC, PARAMETER ::   avm_bak   = 1.e-4_wp          !: background viscosity
   REAL(wp), PUBLIC, PARAMETER ::   avt_bak   = 1.e-5_wp          !: background diffusion
   REAL(wp), PUBLIC, PARAMETER ::   itvref    = 1.0_wp / 288.0_wp !: inverse of reference virtual temperature     
   !++ TKE closure parameters
   REAL(wp), PUBLIC, PARAMETER ::   rn_phimax = (1._wp - 2.2_wp) / 2.2_wp !: maximum value for Ri * mxl^2 * N^2 / tke in phiz computation
   REAL(wp), PUBLIC, PARAMETER ::   rn_Cek    = 258._wp                   !: Ekman constant for Richardson number 
   REAL(wp), PUBLIC, PARAMETER ::   rn_epssfc = 1._wp / ( 1._wp + 2.8_wp * 2.8_wp )
   REAL(wp), PUBLIC            ::   rn_Ceps                       !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_Cm                         !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_Ct                         !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_Ce                         !: namelist parameter 
   REAL(wp), PUBLIC            ::   rn_Rod                        !: namelist parameter   
   REAL(wp), PUBLIC            ::   rn_Sch    
   REAL(wp), PUBLIC            ::   rn_Esfc
   REAL(wp), PUBLIC            ::   rn_Lsfc
   REAL(wp), PUBLIC            ::   mxl_min    
   REAL(wp), PUBLIC            ::   rn_ldyn_min                   !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_ldyn_max                   !: namelist parameter  
   REAL(wp), PUBLIC            ::   rn_ltra_min                   !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_ltra_max                   !: namelist parameter
   REAL(wp), PUBLIC            ::   rn_Ric                        !: critical Richardson number 
   REAL(wp), PUBLIC            ::   rn_vfac                       !: multiplicative factor for ocean/ice velocity

   !!---------------------------------------------------------------------
   !! ABL parameters for the vertical profile of the restoring term 
   !!---------------------------------------------------------------------
   REAL(wp), PUBLIC, PARAMETER ::   jp_bmin    =   0.5_wp 
   REAL(wp), PUBLIC, PARAMETER ::   jp_bmax    =   1.5_wp
   REAL(wp), PUBLIC            ::   jp_alp0_tra
   REAL(wp), PUBLIC            ::   jp_alp1_tra   
   REAL(wp), PUBLIC            ::   jp_alp2_tra   
   REAL(wp), PUBLIC            ::   jp_alp3_tra   
   REAL(wp), PUBLIC            ::   jp_alp0_dyn
   REAL(wp), PUBLIC            ::   jp_alp1_dyn   
   REAL(wp), PUBLIC            ::   jp_alp2_dyn   
   REAL(wp), PUBLIC            ::   jp_alp3_dyn  
   REAL(wp), PUBLIC            ::   jp_pblh_min
   REAL(wp), PUBLIC            ::   jp_pblh_max      
   ! parameter for the semi-implicit treatment of Coriolis term  
   REAL(wp), PUBLIC, PARAMETER ::   gamma_Cor  = 0.55_wp
   ! ABL timestep
   REAL(wp), PUBLIC            :: rDt_abl

   !!---------------------------------------------------------------------
   !! ABL parameters for the diagnostic mixing length option nn_amxl = 1 
   !!---------------------------------------------------------------------   
   REAL(wp), PUBLIC, PARAMETER ::   amx1 =    4.3995604393911742_wp
   REAL(wp), PUBLIC, PARAMETER ::   amx2 =  -18.159100102732943_wp
   REAL(wp), PUBLIC, PARAMETER ::   amx3 =   40.241226956967239_wp
   REAL(wp), PUBLIC, PARAMETER ::   amx4 =  -43.603409583363678_wp
   REAL(wp), PUBLIC, PARAMETER ::   amx5 =   17.121715347554314_wp
   REAL(wp), PUBLIC, PARAMETER ::   bmx1 =  -16.262675447730114_wp
   REAL(wp), PUBLIC, PARAMETER ::   bmx2 =   85.088728134110781_wp
   REAL(wp), PUBLIC, PARAMETER ::   bmx3 = -193.46548261141191_wp
   REAL(wp), PUBLIC, PARAMETER ::   bmx4 =  196.71548261141191_wp
   REAL(wp), PUBLIC, PARAMETER ::   bmx5 =  -72.076052686380677_wp 

   !!----------------------------------------------------------------------
   !! NEMO/ABL 4.0 , NEMO Consortium (2018)
   !! $Id: sbc_oce.F90 10882 2019-04-17 15:40:17Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE par_abl
