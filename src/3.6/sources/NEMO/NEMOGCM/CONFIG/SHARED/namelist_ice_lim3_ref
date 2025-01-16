!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!! LIM3 namelist :  
!!              1 - Generic parameters                 (namicerun)
!!              2 - Ice initialization                 (namiceini)
!!              3 - Ice discretization                 (namiceitd)
!!              4 - Ice dynamics and transport         (namicedyn)
!!              5 - Ice thermodynamics                 (namicethd)
!!              6 - Ice salinity                       (namicesal)
!!              7 - Ice mechanical redistribution      (namiceitdme)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
!------------------------------------------------------------------------------
&namicerun     !   Generic parameters
!------------------------------------------------------------------------------
   jpl            =    5           !  number of ice  categories
   nlay_i         =    2           !  number of ice  layers
   nlay_s         =    1           !  number of snow layers (only 1 is working)
   cn_icerst_in  = "restart_ice"   !  suffix of ice restart name (input)
   cn_icerst_indir = "."           !  directory from which to read input ice restarts
   cn_icerst_out = "restart_ice"   !  suffix of ice restart name (output)
   cn_icerst_outdir = "."          !  directory in which to write output ice restarts
   ln_limdyn     = .true.          !  ice dynamics (T) or thermodynamics only (F)
   rn_amax_n     = 0.999           !  maximum tolerated ice concentration NH
   rn_amax_s     = 0.999           !  maximum tolerated ice concentration SH
   ln_limdiahsb  = .false.         !  check the heat and salt budgets (T) or not (F)
   ln_limdiaout  = .true.          !  output the heat and salt budgets (T) or not (F)
   ln_icectl     = .false.         !  ice points output for debug (T or F)
   iiceprt       = 10              !  i-index for debug
   jiceprt       = 10              !  j-index for debug
/
!------------------------------------------------------------------------------
&namiceini     !   Ice initialization
!------------------------------------------------------------------------------
   ln_iceini      = .true.         !  activate ice initialization (T) or not (F)
   rn_thres_sst   =  2.0           !  maximum water temperature with initial ice (degC)
   rn_hts_ini_n   =  0.3           !  initial real snow thickness (m), North
   rn_hts_ini_s   =  0.3           !        "            "             South
   rn_hti_ini_n   =  3.0           !  initial real ice thickness  (m), North
   rn_hti_ini_s   =  1.0           !        "            "             South
   rn_ati_ini_n   =  0.9           !  initial ice concentration   (-), North
   rn_ati_ini_s   =  0.9           !        "            "             South
   rn_smi_ini_n   =  6.3           !  initial ice salinity     (g/kg), North
   rn_smi_ini_s   =  6.3           !        "            "             South
   rn_tmi_ini_n   =  270.          !  initial ice/snw temperature (K), North
   rn_tmi_ini_s   =  270.          !        "            "             South
/
!------------------------------------------------------------------------------
&namiceitd     !   Ice discretization
!------------------------------------------------------------------------------
   nn_catbnd      =    2           !  computation of ice category boundaries based on
                                   !      1: tanh function
                                   !      2: h^(-alpha), function of rn_himean
   rn_himean      =    2.0         !  expected domain-average ice thickness (m), nn_catbnd = 2 only
/
!------------------------------------------------------------------------------
&namicedyn     !   Ice dynamics and transport
!------------------------------------------------------------------------------
   nn_icestr      =    0           !  ice strength parameteriztaion                      
                                   !     0: Hibler_79     P = pstar*<h>*exp(-c_rhg*A)
                                   !     1: Rothrock_75   P = Cf*coeff*integral(wr.h^2)    
   ln_icestr_bvf  =    .false.     !  ice strength function brine volume (T) or not (F)     
   rn_pe_rdg      =   17.0         !  ridging work divided by pot. energy change in ridging, if nn_icestr = 1
   rn_pstar       =    2.0e+04     !  ice strength thickness parameter (N/m2), nn_icestr = 0 
   rn_crhg        =   20.0         !  ice strength conc. parameter (-), nn_icestr = 0       
   rn_cio         =    5.0e-03     !  ice-ocean drag coefficient           (-)             
   rn_creepl      =    1.0e-12     !  creep limit (s-1)                                   
   rn_ecc         =    2.0         !  eccentricity of the elliptical yield curve          
   nn_nevp        =  120           !  number of EVP subcycles                             
   rn_relast      =    0.333       !  ratio of elastic timescale to ice time step: Telast = dt_ice * rn_relast 
                                   !     advised value: 1/3 (rn_nevp=120) or 1/9 (rn_nevp=300)
/
!------------------------------------------------------------------------------
&namicehdf     !   Ice horizontal diffusion
!------------------------------------------------------------------------------
   nn_ahi0        =    -1          !  horizontal diffusivity computation
                                   !    -1: no diffusion (bypass limhdf)
                                   !     0: use rn_ahi0_ref
                                   !     1: use rn_ahi0_ref x mean grid cell length / ( 2deg mean grid cell length )
                                   !     2: use rn_ahi0_ref x grid cell length      / ( 2deg mean grid cell length )
   rn_ahi0_ref    = 350.0          !  horizontal sea ice diffusivity (m2/s)
                                   !     if nn_ahi0 > 0, rn_ahi0_ref is the reference value at a nominal 2 deg resolution
   nn_convfrq     = 5              !  convergence check frequency of the Crant-Nicholson scheme (perf. optimization)
/
!------------------------------------------------------------------------------
&namicethd     !   Ice thermodynamics
!------------------------------------------------------------------------------
   rn_hnewice  = 0.1               !  thickness for new ice formation in open water (m)
   ln_frazil   = .false.           !  use frazil ice collection thickness as a function of wind (T) or not (F)
   rn_maxfrazb = 1.0               !  maximum fraction of frazil ice collecting at the ice base
   rn_vfrazb   = 0.417             !  thresold drift speed for frazil ice collecting at the ice bottom (m/s)
   rn_Cfrazb   = 5.0               !  squeezing coefficient for frazil ice collecting at the ice bottom
   rn_himin    = 0.10              !  minimum ice thickness (m) used in remapping, must be smaller than rn_hnewice
   rn_betas    = 0.66              !  exponent in lead-ice repratition of snow precipitation
                                   !     betas = 1 -> equipartition, betas < 1 -> more on leads
   rn_kappa_i  = 1.0               !  radiation attenuation coefficient in sea ice (m-1)
   nn_conv_dif = 50                !  maximal number of iterations for heat diffusion computation
   rn_terr_dif = 0.0001            !  maximum temperature after heat diffusion (degC)
   nn_ice_thcon= 1                 !  sea ice thermal conductivity
                                   !     0: k = k0 + beta.S/T (Untersteiner, 1964)
                                   !     1: k = k0 + beta1.S/T - beta2.T (Pringle et al., 2007)
   rn_cdsn     = 0.31              !  thermal conductivity of the snow (0.31 W/m/K, Maykut and Untersteiner, 1971)
                                   !  Obs: 0.1-0.5 (Lecomte et al, JAMES 2013)
   nn_monocat  = 0                 !  virtual ITD mono-category parameterizations (1, jpl = 1 only) or not (0)
                                   !     2: simple piling instead of ridging --- temporary option
                                   !     3: activate G(he) only              --- temporary option
                                   !     4: activate lateral melting only    --- temporary option
  ln_it_qnsice = .true.            !  iterate the surface non-solar flux with surface temperature (T) or not (F)
/
!------------------------------------------------------------------------------
&namicesal     !   Ice salinity
!------------------------------------------------------------------------------
   nn_icesal   =  2                !  ice salinity option
                                   !     1: constant ice salinity (S=rn_icesal)
                                   !     2: varying salinity parameterization S(z,t)
                                   !     3: prescribed salinity profile S(z), Schwarzacher, 1959
   rn_icesal   =  4.               !  ice salinity (g/kg, nn_icesal = 1 only)
   rn_sal_gd   =  5.               !  restoring ice salinity, gravity drainage (g/kg)
   rn_time_gd  =  1.73e+6          !  restoring time scale, gravity drainage  (s)
   rn_sal_fl   =  2.               !  restoring ice salinity, flushing (g/kg)
   rn_time_fl  =  8.64e+5          !  restoring time scale, flushing (s)
   rn_simax    = 20.               !  maximum tolerated ice salinity (g/kg)
   rn_simin    =  0.1              !  minimum tolerated ice salinity (g/kg)
/
!------------------------------------------------------------------------------
&namiceitdme   !   Ice mechanical redistribution (ridging and rafting)
!------------------------------------------------------------------------------
   rn_Cs       =   0.5             !  fraction of shearing energy contributing to ridging
   rn_fsnowrdg =   0.5             !  snow volume fraction that survives in ridging
   rn_fsnowrft =   0.5             !  snow volume fraction that survives in rafting
   nn_partfun  =   1               !  type of ridging participation function
                                   !     0: linear (Thorndike et al, 1975)
                                   !     1: exponential (Lipscomb, 2007
   rn_gstar    =   0.15            !  fractional area of thin ice being ridged (nn_partfun = 0)
   rn_astar    =   0.05            !  exponential measure of ridging ice fraction (nn_partfun = 1)
   rn_hstar    = 100.0             !  determines the maximum thickness of ridged ice (m) (Hibler, 1980)
   ln_rafting  =   .true.          !  rafting activated (T) or not (F)
   rn_hraft    =   0.75            !  threshold thickness for rafting (m)
   rn_craft    =   5.0             !  squeezing coefficient used in the rafting function
   rn_por_rdg  =   0.3             !  porosity of newly ridged ice (Lepparanta et al., 1995)
/
