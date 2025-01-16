MODULE usrdef_nam
   !!======================================================================
   !!                     ***  MODULE usrdef_nam   ***
   !!
   !!                     ===  C1D_PAPA configuration  ===
   !!
   !! User defined : set the domain characteristics of a user configuration
   !!======================================================================
   !! History :  4.0  ! 2016-03  (S. Flavoni, G. Madec)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_nam   : read user defined namelist and set global domain size
   !!   usr_def_hgr   : initialize the horizontal mesh 
   !!----------------------------------------------------------------------
   USE dom_oce  , ONLY: nimpp, njmpp             ! ocean space and time domain
   USE dom_oce  , ONLY: ln_zco, ln_zps, ln_sco   ! flag of type of coordinate
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_nam   ! called in nemogcm.F90 module

   !                              !!* namusr_def namelist *!!
   LOGICAL, PUBLIC ::   ln_bench   ! =T benchmark test with gyre: the gridsize is constant (no need to adjust timestep or viscosity)
   INTEGER, PUBLIC ::   nn_GYRE    ! 1/nn_GYRE = the resolution chosen in degrees and thus defining the horizontal domain size
   REAL(wp), PUBLIC::   rn_bathy   ! Depth in meters for 1D configuration

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_nam.F90 10072 2018-08-28 15:21:50Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usr_def_nam( ldtxt, ldnam, cd_cfg, kk_cfg, kpi, kpj, kpk, kperio )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_nam  ***
      !!                    
      !! ** Purpose :   read user defined namelist and define the domain size
      !!
      !! ** Method  :   read in namusr_def containing all the user specific namelist parameter
      !!
      !!                Here C1D configuration
      !!
      !! ** input   : - namusr_def namelist found in namelist_cfg
      !!----------------------------------------------------------------------
      CHARACTER(len=*), DIMENSION(:), INTENT(out) ::   ldtxt, ldnam    ! stored print information
      CHARACTER(len=*)              , INTENT(out) ::   cd_cfg          ! configuration name
      INTEGER                       , INTENT(out) ::   kk_cfg          ! configuration resolution
      INTEGER                       , INTENT(out) ::   kpi, kpj, kpk   ! global domain sizes 
      INTEGER                       , INTENT(out) ::   kperio          ! lateral global domain b.c. 
      !
      INTEGER ::   ios, ii   ! Local integer
      !!
      NAMELIST/namusr_def/ rn_bathy
      !!----------------------------------------------------------------------
      !
      ii = 1
      !
      REWIND( numnam_cfg )          ! Namelist namusr_def (exist in namelist_cfg only)
      READ  ( numnam_cfg, namusr_def, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namusr_def in configuration namelist', .TRUE. )
      !
      WRITE( ldnam(:), namusr_def )
      !
      cd_cfg = 'C1D'               ! name & resolution (not used)
      kk_cfg = 0

      ! Global Domain size:  C1D domain is 3 x 3 grid-points x 75 or vertical levels
      kpi = 3
      kpj = 3
      kpk = 75 
      !
      !                             ! control print
      WRITE(ldtxt(ii),*) '   '                                                                            ;   ii = ii + 1
      WRITE(ldtxt(ii),*) 'usr_def_nam  : read the user defined namelist (namusr_def) in namelist_cfg'     ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '~~~~~~~~~~~ '                                                                   ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '   Namelist namusr_def : C1 case'                                               ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '      type of vertical coordinate : '                                           ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '         z-coordinate flag                     ln_zco = ', ln_zco               ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '         z-partial-step coordinate flag        ln_zps = ', ln_zps               ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '         s-coordinate flag                     ln_sco = ', ln_sco               ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '      C1D domain = 3 x 3 x75 grid-points                '                       ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '         resulting global domain size :        jpiglo = ', kpi                  ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '                                               jpjglo = ', kpj                  ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '                                               jpkglo = ', kpk                  ;   ii = ii + 1

      !
      !                             ! Set the lateral boundary condition of the global domain
      kperio =  7                   ! C1D configuration : 3x3 basin with cyclic Est-West and Norht-South condition
      !
      WRITE(ldtxt(ii),*) '   Lateral boundary condition of the global domain'                           ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '      C1D : closed basin                 jperio = ', kperio                   ;   ii = ii + 1
      !
   END SUBROUTINE usr_def_nam

   !!======================================================================
END MODULE usrdef_nam
