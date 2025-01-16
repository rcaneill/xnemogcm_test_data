MODULE seddta
   !!======================================================================
   !!                     ***  MODULE  seddta  ***
   !! Sediment data  :  read sediment input data from a file
   !!=====================================================================

   !! * Modules used
   USE sed
   USE sedarr
   USE sedini
   USE phycst, ONLY : rday
   USE iom
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_dta   ! 

   !! *  Module variables
   REAL(wp) ::  conv2    ! [kg/m2/month]-->[g/cm2/s] ( 1 month has 30 days )

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   !!---------------------------------------------------------------------------
   !!   sed_dta  : read the NetCDF data file in online version using module iom
   !!---------------------------------------------------------------------------

   SUBROUTINE sed_dta( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dta  ***
      !!                    
      !! ** Purpose :   Reads data from a netcdf file and 
      !!                initialization of rain and pore water (k=1) components
      !! 
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization ; Use of iom
      !!----------------------------------------------------------------------

      !! Arguments
      INTEGER, INTENT( in ) ::   kt    ! time-step
      INTEGER, INTENT( in ) ::   Kbb, Kmm ! time level indices

      !! * Local declarations
      INTEGER  ::  ji, jj, js, jw, ikt

      REAL(wp), DIMENSION(jpoce) :: zdtap, zdtag
      REAL(wp), DIMENSION(jpi,jpj) :: zwsbio4, zwsbio3, zddust
      REAL(wp) :: zf0, zf1, zf2, zkapp, zratio, zdep
      REAL(wp) :: zzf0, zf0s, zf0b, zzf1, zf1s, zf1b, zzf2, zf2s, zf2b

      !----------------------------------------------------------------------

      ! Initialization of sediment variable 
      ! Spatial dimension is merged, and unity converted if needed
      !-------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dta')

      IF (lwp) THEN
         WRITE(numsed,*)
         WRITE(numsed,*) ' sed_dta : Bottom layer fields'
         WRITE(numsed,*) ' ~~~~~~'
         WRITE(numsed,*) ' Data from SMS model'
         WRITE(numsed,*)
      ENDIF


      ! open file
      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_dta : Sediment fields'
         dtsed = rDt_trc
         conv2 = 1.0e+3 /  1.0e+4 
      ENDIF

      ! Initialization of temporaries arrays  
      zdtap(:)    = 0. 
      zdtag(:)    = 0.  
      zddust(:,:) = 0.0

      ! reading variables
      IF (lwp) WRITE(numsed,*)
      IF (lwp) WRITE(numsed,*) ' sed_dta : Bottom layer fields at time  kt = ', kt
      ! reading variables
      !
      !    Sinking speeds of detritus is increased with depth as shown
      !    by data and from the coagulation theory
      !    -----------------------------------------------------------
      IF (ln_sediment_offline) THEN
         DO_2D( 0, 0, 0, 0 )
            ikt = mbkt(ji,jj)
            zwsbio4(ji,jj) = wsbio2 / rday
            zwsbio3(ji,jj) = wsbio  / rday
         END_2D
      ELSE
         DO_2D( 0, 0, 0, 0 )
            ikt = mbkt(ji,jj)
            zdep = e3t(ji,jj,ikt,Kmm) / rDt_trc
            zwsbio4(ji,jj) = MIN( 0.99 * zdep, wsbio4(ji,jj,ikt) / rday )
            zwsbio3(ji,jj) = MIN( 0.99 * zdep, wsbio3(ji,jj,ikt) / rday )
         END_2D
      ENDIF

      trc_data(:,:,:) = 0.
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj)
         IF ( tmask(ji,jj,ikt) == 1.0 ) THEN
            trc_data(ji,jj,jwsil) = tr(ji,jj,ikt,jpsil,Kbb)
            trc_data(ji,jj,jwoxy) = tr(ji,jj,ikt,jpoxy,Kbb)
            trc_data(ji,jj,jwdic) = tr(ji,jj,ikt,jpdic,Kbb)
            trc_data(ji,jj,jwno3) = tr(ji,jj,ikt,jpno3,Kbb) * redNo3 / redC
            trc_data(ji,jj,jwpo4) = tr(ji,jj,ikt,jppo4,Kbb) / redC
            trc_data(ji,jj,jwalk) = tr(ji,jj,ikt,jptal,Kbb) 
            trc_data(ji,jj,jwnh4) = tr(ji,jj,ikt,jpnh4,Kbb) * redNo3 / redC 
            trc_data(ji,jj,jwh2s) = 0.0
            trc_data(ji,jj,jwso4) = 0.14 * ts(ji,jj,ikt,jp_sal,Kmm) / 1.80655 / 96.062
            trc_data(ji,jj,jwfe2) = tr(ji,jj,ikt,jpfer,Kbb)
            trc_data(ji,jj,jwlgw) = 1E-9
            trc_data(ji,jj,12 )   = MIN(tr(ji,jj,ikt,jpgsi,Kbb), 1E-4) * zwsbio4(ji,jj) * 1E3
            trc_data(ji,jj,13 )   = MIN(tr(ji,jj,ikt,jppoc,Kbb), 1E-4) * zwsbio3(ji,jj) * 1E3
            trc_data(ji,jj,14 )   = MIN(tr(ji,jj,ikt,jpgoc,Kbb), 1E-4) * zwsbio4(ji,jj) * 1E3
            trc_data(ji,jj,15)    = MIN(tr(ji,jj,ikt,jpcal,Kbb), 1E-4) * zwsbio4(ji,jj) * 1E3
            trc_data(ji,jj,16)    = ts(ji,jj,ikt,jp_tem,Kmm)
            trc_data(ji,jj,17)    = ts(ji,jj,ikt,jp_sal,Kmm)
            trc_data(ji,jj,18 )   = ( tr(ji,jj,ikt,jpsfe,Kbb) * zwsbio3(ji,jj) + tr(ji,jj,ikt,jpbfe,Kbb)  &
            &                       * zwsbio4(ji,jj)  ) * 1E3 / ( trc_data(ji,jj,13 ) + trc_data(ji,jj,14 ) + rtrn )
            trc_data(ji,jj,18 )   = MIN(1E-3, trc_data(ji,jj,18 ) )
         ENDIF
      END_2D

      ! Pore water initial concentration [mol/l] in  k=1
      !-------------------------------------------------
      DO jw = 1, jpwat
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jw), trc_data(1:jpi,1:jpj,jw), iarroce(1:jpoce) )
      END DO

      !  Solid components : 
      !-----------------------
      !  Sinking fluxes for OPAL in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jsopal), trc_data(1:jpi,1:jpj,12), iarroce(1:jpoce) ) 
      rainrm_dta(1:jpoce,jsopal) = rainrm_dta(1:jpoce,jsopal) * 1e-4

      !  Sinking fluxes for POC in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce, zdtap(1:jpoce), trc_data(1:jpi,1:jpj,13) , iarroce(1:jpoce) )      
      CALL pack_arr ( jpoce, zdtag(1:jpoce), trc_data(1:jpi,1:jpj,14) , iarroce(1:jpoce) )
      DO ji = 1, jpoce
         zzf2 = 2E-2
         zzf1 = 0.3
         zzf0 = 1.0 - zzf1 - zzf2
         zf0s = zzf0
         zf1s = zzf1
         zf2s = 1.0 - zf1s - zf0s
         zf0b = zzf0
         zf1b = zzf1
         zf2b = 1.0 - zf1b - zf0b
         rainrm_dta(ji,jspoc) =   ( zdtap(ji) * zf0s +  zdtag(ji) * zf0b ) * 1e-4
         rainrm_dta(ji,jspos) =   ( zdtap(ji) * zf1s +  zdtag(ji) * zf1b ) * 1e-4
         rainrm_dta(ji,jspor) =   ( zdtap(ji) * zf2s +  zdtag(ji) * zf2b ) * 1e-4
      END DO

      !  Sinking fluxes for Calcite in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jscal), trc_data(1:jpi,1:jpj,15), iarroce(1:jpoce) )
      rainrm_dta(1:jpoce,jscal) = rainrm_dta(1:jpoce,jscal) * 1e-4

      ! vector temperature [°C] and salinity 
      CALL pack_arr ( jpoce,  temp(1:jpoce), trc_data(1:jpi,1:jpj,16), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce,  salt(1:jpoce), trc_data(1:jpi,1:jpj,17), iarroce(1:jpoce) )
      
      ! Clay rain rate in [mol/(cm**2.s)] 
      ! inputs data in [kg.m-2.sec-1] ---> 1e+3/(1e+4) [g.cm-2.s-1]   
      ! divided after by molecular weight g.mol-1      
      CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jsclay), dust(1:jpi,1:jpj), iarroce(1:jpoce) )
      rainrm_dta(1:jpoce,jsclay) = rainrm_dta(1:jpoce,jsclay) * conv2 / mol_wgt(jsclay)   &
      &                            + wacc(1:jpoce) * por1(2) * dens_sol(jsclay) / mol_wgt(jsclay) / ( rday * 365.0 )
      rainrm_dta(1:jpoce,jsfeo)  = rainrm_dta(1:jpoce,jsclay) * mol_wgt(jsclay) / mol_wgt(jsfeo) * 0.035 * 0.5
      rainrm_dta(1:jpoce,jsclay) = rainrm_dta(1:jpoce,jsclay) * ( 1.0 - 0.035 * 0.5 ) 
      CALL unpack_arr ( jpoce, zddust(1:jpi,1:jpj), iarroce(1:jpoce), wacc(1:jpoce) )
      zddust(:,:) = dust(:,:) + zddust(:,:) / ( rday * 365.0 ) * por1(2) * dens_sol(jsclay) / conv2

!    rainrm_dta(1:jpoce,jsclay) = 1.0E-4 * conv2 / mol_wgt(jsclay)

      ! Iron monosulphide rain rates. Set to 0
      rainrm_dta(1:jpoce,jsfes)  = 0. 

      ! Fe/C ratio in sinking particles that fall to the sediments
      CALL pack_arr ( jpoce,  fecratio(1:jpoce), trc_data(1:jpi,1:jpj,18), iarroce(1:jpoce) )

      ! sediment pore water at 1st layer (k=1)
      pwcp(1:jpoce,1,1:jpwat) = pwcp_dta(1:jpoce,1:jpwat)

      ! Calculation of raintg of each sol. comp.: rainrm in [g/(cm**2.s)]
      DO js = 1, jpsol
         rainrg(1:jpoce,js) = rainrm_dta(1:jpoce,js) * mol_wgt(js)
      ENDDO

      ! computation of dzdep = total thickness of solid material rained [cm] in each cell
      dzdep(:) = 0.
      DO js = 1, jpsol
         dzdep(1:jpoce) = dzdep(1:jpoce) + rainrg(1:jpoce,js) * dtsed / ( dens_sol(js) * por1(2) )
      END DO

      IF( lk_iomput ) THEN
          IF( iom_use("sflxclay" ) ) CALL iom_put( "sflxclay", zddust(:,:) * 1E3 / 1.E4 )
          IF( iom_use("sflxcal" ) )  CALL iom_put( "sflxcal", trc_data(:,:,15) / 1.E4 )
          IF( iom_use("sflxbsi" ) )  CALL iom_put( "sflxbsi", trc_data(:,:,12) / 1.E4 )
          IF( iom_use("sflxpoc" ) )  CALL iom_put( "sflxpoc", ( trc_data(:,:,13) + trc_data(:,:,14) ) / 1.E4 )
      ENDIF

      IF( ln_timing )  CALL timing_stop('sed_dta')
      
   END SUBROUTINE sed_dta

END MODULE seddta
