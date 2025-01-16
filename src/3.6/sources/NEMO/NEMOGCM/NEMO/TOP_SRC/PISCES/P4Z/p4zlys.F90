MODULE p4zlys
   !!======================================================================
   !!                         ***  MODULE p4zlys  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :    -   !  1988-07  (E. MAIER-REIMER) Original code
   !!              -   !  1998     (O. Aumont) additions
   !!              -   !  1999     (C. Le Quere) modifications
   !!             1.0  !  2004     (O. Aumont) modifications
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!                  !  2011-02  (J. Simeon, J. Orr)  Calcon salinity dependence
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Improvment of calcite dissolution
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_lys        :   Compute the CaCO3 dissolution 
   !!   p4z_lys_init   :   Read the namelist parameters
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE prtctl_trc      !  print control for debugging
   USE p4zche          !  Chemical model
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_lys         ! called in trcsms_pisces.F90
   PUBLIC   p4z_lys_init    ! called in trcsms_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC :: kdca !: diss. rate constant calcite
   REAL(wp), PUBLIC :: nca  !: order of reaction for calcite dissolution

   !! * Module variables
   REAL(wp) :: calcon = 1.03E-2           !: mean calcite concentration [Ca2+] in sea water [mole/kg solution]
 
   INTEGER  :: rmtss                      !: number of seconds per month 

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: p4zlys.F90 3321 2012-03-05 17:10:55Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_lys( kt, knt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lys  ***
      !!
      !! ** Purpose :   CALCULATES DEGREE OF CACO3 SATURATION IN THE WATER
      !!                COLUMN, DISSOLUTION/PRECIPITATION OF CACO3 AND LOSS
      !!                OF CACO3 TO THE CACO3 SEDIMENT POOL.
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt, knt ! ocean time step
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zdispot, zfact, zcalcon
      REAL(wp) ::   zomegaca, zexcess, zexcess0
      CHARACTER (len=25) :: charout
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zco3, zco3sat, zcaldiss, zhinit, zhi   
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('p4z_lys')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zco3, zco3sat, zcaldiss, zhinit, zhi )
      !
      zco3    (:,:,:) = 0.
      zcaldiss(:,:,:) = 0.
      zhinit(:,:,:)   = hi(:,:,:) * 1000. / ( rhop(:,:,:) + rtrn )
      !     -------------------------------------------
      !     COMPUTE [CO3--] and [H+] CONCENTRATIONS
      !     -------------------------------------------

      CALL p4z_che_solve_hi( zhinit, zhi )

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zco3(ji,jj,jk) = trb(ji,jj,jk,jpdic) * ak13(ji,jj,jk) * ak23(ji,jj,jk) / (zhi(ji,jj,jk)**2   &
               &                + ak13(ji,jj,jk) * zhi(ji,jj,jk) + ak13(ji,jj,jk) * ak23(ji,jj,jk) + rtrn )
               hi(ji,jj,jk)   = zhi(ji,jj,jk) * rhop(ji,jj,jk) / 1000.
            END DO
         END DO
      END DO

      !     ---------------------------------------------------------
      !        CALCULATE DEGREE OF CACO3 SATURATION AND CORRESPONDING
      !        DISSOLOUTION AND PRECIPITATION OF CACO3 (BE AWARE OF
      !        MGCO3)
      !     ---------------------------------------------------------

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi

               ! DEVIATION OF [CO3--] FROM SATURATION VALUE
               ! Salinity dependance in zomegaca and divide by rhop/1000 to have good units
               zcalcon  = calcon * ( tsn(ji,jj,jk,jp_sal) / 35._wp )
               zfact    = rhop(ji,jj,jk) / 1000._wp
               zomegaca = ( zcalcon * zco3(ji,jj,jk) ) / ( aksp(ji,jj,jk) * zfact + rtrn )
               zco3sat(ji,jj,jk) = aksp(ji,jj,jk) * zfact / ( zcalcon + rtrn )

               ! SET DEGREE OF UNDER-/SUPERSATURATION
               excess(ji,jj,jk) = 1._wp - zomegaca
               zexcess0 = MAX( 0., excess(ji,jj,jk) )
               zexcess  = zexcess0**nca

               ! AMOUNT CACO3 (12C) THAT RE-ENTERS SOLUTION
               !       (ACCORDING TO THIS FORMULATION ALSO SOME PARTICULATE
               !       CACO3 GETS DISSOLVED EVEN IN THE CASE OF OVERSATURATION)
               zdispot = kdca * zexcess * trb(ji,jj,jk,jpcal)
# if defined key_degrad
               zdispot = zdispot * facvol(ji,jj,jk)
# endif
              !  CHANGE OF [CO3--] , [ALK], PARTICULATE [CACO3],
              !       AND [SUM(CO2)] DUE TO CACO3 DISSOLUTION/PRECIPITATION
              zcaldiss(ji,jj,jk)  = zdispot * rfact2 / rmtss ! calcite dissolution
              !
              tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) + 2. * zcaldiss(ji,jj,jk)
              tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) -      zcaldiss(ji,jj,jk)
              tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) +      zcaldiss(ji,jj,jk)
            END DO
         END DO
      END DO
      !

      IF( lk_iomput .AND. knt == nrdttrc ) THEN
         IF( iom_use( "PH"     ) ) CALL iom_put( "PH"    , -1. * LOG10( MAX( rtrn, hi(:,:,:) ) )  * tmask(:,:,:) )
         IF( iom_use( "CO3"    ) ) CALL iom_put( "CO3"   , zco3(:,:,:)    * 1.e+3            * tmask(:,:,:) )
         IF( iom_use( "CO3sat" ) ) CALL iom_put( "CO3sat", zco3sat(:,:,:) * 1.e+3            * tmask(:,:,:) )
         IF( iom_use( "DCAL"   ) ) CALL iom_put( "DCAL"  , zcaldiss(:,:,:) * 1.e+3 * rfact2r * tmask(:,:,:) )
      ELSE
         IF( ln_diatrc ) THEN
            trc3d(:,:,:,jp_pcs0_3d    ) = -1. * LOG10( hi(:,:,:) ) * tmask(:,:,:)
            trc3d(:,:,:,jp_pcs0_3d + 1) = zco3(:,:,:)              * tmask(:,:,:)
            trc3d(:,:,:,jp_pcs0_3d + 2) = zco3sat(:,:,:)           * tmask(:,:,:)
         ENDIF
      ENDIF
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('lys ')")
        CALL prt_ctl_trc_info(charout)
        CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zco3, zcaldiss, zhinit, zhi, zco3sat )
      !
      IF( nn_timing == 1 )  CALL timing_stop('p4z_lys')
      !
   END SUBROUTINE p4z_lys

   SUBROUTINE p4z_lys_init

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lys_init  ***
      !!
      !! ** Purpose :   Initialization of CaCO3 dissolution parameters
      !!
      !! ** Method  :   Read the nampiscal namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampiscal
      !!
      !!----------------------------------------------------------------------
      INTEGER  ::  ios                 ! Local integer output status for namelist read
      NAMELIST/nampiscal/ kdca, nca
      !!----------------------------------------------------------------------

      REWIND( numnatp_ref )              ! Namelist nampiscal in reference namelist : Pisces CaCO3 dissolution
      READ  ( numnatp_ref, nampiscal, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nampiscal in reference namelist', lwp )

      REWIND( numnatp_cfg )              ! Namelist nampiscal in configuration namelist : Pisces CaCO3 dissolution
      READ  ( numnatp_cfg, nampiscal, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nampiscal in configuration namelist', lwp )
      IF(lwm) WRITE ( numonp, nampiscal )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for CaCO3 dissolution, nampiscal'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    diss. rate constant calcite (per month)   kdca      =', kdca
         WRITE(numout,*) '    order of reaction for calcite dissolution nca       =', nca
      ENDIF

      ! Number of seconds per month 
      rmtss =  nyear_len(1) * rday / raamo
      !
   END SUBROUTINE p4z_lys_init

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_lys( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'p4z_lys: You should not have seen this print! error?', kt
   END SUBROUTINE p4z_lys
#endif 
   !!======================================================================
END MODULE p4zlys
