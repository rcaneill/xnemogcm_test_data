MODULE p4zbio
   !!======================================================================
   !!                         ***  MODULE p4zbio  ***
   !! TOP :   PISCES biogeochemical model
   !!         This module is for both PISCES and PISCES-QUOTA
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.6  ! 2015 (O. Aumont) PISCES-QUOTA
   !!----------------------------------------------------------------------
   !!   p4z_bio        :   computes the interactions between the different
   !!                      compartments of PISCES
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zsink         !  vertical flux of particulate matter due to sinking
   USE p4zopt          !  optical model
   USE p4zlim          !  Co-limitations of differents nutrients
   USE p4zprod         !  Growth rate of the 2 phyto groups
   USE p4zmort         !  Mortality terms for phytoplankton
   USE p4zmicro        !  Sources and sinks of microzooplankton
   USE p4zmeso         !  Sources and sinks of mesozooplankton
   USE p5zlim          !  Co-limitations of differents nutrients
   USE p5zprod         !  Growth rate of the 2 phyto groups
   USE p5zmort         !  Mortality terms for phytoplankton
   USE p5zmicro        !  Sources and sinks of microzooplankton
   USE p5zmeso         !  Sources and sinks of mesozooplankton
   USE p4zrem          !  Remineralisation of organic matter
   USE p4zpoc          !  Remineralization of organic particles
   USE p4zagg          !  Aggregation of particles
   USE p4zfechem
   USE p4zligand       !  Prognostic ligand model
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager
  
   IMPLICIT NONE
   PRIVATE

   PUBLIC  p4z_bio    

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zbio.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_bio ( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_bio  ***
      !!
      !! ** Purpose :   Ecosystem model in the whole ocean: computes the
      !!                different interactions between the different compartments
      !!                of PISCES
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, knt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER             :: ji, jj, jk, jn
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_bio')

      ! ASSIGN THE SHEAR RATE THAT IS USED FOR AGGREGATION
      ! OF PHYTOPLANKTON AND DETRITUS. Shear rate is supposed to equal 1
      ! in the mixed layer and 0.1 below the mixed layer.
      xdiss(:,:,:) = 1.
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpkm1 )
         IF( gdepw(ji,jj,jk+1,Kmm) > hmld(ji,jj) )   xdiss(ji,jj,jk) = 0.01
      END_3D

      CALL p4z_opt     ( kt, knt, Kbb, Kmm       )     ! Optic: PAR in the water column
      CALL p4z_sink    ( kt, knt, Kbb, Kmm, Krhs )     ! vertical flux of particulate organic matter
      CALL p4z_fechem  ( kt, knt, Kbb, Kmm, Krhs )     ! Iron chemistry/scavenging
      !
      IF( ln_p4z ) THEN  ! PISCES standard
         ! Phytoplankton only sources/sinks terms
         CALL p4z_lim  ( kt, knt, Kbb, Kmm       )     ! co-limitations by the various nutrients
         CALL p4z_prod ( kt, knt, Kbb, Kmm, Krhs )     ! phytoplankton growth rate over the global ocean. 
         !                                          ! (for each element : C, Si, Fe, Chl )
         CALL p4z_mort ( kt,      Kbb,      Krhs )     ! phytoplankton mortality
         ! zooplankton sources/sinks routines 
         CALL p4z_micro( kt, knt, Kbb,      Krhs )     ! microzooplankton
         CALL p4z_meso ( kt, knt, Kbb, Kmm, Krhs )     ! mesozooplankton
      ELSE  ! PISCES-QUOTA
         ! Phytoplankton only sources/sinks terms
         CALL p5z_lim  ( kt, knt, Kbb, Kmm       )     ! co-limitations by the various nutrients
         CALL p5z_prod ( kt, knt, Kbb, Kmm, Krhs )     ! phytoplankton growth rate over the global ocean. 
         !                                          ! (for each element : C, Si, Fe, Chl )
         CALL p5z_mort ( kt,      Kbb,      Krhs      )     ! phytoplankton mortality
         !  zooplankton sources/sinks routines 
         CALL p5z_micro( kt, knt, Kbb,      Krhs )           ! microzooplankton
         CALL p5z_meso ( kt, knt, Kbb, Kmm, Krhs )           ! mesozooplankton
      ENDIF
      !
      CALL p4z_agg     ( kt, knt, Kbb,      Krhs )     ! Aggregation of particles
      CALL p4z_rem     ( kt, knt, Kbb, Kmm, Krhs )     ! remineralization terms of organic matter+scavenging of Fe
      CALL p4z_poc     ( kt, knt, Kbb, Kmm, Krhs )     ! Remineralization of organic particles
      !
      ! Ligand production. ln_ligand should be set .true. to activate
      IF( ln_ligand )  &
      & CALL p4z_ligand( kt, knt, Kbb,      Krhs )

      ! Update of the size of the different phytoplankton groups
      sized(:,:,:) = MAX(1.0, sizeda(:,:,:) )
      sizen(:,:,:) = MAX(1.0, sizena(:,:,:) )
      IF (ln_p5z) THEN
         sizep(:,:,:) = MAX(1.0, sizepa(:,:,:) )
      ENDIF
      !                                                             !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('bio ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_bio')
      !
   END SUBROUTINE p4z_bio

   !!======================================================================
END MODULE p4zbio
