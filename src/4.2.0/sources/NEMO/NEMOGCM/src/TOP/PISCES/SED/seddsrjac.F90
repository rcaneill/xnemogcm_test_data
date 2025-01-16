MODULE seddsrjac
   !!======================================================================
   !!              ***  MODULE  seddsr  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE seddsr
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_dsrjac

   !! * Module variables

   !! $Id: seddsr.F90 10362 2018-11-30 15:38:17Z aumont $
CONTAINS
   
   SUBROUTINE sed_dsrjac( NEQ, NROWPD, jacvode, accmask )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  Computation of the redox reactions in sediment.
      !!                The main redox reactions are solved in sed_dsr whereas
      !!                the secondary reactions are solved in sed_dsr_redoxb.
      !!                A strand spliting approach is being used here (see 
      !!                sed_dsr_redoxb for more information). 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model.
      !!                             The original method is replaced by a 
      !!                              Strand splitting method which deals 
      !!                              well with stiff reactions.
      !!----------------------------------------------------------------------
      !! Arguments
      ! --- local variables
      INTEGER, INTENT(in) :: NEQ, NROWPD
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
      INTEGER :: ji, jni, jnj, jnij, jk, js, jw, jn   ! dummy looop indices

      REAL(wp), DIMENSION(jpoce, jpksed) ::zlimo2, zlimno3, zlimso4, zlimfeo    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp), DIMENSION(jpoce, jpksed) ::zlimdo2, zlimdno3, zlimdso4, zlimdfeo    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp)  ::  zvolw, zreasat, zlimtmpo2, zlimtmpno3, zlimtmpfeo, zlimtmpso4
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsrjac')
!
     ! Initializations
     !----------------------
      
      zlimo2 (:,:)    = 0.    ;   zlimno3(:,:)  = 0.
      zlimso4(:,:)    = 0.
  
      !----------------------------------------------------------
      ! 5.  Beginning of solid reaction
      !---------------------------------------------------------
      
      ! Computes SMS of oxygen
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zlimo2(ji,jk) = pwcp(ji,jk,jwoxy) / ( pwcp(ji,jk,jwoxy) + xksedo2 )
               zlimdo2(ji,jk) = xksedo2 / ( pwcp(ji,jk,jwoxy) + xksedo2 )**2
               ! Acid Silicic 
               jni = (jk-1)*jpvode+isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - so2ut * rearatpom(ji,jk) * zlimdo2(ji,jk)
            ENDIF
         END DO
      ENDDO

      !--------------------------------------------------------------------
      ! Denitrification
      !--------------------------------------------------------------------
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN

               zlimno3(ji,jk) = pwcp(ji,jk,jwno3) / ( pwcp(ji,jk,jwno3) + xksedno3 ) 
               zlimdno3(ji,jk) = xksedno3 / ( pwcp(ji,jk,jwno3) + xksedno3 )**2
               ! For nitrates
               jni = (jk-1)*jpvode+isvode(jwno3)
               jnj = (jk-1)*jpvode+isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - srDnit * rearatpom(ji,jk) * (1.0 - zlimo2(ji,jk) ) &
               &         * zlimdno3(ji,jk)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + srDnit * rearatpom(ji,jk) * zlimno3(ji,jk) * zlimdo2(ji,jk)
            ENDIF
         END DO
      ENDDO

      !--------------------------------------------------------------------
      ! Begining POC iron reduction
      !--------------------------------------------------------------------

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zlimfeo(ji,jk)  = solcp(ji,jk,jsfeo) / ( solcp(ji,jk,jsfeo) + xksedfeo )
               zlimdfeo(ji,jk) = xksedfeo / ( solcp(ji,jk,jsfeo) + xksedfeo )**2
               ! For FEOH
               jni = (jk-1)*jpvode+isvode(jpwat+jsfeo)
               jnij = jpvode + 1
               zlimtmpfeo = ( 1.0 - zlimno3(ji,jk) ) * ( 1.0 - zlimo2(ji,jk) ) * zlimdfeo(ji,jk)
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 4.0 * rearatpom(ji,jk) / volc(ji,jk,jsfeo) * zlimtmpfeo
               jnj = (jk-1)*jpvode+isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               zlimtmpo2 = zlimfeo(ji,jk) * zlimdo2(ji,jk) * ( 1.0 - zlimno3(ji,jk) )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 4.0 * rearatpom(ji,jk) / volc(ji,jk,jsfeo) * zlimtmpo2 
               jnj = (jk-1)*jpvode+isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               zlimtmpno3 = zlimfeo(ji,jk) * zlimdno3(ji,jk) * ( 1.0 - zlimo2(ji,jk) )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 4.0 * rearatpom(ji,jk) / volc(ji,jk,jsfeo) * zlimtmpno3
               ! Iron
               zreasat = rearatpom(ji,jk) * 4.0 * radssol(jk,jwfe2)
               jni = (jk-1)*jpvode+isvode(jwfe2)
               jnj = (jk-1)*jpvode+isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zreasat * zlimtmpfeo
               jnj = (jk-1)*jpvode+isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zreasat * zlimtmpo2
               jnj = (jk-1)*jpvode+isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zreasat * zlimtmpno3
            ENDIF
         END DO
      ENDDO

      !--------------------------------------------------------------------
      ! Begining POC denitrification and NO3- diffusion
      !--------------------------------------------------------------------

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zlimso4(ji,jk) = pwcp(ji,jk,jwso4) / ( pwcp(ji,jk,jwso4) + xksedso4 )
               zlimdso4(ji,jk) = xksedso4 / ( pwcp(ji,jk,jwso4) + xksedso4 )**2
               ! For sulfur
               jni = (jk-1) * jpvode + isvode(jwso4)
               jnij = jpvode + 1
               zlimtmpso4 = ( 1.0 - zlimno3(ji,jk) ) * ( 1.0 - zlimo2(ji,jk) ) * ( 1.0 - zlimfeo(ji,jk) ) * zlimdso4(ji,jk)
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 0.5 * rearatpom(ji,jk) * zlimtmpso4
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               zlimtmpo2 = zlimso4(ji,jk) * zlimdo2(ji,jk) * ( 1.0 - zlimno3(ji,jk) )  &
               &      * ( 1.0 - zlimfeo(ji,jk) ) 
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpo2
               jnj = (jk-1) * jpvode + isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               zlimtmpno3 = zlimso4(ji,jk) * ( 1.0 - zlimo2(ji,jk) ) * zlimdno3(ji,jk)   &
               &      * ( 1.0 - zlimfeo(ji,jk) )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpno3 
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               zlimtmpfeo = zlimso4(ji,jk) * ( 1.0 - zlimo2(ji,jk) ) * ( 1.0 - zlimno3(ji,jk) ) * zlimdfeo(ji,jk)
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpfeo
               jni = (jk-1) * jpvode + isvode(jwh2s)
               jnj = (jk-1) * jpvode + isvode(jwso4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpso4
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpo2
               jnj = (jk-1) * jpvode + isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpno3
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpfeo
            ENDIF
         END DO
      ENDDO

      ! Secondary redox reactions
      ! -------------------------

      call sed_dsr_redoxbjac( NEQ, NROWPD, jacvode, accmask )

      IF( ln_timing )  CALL timing_stop('sed_dsrjac')
!      
   END SUBROUTINE sed_dsrjac

   SUBROUTINE sed_dsr_redoxbjac( NEQ, NROWPD, jacvode, accmask )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr_redox  ***
      !! 
      !!  ** Purpose :  computes secondary redox reactions
      !!
      !!   History :
      !!        !  18-08 (O. Aumont)  Original code
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) :: NEQ, NROWPD
      REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      ! --- local variables
      INTEGER   ::  ji, jni, jnj, jnij, jk   ! dummy looop indices

      REAL(wp)  ::  zalpha, zexcess, zh2seq, zsedfer, zdsedfer
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsr_redoxbjac')

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zalpha = ( pwcp(ji,jk,jwfe2) - pwcp(ji,jk,jwlgw) ) * 1E9
               zsedfer = ( zalpha + SQRT( zalpha**2 + 1.E-10 ) ) /2.0 * 1E-9
               zdsedfer = zsedfer*1E9 / SQRT( zalpha**2 +1E-10 ) 

               ! First pass of the scheme. At the end, it is 1st order 
               ! -----------------------------------------------------
               ! Fe (both adsorbed and solute) + O2
               jni = (jk-1) * jpvode + isvode(jwfe2)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fe2 * pwcp(ji,jk,jwoxy) * zdsedfer
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fe2 * zsedfer
               jni = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 0.25 * reac_fe2 * zsedfer / radssol(jk,jwfe2)
               jnj = (jk-1) * jpvode + isvode(jwfe2)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.25 * reac_fe2 / radssol(jk,jwfe2) * pwcp(ji,jk,jwoxy) * zdsedfer
               jni = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnj = (jk-1) * jpvode + isvode(jwfe2)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fe2 / radssol(jk,jwfe2) * pwcp(ji,jk,jwoxy)   &
               &     * zdsedfer / volc(ji,jk,jsfeo)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fe2 / radssol(jk,jwfe2) * zsedfer  &
               &     / volc(ji,jk,jsfeo)

               ! H2S + O2
               jni = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_h2s * pwcp(ji,jk,jwoxy)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_h2s * pwcp(ji,jk,jwh2s)
               jni = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_h2s * pwcp(ji,jk,jwh2s)
               jnj = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_h2s * pwcp(ji,jk,jwoxy)
               jni = (jk-1) * jpvode + isvode(jwso4)
               jnj = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_h2s * pwcp(ji,jk,jwoxy)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_h2s * pwcp(ji,jk,jwh2s)

               ! NH4 + O2
               jni = (jk-1) * jpvode + isvode(jwnh4)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_nh4 * pwcp(ji,jk,jwoxy)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_nh4 * pwcp(ji,jk,jwnh4)
               jni = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_nh4 * pwcp(ji,jk,jwnh4) / radssol(jk,jwnh4)
               jnj = (jk-1) * jpvode + isvode(jwnh4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_nh4 * pwcp(ji,jk,jwoxy) / radssol(jk,jwnh4)
               jni = (jk-1) * jpvode + isvode(jwno3)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_nh4 * pwcp(ji,jk,jwnh4) / radssol(jk,jwnh4)
               jnj = (jk-1) * jpvode + isvode(jwnh4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_nh4 * pwcp(ji,jk,jwoxy) / radssol(jk,jwnh4)

               ! FeS - O2
               jni = (jk-1) * jpvode + isvode(jpwat+jsfes)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_feso * pwcp(ji,jk,jwoxy) 
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_feso * solcp(ji,jk,jsfes)
               jni = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes)
               jni = (jk-1) * jpvode + isvode(jwfe2)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes) * radssol(jk,jwfe2)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes) * radssol(jk,jwfe2)
               jni = (jk-1) * jpvode + isvode(jwso4)
               jnj = (jk-1) * jpvode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes)

               ! FEOH + H2S
               jni = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 8.0 * reac_feh2s * pwcp(ji,jk,jwh2s)
               jnj = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 8.0 * reac_feh2s * solcp(ji,jk,jsfeo)
               jni = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo)
               jni = (jk-1) * jpvode + isvode(jwfe2)
               jnj = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 8.0 * reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo) * radssol(jk,jwfe2)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 8.0 * reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo) * radssol(jk,jwfe2)
               jni = (jk-1) * jpvode + isvode(jwso4)
               jnj = (jk-1) * jpvode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo)
               jnj = (jk-1) * jpvode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo)

               ! Fe + H2S
               zh2seq     = MAX(rtrn, 2.1E-3 * hipor(ji,jk))
               zexcess = pwcp(ji,jk,jwh2s) * zsedfer / zh2seq - 1.0
               IF ( zexcess >= 0.0 ) THEN
                  jni = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fesp * pwcp(ji,jk,jwh2s) * zdsedfer / zh2seq * radssol(jk,jwfe2)
                  jnj = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesp * zsedfer / zh2seq * radssol(jk,jwfe2)
                  jni = (jk-1) * jpvode + isvode(jpwat+jsfes)
                  jnj = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fesp * pwcp(ji,jk,jwh2s) / zh2seq   &
                  &     * zdsedfer / volc(ji,jk,jsfes)
                  jnj = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fesp * zsedfer / zh2seq   &
                  &     / volc(ji,jk,jsfes)
                  jni = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fesp * zsedfer / zh2seq 
                  jnj = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesp * pwcp(ji,jk,jwh2s) * zdsedfer / zh2seq 
               ELSE
                  jni = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fesd * pwcp(ji,jk,jwh2s) / zh2seq   &
                  &     * zdsedfer * radssol(jk,jwfe2) * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * zsedfer / zh2seq * radssol(jk,jwfe2)   &
                  &     * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = (jk-1) * jpvode + isvode(jpwat+jsfes)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * zexcess * radssol(jk,jwfe2) * volc(ji,jk,jsfes)
                  jni = (jk-1) * jpvode + isvode(jpwat+jsfes)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) + reac_fesd * zexcess 
                  jnj = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fesd * solcp(ji,jk,jsfes)   &
                  &     * zdsedfer * pwcp(ji,jk,jwh2s) / zh2seq 
                  jnj = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fesd * solcp(ji,jk,jsfes)   &
                  &     * zsedfer / zh2seq
                  jni = (jk-1) * jpvode + isvode(jwh2s)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fesd * zsedfer / zh2seq    &
                  &     * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = (jk-1) * jpvode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * pwcp(ji,jk,jwh2s) / zh2seq   &
                  &     * zdsedfer * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = (jk-1) * jpvode + isvode(jpwat+jsfes)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * zexcess * volc(ji,jk,jsfes)
               ENDIF
            ENDIF
         END DO
     END DO

      IF( ln_timing )  CALL timing_stop('sed_dsr_redoxbjac')

  END SUBROUTINE sed_dsr_redoxbjac

END MODULE seddsrjac
