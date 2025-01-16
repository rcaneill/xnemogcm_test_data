MODULE seddsr
   !!======================================================================
   !!              ***  MODULE  seddsr  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_dsr

   !! * Module variables

   REAL(wp), DIMENSION(jpsol), PUBLIC      :: dens_mol_wgt  ! molecular density 

   !! $Id: seddsr.F90 15450 2021-10-27 14:32:08Z cetlod $
CONTAINS
   
   SUBROUTINE sed_dsr( accmask )
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
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      INTEGER :: ji, jk, js, jw, jn   ! dummy looop indices

      REAL(wp), DIMENSION(jpoce,jpksed) ::zlimo2, zlimno3, zlimso4, zlimfeo    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp)  ::  zreasat
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsr')
!
     ! Initializations
     !----------------------
      
      zlimo2 (:,:)    = 0.    ;   zlimno3(:,:)  = 0.
      zlimso4(:,:)    = 0.
  
      !----------------------------------------------------------
      ! 5.  Beginning of solid reaction
      !---------------------------------------------------------
      
      ! Computes the SMS of the species which do not affect the remin
      ! processes (Alkalinity, PO4, NH4, and the release of Fe from 
      ! biogenic Fe
      ! In the following, all loops start from jpk = 2 as reactions 
      ! only occur in the sediment
      ! --------------------------------------------------------------
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zreasat = rearatpom(ji,jk)
               ! For alkalinity
               pwcpa(ji,jk,jwalk)  = pwcpa(ji,jk,jwalk) + zreasat * ( srno3 - 2.* spo4r )
               ! For Phosphate (in mol/l)
               pwcpa(ji,jk,jwpo4)  = pwcpa(ji,jk,jwpo4) + zreasat * spo4r
               ! For Ammonium
               pwcpa(ji,jk,jwnh4)  = pwcpa(ji,jk,jwnh4) + zreasat * srno3 * radssol(jk,jwnh4)
               ! For iron (in mol/l)
               pwcpa(ji,jk,jwfe2)  = pwcpa(ji,jk,jwfe2) + fecratio(ji) * zreasat * radssol(jk,jwfe2)
            ENDIF
         END DO
      ENDDO

      ! Computes SMS of oxygen
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zlimo2(ji,jk) = pwcp(ji,jk,jwoxy) / ( pwcp(ji,jk,jwoxy) + xksedo2 )
               zlimo2(ji,jk) = MAX( 0., zlimo2(ji,jk) )
               zreasat = rearatpom(ji,jk) * zlimo2(ji,jk)
               ! Acid Silicic 
               pwcpa(ji,jk,jwoxy)  = pwcpa(ji,jk,jwoxy) - so2ut * zreasat
            ENDIF
         END DO
      ENDDO

      !--------------------------------------------------------------------
      ! Denitrification
      !--------------------------------------------------------------------
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zlimno3(ji,jk) = ( 1.0 - zlimo2(ji,jk) ) * pwcp(ji,jk,jwno3) / ( pwcp(ji,jk,jwno3) + xksedno3 ) 
               zlimno3(ji,jk) = MAX(0., zlimno3(ji,jk) )
               zreasat = rearatpom(ji,jk) * zlimno3(ji,jk) * srDnit
               ! For nitrates
               pwcpa(ji,jk,jwno3) = pwcpa(ji,jk,jwno3) - zreasat
               ! For alkalinity
               pwcpa(ji,jk,jwalk) = pwcpa(ji,jk,jwalk) + zreasat
            ENDIF
         END DO
      ENDDO


      !--------------------------------------------------------------------
      ! Begining POC iron reduction
      !--------------------------------------------------------------------

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zlimfeo(ji,jk) = ( 1.0 - zlimno3(ji,jk) - zlimo2(ji,jk) ) * solcp(ji,jk,jsfeo) / ( solcp(ji,jk,jsfeo) + xksedfeo )
               zlimfeo(ji,jk) = MAX(0., zlimfeo(ji,jk) )
               zreasat = 4.0 * rearatpom(ji,jk) * zlimfeo(ji,jk)
               ! For FEOH
               solcpa(ji,jk,jsfeo) = solcpa(ji,jk,jsfeo) - zreasat / volc(ji,jk,jsfeo)
               ! For PO4
               pwcpa(ji,jk,jwpo4)  = pwcpa(ji,jk,jwpo4) + zreasat * redfep
               ! For alkalinity
               pwcpa(ji,jk,jwalk)  = pwcpa(ji,jk,jwalk) + 2.0 * zreasat
               ! Iron
               pwcpa(ji,jk,jwfe2)  = pwcpa(ji,jk,jwfe2) + zreasat * radssol(jk,jwfe2)
            ENDIF
         END DO
      ENDDO

      !--------------------------------------------------------------------
      ! Begining POC denitrification and NO3- diffusion
      !--------------------------------------------------------------------

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zlimso4(ji,jk) = ( 1.0 - zlimno3(ji,jk) - zlimo2(ji,jk) - zlimfeo(ji,jk) ) * pwcp(ji,jk,jwso4) / ( pwcp(ji,jk,jwso4) + xksedso4 )
               zlimso4(ji,jk) = MAX(0., zlimso4(ji,jk) )
               zreasat = 0.5 * rearatpom(ji,jk) * zlimso4(ji,jk)
               ! For sulfur
               pwcpa(ji,jk,jwso4)  =  pwcpa(ji,jk,jwso4) - zreasat 
               pwcpa(ji,jk,jwh2s)  = pwcpa(ji,jk,jwh2s) + zreasat
               ! For alkalinity
               pwcpa(ji,jk,jwalk)  = pwcpa(ji,jk,jwalk) + 2.0 * zreasat
            ENDIF
         END DO
      ENDDO

      ! Secondary redox reactions
      ! -------------------------

      call sed_dsr_redoxb( accmask )

      IF( ln_timing )  CALL timing_stop('sed_dsr')
!      
   END SUBROUTINE sed_dsr

   SUBROUTINE sed_dsr_redoxb( accmask )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr_redox  ***
      !! 
      !!  ** Purpose :  computes secondary redox reactions
      !!
      !!   History :
      !!        !  18-08 (O. Aumont)  Original code
      !!----------------------------------------------------------------------
      !! Arguments
      ! --- local variables
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      INTEGER   ::  ji, jni, jnj, jk   ! dummy looop indices

      REAL(wp)  ::  zalpha, zexcess, zh2seq, zsedfer, zreasat
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsr_redoxb')

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zalpha = ( pwcp(ji,jk,jwfe2) - pwcp(ji,jk,jwlgw) ) * 1E9
               zsedfer = ( zalpha + SQRT( zalpha**2 + 1.E-10 ) ) /2.0 * 1E-9

               ! First pass of the scheme. At the end, it is 1st order 
               ! -----------------------------------------------------
               ! Fe (both adsorbed and solute) + O2
               zreasat = reac_fe2 * zsedfer / radssol(jk,jwfe2) * pwcp(ji,jk,jwoxy)
               pwcpa(ji,jk,jwfe2) = pwcpa(ji,jk,jwfe2) - zreasat * radssol(jk,jwfe2)
               pwcpa(ji,jk,jwoxy) = pwcpa(ji,jk,jwoxy) - 0.25 * zreasat 
               pwcpa(ji,jk,jwpo4) = pwcpa(ji,jk,jwpo4) - redfep * zreasat 
               pwcpa(ji,jk,jwalk) = pwcpa(ji,jk,jwalk) - 2.0 * zreasat
               solcpa(ji,jk,jsfeo) = solcpa(ji,jk,jsfeo) + zreasat / volc(ji,jk,jsfeo)

               ! H2S + O2
               zreasat = reac_h2s * pwcp(ji,jk,jwoxy) * pwcp(ji,jk,jwh2s)
               pwcpa(ji,jk,jwh2s) = pwcpa(ji,jk,jwh2s) - zreasat
               pwcpa(ji,jk,jwoxy) = pwcpa(ji,jk,jwoxy) - 2.0 * zreasat
               pwcpa(ji,jk,jwso4) = pwcpa(ji,jk,jwso4) + zreasat
               pwcpa(ji,jk,jwalk) = pwcpa(ji,jk,jwalk) - 2.0 * zreasat

               ! NH4 + O2
               zreasat = reac_nh4 * pwcp(ji,jk,jwnh4) / radssol(jk,jwnh4) * pwcp(ji,jk,jwoxy)
               pwcpa(ji,jk,jwnh4) = pwcpa(ji,jk,jwnh4) - zreasat * radssol(jk,jwnh4) 
               pwcpa(ji,jk,jwoxy) = pwcpa(ji,jk,jwoxy) - 2.0 * zreasat 
               pwcpa(ji,jk,jwno3) = pwcpa(ji,jk,jwno3) + zreasat
               pwcpa(ji,jk,jwalk) = pwcpa(ji,jk,jwalk) - 2.0 * zreasat

               ! FeS - O2
               zreasat = reac_feso * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes) * pwcp(ji,jk,jwoxy) 
               solcpa(ji,jk,jsfes) = solcpa(ji,jk,jsfes) - zreasat / volc(ji,jk,jsfes)
               pwcpa(ji,jk,jwoxy) = pwcpa(ji,jk,jwoxy) - 2.0 * zreasat
               pwcpa(ji,jk,jwfe2) = pwcpa(ji,jk,jwfe2) + zreasat * radssol(jk,jwfe2)
               pwcpa(ji,jk,jwso4) = pwcpa(ji,jk,jwso4) + zreasat

               ! FEOH + H2S
               zreasat = reac_feh2s * solcp(ji,jk,jsfeo) * volc(ji,jk,jsfeo) * pwcp(ji,jk,jwh2s) 
               solcpa(ji,jk,jsfeo) = solcpa(ji,jk,jsfeo) - 8.0 * zreasat / volc(ji,jk,jsfeo)
               pwcpa(ji,jk,jwh2s) = pwcpa(ji,jk,jwh2s) - zreasat
               pwcpa(ji,jk,jwfe2) = pwcpa(ji,jk,jwfe2) + 8.0 * zreasat * radssol(jk,jwfe2) 
               pwcpa(ji,jk,jwalk) = pwcpa(ji,jk,jwalk) + 14.0 * zreasat 
               pwcpa(ji,jk,jwso4) = pwcpa(ji,jk,jwso4) + zreasat
               pwcpa(ji,jk,jwpo4) = pwcpa(ji,jk,jwpo4) + 8.0 * redfep * zreasat

               ! Fe + H2S
               zh2seq     = MAX(rtrn, 2.1E-3 * hipor(ji,jk))
               zexcess = pwcp(ji,jk,jwh2s) * zsedfer / zh2seq - 1.0
               IF ( zexcess >= 0.0 ) THEN
                  zreasat = reac_fesp * zexcess
                  pwcpa (ji,jk,jwfe2) = pwcpa(ji,jk,jwfe2) - zreasat * radssol(jk,jwfe2)
                  solcpa(ji,jk,jsfes) = solcpa(ji,jk,jsfes) + zreasat / volc(ji,jk,jsfes)
                  pwcpa(ji,jk,jwh2s)  = pwcpa(ji,jk,jwh2s) - zreasat
               ELSE
                  zreasat = reac_fesd * zexcess * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  pwcpa (ji,jk,jwfe2) = pwcpa(ji,jk,jwfe2) - zreasat * radssol(jk,jwfe2)
                  solcpa(ji,jk,jsfes) = solcpa(ji,jk,jsfes) + zreasat / volc(ji,jk,jsfes)
                  pwcpa(ji,jk,jwh2s)  = pwcpa(ji,jk,jwh2s) - zreasat
               ENDIF
               ! For alkalinity
               pwcpa(ji,jk,jwalk)  = pwcpa(ji,jk,jwalk) - zreasat * 2.0
            ENDIF
         END DO
     END DO

      IF( ln_timing )  CALL timing_stop('sed_dsr_redoxb')

  END SUBROUTINE sed_dsr_redoxb

END MODULE seddsr
