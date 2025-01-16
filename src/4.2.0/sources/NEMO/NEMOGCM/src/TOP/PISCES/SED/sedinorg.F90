MODULE sedinorg
   !!======================================================================
   !!              ***  MODULE  sedinorg  ***
   !!    Sediment : dissolution and reaction in pore water of 
   !!               inorganic species
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE sedmat
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_inorg

   !! $Id: seddsr.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   
   SUBROUTINE sed_inorg( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_inorg  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  implicit simultaneous computation of undersaturation
      !!               resulting from diffusive pore water transport and chemical
      !!               pore water reactions. Solid material is consumed according
      !!               to redissolution and remineralisation
      !!
      !!  ** Remarks :
      !!              - undersaturation : deviation from saturation concentration
      !!              - reaction rate   : sink of undersaturation from dissolution
      !!                                 of solid material 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in)  :: kt   ! time step
      ! --- local variables
      INTEGER   ::  ji,jk          ! dummy looop indices
      REAL(wp), DIMENSION(jpoce) ::  zsieq, reac_silf
      REAL(wp)  ::  zsolid1, zreasat, zco3sat
      REAL(wp)  ::  zsatur, zsatur2, znusil, zsolcpcl, zsolcpsi, zexcess
      REAL(wp), DIMENSION(jpoce,jpksed) :: zundsat, zrearat, psms
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_inorg')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_inorg : Dissolution of CaCO3 and BSi  '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF
!
      DO ji = 1, jpoce
         ! -----------------------------------------------
         ! Computation of Si solubility
         ! Param of Ridgwell et al. 2002
         ! -----------------------------------------------

         zsolcpcl = 0.0
         zsolcpsi = 0.0
         DO jk = 1, jpksed
            zsolcpsi = zsolcpsi + solcp(ji,jk,jsopal) * vols3d(ji,jk)
            zsolcpcl = zsolcpcl + solcp(ji,jk,jsclay) * vols3d(ji,jk)
         END DO
         zsolcpsi = MAX( zsolcpsi, rtrn )
         zsieq(ji) = sieqs(ji) * MAX(0.2, 1.0 - (0.045 * zsolcpcl / zsolcpsi )**0.58 )
         reac_silf(ji) = reac_sil * ( 0.05 + 0.055 * ( 1.64 * ( zsolcpcl / zsolcpsi + 0.01 ) )**(-0.75) ) / 1.25 
      END DO

      
      DO ji = 1, jpoce
         DO jk = 2, jpksed
            zsolid1 = volc(ji,jk,jsopal) * solcp(ji,jk,jsopal)
            zsatur = MAX(0., ( zsieq(ji) - pwcp(ji,jk,jwsil) ) / zsieq(ji) )
            zsatur2 = (1.0 + temp(ji) / 400.0 )**37
            znusil = ( 0.225 * ( 1.0 + temp(ji) / 15.) * zsatur + 0.775 * zsatur2 * zsatur**9.25 )
            solcp(ji,jk,jsopal) = solcp(ji,jk,jsopal) - reac_silf(ji) * znusil * dtsed * solcp(ji,jk,jsopal)
            pwcp(ji,jk,jwsil) = pwcp(ji,jk,jwsil) + reac_silf(ji) * znusil * dtsed * zsolid1
         END DO
      END DO

      !---------------------------------------------------------------
      ! Performs CaCO3 particle deposition and redissolution (indice 9)
      !--------------------------------------------------------------

      ! computes co3por from the updated pwcp concentrations (note [co3por] = mol/l)
      ! *densSW(l)**2 converts aksps [mol2/kg sol2] into [mol2/l2] to get [undsat] in [mol/l]
      DO ji = 1, jpoce
         zco3sat = aksps(ji) * densSW(ji) * densSW(ji) / ( calcon2(ji) + rtrn )
         saturco3(ji,:) = 1.0 - co3por(ji,:) / ( rtrn + zco3sat )
         DO jk = 1, jpksed
            zsolid1 = volc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zundsat(ji,jk) = MAX( 0., zco3sat - co3por(ji,jk) )
            zrearat(ji,jk) = ( reac_cal * zsolid1 / ( zco3sat + rtrn ) ) / &
            &                ( 1. + reac_cal * dtsed * zundsat(ji,jk) / ( zco3sat + rtrn ) )
         END DO
      END DO

      psms(:,:) = 0.0
      ! solves tridiagonal system
      CALL sed_mat_dsri( jpksed, jwdic, -zrearat(:,:), psms(:,:), dtsed, zundsat )

      ! New solid concentration values (jk=2 to jksed) for cacO3
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zreasat = zrearat(ji,jk) * dtsed * zundsat(ji,jk) / ( volc(ji,jk,jscal) + rtrn )
            solcp(ji,jk,jscal) = solcp(ji,jk,jscal) - zreasat
            zreasat = zrearat(ji,jk) * dtsed * zundsat(ji,jk)
            ! For DIC
            pwcp(ji,jk,jwdic)  = pwcp(ji,jk,jwdic) + zreasat
            ! For alkalinity
            pwcp(ji,jk,jwalk)  = pwcp(ji,jk,jwalk) + 2.* zreasat
         ENDDO
      ENDDO


      IF( ln_timing )  CALL timing_stop('sed_inorg')
!      
   END SUBROUTINE sed_inorg

END MODULE sedinorg
