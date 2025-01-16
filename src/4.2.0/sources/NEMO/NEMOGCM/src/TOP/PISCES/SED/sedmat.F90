MODULE sedmat
   !!======================================================================
   !!              ***  MODULE  sedmat  ***
   !!    Sediment : linear system of equations
   !!=====================================================================
   !! * Modules used
   !!----------------------------------------------------------------------

   USE sed     ! sediment global variable
   USE lib_mpp         ! distribued memory computing library


   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_mat_dsr 
   PUBLIC sed_mat_dsrjac
   PUBLIC sed_mat_dsri
   PUBLIC sed_mat_btb
   PUBLIC sed_mat_btbjac
   PUBLIC sed_mat_btbi
   PUBLIC sed_mat_coef

   !! $Id: sedmat.F90 15450 2021-10-27 14:32:08Z cetlod $
 CONTAINS

    SUBROUTINE sed_mat_coef( nksed )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_coef  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!         For mass balance in kbot+sediment :
       !!              dz3d  (:,1) = dz(1) = 0.5 cm
       !!              volw3d(:,1) = dzkbot ( see sedini.F90 ) 
       !!              dz(2)       = 0.3 cm 
       !!              dz3d(:,2)   = 0.3 + dzdep   ( see seddsr.F90 )     
       !!              volw3d(:,2) and vols3d(l,2) are thickened ( see seddsr.F90 ) 
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER, INTENT(in) :: nksed

       !---Local declarations
       INTEGER  ::  ji, jk
       REAL(wp) ::  aplus, aminus, dxplus, dxminus
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_coef')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       ! first sediment level          
       DO ji = 1, jpoce
          aplus  = ( por(1) + por(2) ) * 0.5
          dxplus = ( dz3d(ji,1) + dz3d(ji,2) ) / 2.
          apluss(ji,1) = ( 1.0 / ( volw3d(ji,1) ) ) * aplus / dxplus

          DO jk = 2, nksed - 1
             aminus  = ( por(jk-1) + por(jk) ) * 0.5
             dxminus = ( dz3d(ji,jk-1) + dz3d(ji,jk) ) / 2.

             aplus   = ( por(jk+1) + por(jk) ) * 0.5
             dxplus  = ( dz3d(ji,jk) + dz3d(ji,jk+1) ) / 2
             !
             aminuss(ji,jk) = ( 1.0 / volw3d(ji,jk) ) * aminus / dxminus
             apluss (ji,jk) = ( 1.0 / volw3d(ji,jk) ) * aplus / dxplus
          END DO

          aminus  = ( por(nksed-1) + por(nksed) ) * 0.5
          dxminus = ( dz3d(ji,nksed-1) + dz3d(ji,nksed) ) / 2.
          aminuss(ji,nksed)  = ( 1.0 / volw3d(ji,nksed) ) * aminus / dxminus

       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_coef')

    END SUBROUTINE sed_mat_coef

    SUBROUTINE sed_mat_dsr( nksed, nvar, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsr  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!         For mass balance in kbot+sediment :
       !!              dz3d  (:,1) = dz(1) = 0.5 cm
       !!              volw3d(:,1) = dzkbot ( see sedini.F90 ) 
       !!              dz(2)       = 0.3 cm 
       !!              dz3d(:,2)   = 0.3 + dzdep   ( see seddsr.F90 )     
       !!              volw3d(:,2) and vols3d(l,2) are thickened ( see seddsr.F90 ) 
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, nksed  ! number of variable
       INTEGER, DIMENSION(jpoce) :: accmask
       INTEGER :: ji

       !---Local declarations
       INTEGER  ::  jk, jn
       REAL(wp), DIMENSION(nksed) :: za, zb, zc

       REAL(wp) ::  rplus,rminus   
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsr')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       jn = nvar
       ! first sediment level          
       DO ji = 1, jpoce
          IF (accmask(ji) == 0) THEN
             rplus  = apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)

             za(1) = 0.
             zb(1) = rplus
             zc(1) = -rplus
 
             DO jk = 2, nksed - 1
                rminus  = aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
                rplus   = apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)
                !     
                za(jk) = -rminus
                zb(jk) = rminus + rplus 
                zc(jk) = -rplus
             END DO

             rminus  = aminuss(ji,nksed) * diff(ji,nksed-1,jn) * radssol(nksed,jn)
             !
             za(nksed) = -rminus
             zb(nksed) = rminus
             zc(nksed) = 0.

             ! solves tridiagonal system of linear equations 
             ! -----------------------------------------------

             pwcpa(ji,1,jn) = pwcpa(ji,1,jn) - ( zc(1) * pwcp(ji,2,jn) + zb(1) * pwcp(ji,1,jn) )
             DO jk = 2, nksed - 1
                pwcpa(ji,jk,jn) =  pwcpa(ji,jk,jn) - ( zc(jk) * pwcp(ji,jk+1,jn) + za(jk) * pwcp(ji,jk-1,jn)    &
                &                  + zb(jk) * pwcp(ji,jk,jn) )
             ENDDO
             pwcpa(ji,nksed,jn) = pwcpa(ji,nksed,jn) - ( za(nksed) * pwcp(ji,nksed-1,jn)    &
             &                     + zb(nksed) * pwcp(ji,nksed,jn) )

          ENDIF
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsr')

    END SUBROUTINE sed_mat_dsr

    SUBROUTINE sed_mat_dsrjac( nksed, nvar, NEQ, NROWPD, jacvode, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsrjac  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!         For mass balance in kbot+sediment :
       !!              dz3d  (:,1) = dz(1) = 0.5 cm
       !!              volw3d(:,1) = dzkbot ( see sedini.F90 ) 
       !!              dz(2)       = 0.3 cm 
       !!              dz3d(:,2)   = 0.3 + dzdep   ( see seddsr.F90 )     
       !!              volw3d(:,2) and vols3d(l,2) are thickened ( see
       !seddsr.F90 ) 
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, nksed, NEQ, NROWPD  ! number of variable
       REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji,jk, jn, jnn, jni, jnj ,jnij
       REAL(wp), DIMENSION(nksed) :: za, zb, zc

       REAL(wp) ::  rplus,rminus
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsrjac')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       jn = nvar
       ! first sediment level          
       DO ji = 1, jpoce
          IF (accmask(ji) == 0 ) THEN
             rplus  = apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)

             za(1) = 0.
             zb(1) = rplus
             zc(1) = -rplus

             DO jk = 2, nksed - 1
                rminus  = aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
                rplus   = apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)
                !     
                za(jk) = -rminus
                zb(jk) = rminus + rplus
                zc(jk) = -rplus
             END DO

             rminus  = aminuss(ji,nksed) * diff(ji,nksed-1,jn) * radssol(nksed,jn)
             !
             za(nksed) = -rminus
             zb(nksed) = rminus
             zc(nksed) = 0.

             ! solves tridiagonal system of linear equations 

             jnn = isvode(jn)
             jnij = jpvode + 1
             jacvode(ji, jnij, jnn) = jacvode(ji,jnij,jnn) - zb(1)
             jnj = jpvode + jnn
             jnij = jnn - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) -zc(1)
             DO jk = 2, nksed - 1
                jni = (jk-1) * jpvode + jnn
                jnj = (jk-2) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - za(jk)
                jnj = (jk-1) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - zb(jk)
                jnj = (jk) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj)   = jacvode(ji, jnij, jnj) - zc(jk)
             END DO
             jni = (nksed-1) * jpvode + jnn
             jnj = (nksed-2) * jpvode + jnn
             jnij = jni - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - za(nksed)
             jnij = jpvode + 1
             jacvode(ji, jnij, jni) = jacvode(ji, jnij, jni) - zb(nksed)
          ENDIF
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsrjac')

    END SUBROUTINE sed_mat_dsrjac

    SUBROUTINE sed_mat_btbi( nksed, nvar, psol, preac, dtsed_in )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btb  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: nksed, nvar      ! number of sediment levels

      REAL(wp), DIMENSION(jpoce,nksed,nvar), INTENT(inout) :: &
          psol, preac

      REAL(wp), INTENT(in) :: dtsed_in

       !---Local declarations
       INTEGER  ::  &
          ji, jk, jn

       REAL(wp) ::  &
          aplus,aminus   ,  &
          rplus,rminus   ,  &
          dxplus,dxminus

       REAL(wp), DIMENSION(nksed)    :: za, zb, zc
       REAL(wp), DIMENSION(nksed)    :: zr, zgamm
       REAL(wp) ::  zbet

       !----------------------------------------------------------------------

      ! Computation left hand side of linear system of 
      ! equations for dissolution reaction
      !---------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_mat_btbi')

      ! first sediment level          
      DO ji = 1, jpoce
         aplus  = ( por1(2) + por1(3) ) / 2.0
         dxplus = ( dz(2) + dz(3) ) / 2.
         rplus  = ( dtsed_in / vols(2) ) * db(ji,2) * aplus / dxplus
         za(2) = 0.
         zb(2) = 1. + rplus
         zc(2) = -rplus

         DO jk = 3, nksed - 1
            aminus  = ( por1(jk-1) + por1(jk) ) * 0.5
            aminus  = ( ( vols(jk-1) / dz(jk-1) ) + ( vols(jk) / dz(jk) ) ) / 2.
            dxminus = ( dz(jk-1) + dz(jk) ) / 2.
            rminus  = ( dtsed_in / vols(jk) ) * db(ji,jk-1) * aminus / dxminus
            !
            aplus   = ( por1(jk) + por1(jk+1) ) * 0.5
            dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
            rplus   = ( dtsed_in / vols(jk) ) * db(ji,jk) * aplus / dxplus
            !     
            za(jk) = -rminus
            zb(jk) = 1. + rminus + rplus
            zc(jk) = -rplus

         ENDDO

         aminus = ( por1(nksed-1) + por1(nksed) ) * 0.5
         dxminus = ( dz(nksed-1) + dz(nksed) ) / 2.
         rminus  = ( dtsed_in / vols(nksed) ) * db(ji,nksed-1) * aminus / dxminus
         !
         za(nksed) = -rminus
         zb(nksed) = 1. + rminus
         zc(nksed) = 0.

         ! solves tridiagonal system of linear equations 
         ! -----------------------------------------------    
         DO jn = 1, nvar
            zr(:) = psol(ji,:,jn)
            zbet     = zb(2) - preac(ji,2,jn) * dtsed_in
            psol(ji,2,jn) = zr(2) / zbet
            ! 
            DO jk = 3, nksed
               zgamm(jk) =  zc(jk-1) / zbet
               zbet      =  zb(jk) - preac(ji,jk,jn) * dtsed_in - za(jk) * zgamm(jk)
               psol(ji,jk,jn) = ( zr(jk) - za(jk) * psol(ji,jk-1,jn) ) / zbet
            ENDDO
            ! 
            DO jk = nksed - 1, 2, -1
               psol(ji,jk,jn) = psol(ji,jk,jn) - zgamm(jk+1) * psol(ji,jk+1,jn)
            ENDDO
         END DO
      END DO
      !
      IF( ln_timing )  CALL timing_stop('sed_mat_btbi')

    END SUBROUTINE sed_mat_btbi


    SUBROUTINE sed_mat_btb( nksed, nvar, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btb  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: &
          nvar, nksed     ! number of sediment levels
       INTEGER, DIMENSION(jpoce) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn

       REAL(wp) ::  &
          aplus,aminus   ,  &
          rplus,rminus   ,  &
          dxplus,dxminus

       REAL(wp), DIMENSION(nksed)      :: za, zb, zc

       !----------------------------------------------------------------------

      ! Computation left hand side of linear system of 
      ! equations for dissolution reaction
      !---------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_mat_btb')

      ! first sediment level          
      jn = nvar
      DO ji = 1, jpoce
         IF (accmask(ji) == 0) THEN
         aplus  = ( por1(2) + por1(3) ) / 2.0
         dxplus = ( dz(2) + dz(3) ) / 2.
         rplus  = ( 1.0 / vols(2) ) * db(ji,2) * aplus / dxplus * rads1sol(2,jn)

         za(2) = 0.
         zb(2) = rplus 
         zc(2) = -rplus

         DO jk = 3, nksed - 1
            aminus  = ( por1(jk-1) + por1(jk) ) * 0.5
            aminus  = ( ( vols(jk-1) / dz(jk-1) ) + ( vols(jk) / dz(jk) ) ) / 2.
            dxminus = ( dz(jk-1) + dz(jk) ) / 2.
            rminus  = ( 1.0 / vols(jk) ) * db(ji,jk-1) * aminus / dxminus * rads1sol(jk,jn)
            !
            aplus   = ( por1(jk) + por1(jk+1) ) * 0.5
            dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
            rplus   = ( 1.0 / vols(jk) ) * db(ji,jk) * aplus / dxplus * rads1sol(jk,jn)
            !     
            za(jk) = -rminus
            zb(jk) = rminus + rplus
            zc(jk) = -rplus

         ENDDO

         aminus = ( por1(nksed-1) + por1(nksed) ) * 0.5
         dxminus = ( dz(nksed-1) + dz(nksed) ) / 2.
         rminus  = ( 1.0 / vols(nksed) ) * db(ji,nksed-1) * aminus / dxminus * rads1sol(nksed,jn)
         !
         za(nksed) = -rminus
         zb(nksed) = rminus
         zc(nksed) = 0.

         ! solves tridiagonal system of linear equations 
         ! -----------------------------------------------    
         pwcpa(ji,2,jn) = pwcpa(ji,2,jn) - ( zc(2) * pwcp(ji,3,jn) + zb(2) * pwcp(ji,2,jn) )
         DO jk = 3, nksed-1
            pwcpa(ji,jk,jn) =  pwcpa(ji,jk,jn) - ( zc(jk) * pwcp(ji,jk+1,jn) + za(jk) * pwcp(ji,jk-1,jn)    &
            &                  + zb(jk) * pwcp(ji,jk,jn) )
         ENDDO
         pwcpa(ji,nksed,jn) = pwcpa(ji,nksed,jn) - ( za(nksed) * pwcp(ji,nksed-1,jn)    &
         &                     + zb(nksed) * pwcp(ji,nksed,jn) )
         ! 
         ENDIF
      END DO
      !
      IF( ln_timing )  CALL timing_stop('sed_mat_btb')
       
    END SUBROUTINE sed_mat_btb

    SUBROUTINE sed_mat_btbjac( nksed, nvar, NEQ, NROWPD, jacvode, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btb  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, nksed, NEQ, NROWPD  ! number of variable
       REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn, jnn, jni, jnj ,jnij

       REAL(wp) ::  &
          aplus,aminus   ,  &
          rplus,rminus   ,  &
          dxplus,dxminus

       REAL(wp), DIMENSION(nksed)      :: za, zb, zc

       !----------------------------------------------------------------------

      ! Computation left hand side of linear system of 
      ! equations for dissolution reaction
      !---------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_mat_btbjac')

      ! first sediment level          
      jn = nvar
      DO ji = 1, jpoce
         IF (accmask(ji) == 0) THEN
         aplus  = ( por1(2) + por1(3) ) / 2.0
         dxplus = ( dz(2) + dz(3) ) / 2.
         rplus  = ( 1.0 / vols(2) ) * db(ji,2) * aplus / dxplus * rads1sol(2,jn)

         za(2) = 0.
         zb(2) = rplus
         zc(2) = -rplus

         DO jk = 3, nksed - 1
            aminus  = ( por1(jk-1) + por1(jk) ) * 0.5
            aminus  = ( ( vols(jk-1) / dz(jk-1) ) + ( vols(jk) / dz(jk) ) ) / 2.
            dxminus = ( dz(jk-1) + dz(jk) ) / 2.
            rminus  = ( 1.0 / vols(jk) ) * db(ji,jk-1) * aminus / dxminus * rads1sol(jk,jn)
            !
            aplus   = ( por1(jk) + por1(jk+1) ) * 0.5
            dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
            rplus   = ( 1.0 / vols(jk) ) * db(ji,jk) * aplus / dxplus * rads1sol(jk,jn)
            !     
            za(jk) = -rminus
            zb(jk) = rminus + rplus
            zc(jk) = -rplus

         ENDDO

         aminus = ( por1(nksed-1) + por1(nksed) ) * 0.5
         dxminus = ( dz(nksed-1) + dz(nksed) ) / 2.
         rminus  = ( 1.0 / vols(nksed) ) * db(ji,nksed-1) * aminus / dxminus * rads1sol(nksed,jn)
         !
         za(nksed) = -rminus
         zb(nksed) = rminus
         zc(nksed) = 0.

         ! solves tridiagonal system of linear equations 
         ! -----------------------------------------------    
         jnn = isvode(jn)
         jni = jpvode + jnn
         jnij = jpvode + 1
         jacvode(ji, jnij, jni) = jacvode(ji,jnij,jni) - zb(2)
         jnj = 2 * jpvode + jnn
         jnij = jni - jnj + jpvode + 1
         jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) -zc(2)
         DO jk = 3, nksed-1
            jni = (jk-1) * jpvode + jnn
            jnj = (jk-2) * jpvode + jnn
            jnij = jni - jnj + jpvode + 1
            jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - za(jk)
            jnj = (jk-1) * jpvode + jnn
            jnij = jni - jnj + jpvode + 1
            jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - zb(jk)
            jnj = (jk) * jpvode + jnn
            jnij = jni - jnj + jpvode + 1
            jacvode(ji, jnij, jnj)   = jacvode(ji, jnij, jnj) - zc(jk)
         ENDDO
         jni = (nksed-1) * jpvode + jnn
         jnj = (nksed-2) * jpvode + jnn
         jnij = jni - jnj + jpvode + 1
         jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - za(nksed)
         jnij = jpvode + 1
         jacvode(ji, jnij, jni) = jacvode(ji, jnij, jni) - zb(nksed)
         ! 
         ENDIF
      END DO
      !
      IF( ln_timing )  CALL timing_stop('sed_mat_btbjac')

    END SUBROUTINE sed_mat_btbjac


    SUBROUTINE sed_mat_dsri( nksed, nvar, preac, psms, dtsed_in, psol )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsr  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!         For mass balance in kbot+sediment :
       !!              dz3d  (:,1) = dz(1) = 0.5 cm
       !!              volw3d(:,1) = dzkbot ( see sedini.F90 ) 
       !!              dz(2)       = 0.3 cm 
       !!              dz3d(:,2)   = 0.3 + dzdep   ( see seddsr.F90 )     
       !!              volw3d(:,2) and vols3d(l,2) are thickened ( see
       !seddsr.F90 ) 
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nksed, nvar  ! number of variable

       REAL(wp), DIMENSION(jpoce,nksed), INTENT(in   ) :: preac  ! reaction rates
       REAL(wp), DIMENSION(jpoce,nksed), INTENT(in   ) :: psms  ! reaction rates
       REAL(wp), DIMENSION(jpoce,nksed), INTENT(inout) :: psol  ! reaction rates
       REAL(wp), INTENT(in) ::  dtsed_in

       !---Local declarations
       INTEGER  ::  ji, jk, jn
       REAL(wp), DIMENSION(jpoce,nksed) :: za, zb, zc, zr
       REAL(wp), DIMENSION(jpoce)        :: zbet
       REAL(wp), DIMENSION(jpoce,nksed) :: zgamm

       REAL(wp) ::  rplus,rminus
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsri')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       jn = nvar
       ! first sediment level          
       DO ji = 1, jpoce
          rplus  = dtsed_in * apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)

          za(ji,1) = 0.
          zb(ji,1) = 1. + rplus
          zc(ji,1) = -rplus
       ENDDO

       DO jk = 2, nksed - 1
          DO ji = 1, jpoce
             rminus  = dtsed_in * aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
             rplus   = dtsed_in * apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)
                !     
             za(ji,jk) = -rminus
             zb(ji,jk) = 1. + rminus + rplus
             zc(ji,jk) = -rplus
          END DO
       END DO

       DO ji = 1, jpoce
          rminus  = dtsed_in * aminuss(ji,nksed) * diff(ji,nksed-1,jn) * radssol(nksed,jn)
          !
          za(ji,nksed) = -rminus
          zb(ji,nksed) = 1. + rminus
          zc(ji,nksed) = 0.
       END DO


       ! solves tridiagonal system of linear equations 
       ! -----------------------------------------------

       zr  (:,:) = psol(:,:) + psms(:,:) * dtsed_in
       zb  (:,:) = zb(:,:) - preac(:,:) * dtsed_in
       zbet(:  ) = zb(:,1)
       psol(:,1) = zr(:,1) / zbet(:)

          ! 
       DO jk = 2, nksed
          DO ji = 1, jpoce
             zgamm(ji,jk) =  zc(ji,jk-1) / zbet(ji)
             zbet(ji)     =  zb(ji,jk) - za(ji,jk) * zgamm(ji,jk)
             psol(ji,jk)  = ( zr(ji,jk) - za(ji,jk) * psol(ji,jk-1) ) / zbet(ji)
          END DO
       ENDDO
          ! 
       DO jk = nksed - 1, 1, -1
          DO ji = 1, jpoce
             psol(ji,jk) = psol(ji,jk) - zgamm(ji,jk+1) * psol(ji,jk+1)
          END DO
       ENDDO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsri')


    END SUBROUTINE sed_mat_dsri


 END MODULE sedmat
