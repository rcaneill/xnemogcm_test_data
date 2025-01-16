MODULE sedsol
   !!======================================================================
   !!              ***  MODULE  sedsol  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!    Diffusion of solutes in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE sedfunc
   USE seddsr
   USE sedjac
   USE sedbtb
   USE sedco3
   USE sedmat
   USE sedorg
# if ! defined key_agrif
   USE trosk
#endif
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_sol

   !! $Id: sedsol.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
CONTAINS
   
   SUBROUTINE sed_sol( kt ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_sol  ***
      !! 
      !!  ** Purpose :  computes pore water diffusion and reactions
      !!
      !!  ** Methode :  Computation of the redox and dissolution reactions 
      !!                in the sediment.
      !!                The main redox reactions are solved in sed_dsr whereas
      !!                the secondary reactions are solved in sed_dsr_redoxb.
      !!                Inorganic dissolution is solved in sed_inorg
      !!                A strand spliting approach is being used here (see 
      !!                sed_dsr_redoxb for more information). 
      !!                Diffusive fluxes are computed in sed_diff
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model.
      !!                             The original method is replaced by a 
      !!                             Strand splitting method which deals 
      !!                             well with stiff reactions.
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) ::   kt
      ! --- local variables
      INTEGER  :: ji, jk, js, jw, jn, neq   ! dummy looop indices
      REAL(wp), DIMENSION( jpoce, jpvode * jpksed ) :: ZXIN, FVAL
      REAL(wp), DIMENSION(jpoce,jpksed) :: preac
# if ! defined key_agrif      
      INTEGER :: JINDEX, ITOL, IJAC, MLJAC, IMAX
      INTEGER :: MUJAC,LE1, LJAC, LWORK
      INTEGER :: IDID, NMAXSTP, ROSM
      REAL(wp) :: X, XEND
      REAL(wp),DIMENSION(jpoce) :: H
      INTEGER, DIMENSION(jpoce) :: accmask
      REAL(wp), DIMENSION(jpvode * jpksed) :: RTOL, ATOL
      REAL(wp), POINTER :: WORK(:)
      INTEGER, DIMENSION(jpoce,3)   :: ISTAT
      REAL(wp), DIMENSION(jpoce,2)  :: RSTAT
#endif      
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_sol')
!
      IF( kt == nitsed000 ) THEN
         IF (lwp) THEN
            WRITE(numsed,*) ' sed_sol : Organic/inorganic degradation related reactions and diffusion'
            WRITE(numsed,*) ' '
         ENDIF
!         ! 
      ENDIF

      ! 1. Change of geometry
      !    Increase of dz3d(2) thickness : dz3d(2) = dz3d(2)+dzdep
      !    Warning : no change for dz(2)
      !---------------------------------------------------------
      dz3d(1:jpoce,2) = dz3d(1:jpoce,2) + dzdep(1:jpoce)

      ! New values for volw3d(:,2) and vols3d(:,2)
      ! Warning : no change neither for volw(2) nor  vols(2)
      !------------------------------------------------------
      volw3d(1:jpoce,2) = dz3d(1:jpoce,2) * por(2)
      vols3d(1:jpoce,2) = dz3d(1:jpoce,2) * por1(2)

      ! 2. Change of previous solid fractions (due to volum changes) for k=2
      !---------------------------------------------------------------------
      DO js = 1, jpsol
         solcp(:,2,js) = solcp(:,2,js) * dz(2) / dz3d(:,2)
      END DO

      ! 3. New solid fractions (including solid rain fractions) for k=2
      !------------------------------------------------------------------
      DO js = 1, jpsol
         DO ji = 1, jpoce
            IF (dzdep(ji) .ne. 0) THEN
               solcp(ji,2,js) = solcp(ji,2,js) + rainrg(ji,js) * dtsed / ( por1(2) * dz3d(ji,2) )
               ! rainrm are temporary cancel
            ENDIF
         END DO
      ENDDO

      ! 4.  Adjustment of bottom water concen.(pwcp(1)):
      !     We impose that pwcp(2) is constant. Including dzdep in dz3d(:,2) we assume
      !     that dzdep has got a porosity of por(2). So pore water volum of jk=2 increase.
      !     To keep pwcp(2) cste we must compensate this "increase" by a slight adjusment
      !     of bottom water concentration.
      !     This adjustment is compensate at the end of routine
      !-------------------------------------------------------------
      DO jw = 1, jpwat
         pwcp(:,1,jw) = pwcp(:,1,jw) - pwcp(:,2,jw) * dzdep(:) * por(2) / dzkbot(:)
      ENDDO

      ! --------------------------------------------------
      ! Computation of the diffusivities
      ! --------------------------------------------------
      DO js = 1, jpwat
         DO jk = 1, jpksed
            diff(:,jk,js) = ( diff1(js) + diff2(js) * temp(:) ) / ( 1.0 - 2.0 * log( por(jk) ) )
         END DO
      END DO

      ! Impact of bioirrigation and adsorption on diffusion
      ! ---------------------------------------------------
      diff(:,:,jwsil) = diff(:,:,jwsil) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwoxy) = diff(:,:,jwoxy) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwdic) = diff(:,:,jwdic) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwno3) = diff(:,:,jwno3) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwpo4) = diff(:,:,jwpo4) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwalk) = diff(:,:,jwalk) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwh2s) = diff(:,:,jwh2s) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwso4) = diff(:,:,jwso4) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwlgw) = diff(:,:,jwlgw) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwnh4) = diff(:,:,jwnh4) * ( 1.0 + irrig(:,:) )
      diff(:,:,jwfe2) = diff(:,:,jwfe2) * ( 1.0 + 0.1 * irrig(:,:) )

      ! Conversion of volume units
      !----------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksed
            volc(:,jk,js) = ( vols3d(:,jk) / mol_wgt(js) ) /  &
            &                 ( volw3d(:,jk) * 1.e-3 )
         ENDDO
      ENDDO

      ! Compute coefficients commonly used in diffusion
      CALL sed_mat_coef( jpksed )
      ! Apply bioturbation and compute the impact of the slow SMS on species
      CALL sed_btb( kt )
      ! Recompute pH after bioturbation and slow chemistry
      CALL sed_co3( kt )

# if ! defined key_agrif
      ! The following part deals with the stiff ODEs
      ! This is the expensive part of the code and should be carefully
      ! chosen. We use the DVODE solver after many trials to find a cheap 
      ! way to solve the ODEs. This is not necessarily the most efficient 
      ! but this is the one that was not too much of a pain to code and that
      ! was the most precise and quick.
      ! The ones I tried : operator splitting (Strang), hybrid spectral methods
      ! Brent, Powell's hybrid method, ...
      ! ---------------------------------------------------------------------
      NEQ  = jpvode * jpksed
      XEND = dtsed 
      RTOL = rosrtol
      ATOL = rosatol
      ITOL = 1
      IJAC = 1
      DO jn = 1, NEQ
         js = jarr(jn,2)
         IF (js == jwfe2) ATOL(jn) = rosatol / 100.0
      END DO
      MLJAC = jpvode
      MUJAC = jpvode
      LE1  = 2*MLJAC+MUJAC+1
      LJAC = MLJAC+MUJAC+1
      LWORK = NEQ*(LJAC+LE1+8) + 5
      ALLOCATE(WORK(LWORK) )

      X    = 0.0
      H(:)    = dtsed
      WORK  = 0.

      ! Put all the species in one local array (nb of tracers * vertical
      ! dimension
      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            zxin(:,jn) = pwcp(:,jk,js) * 1E6
         ELSE
            zxin(:,jn) = solcp(:,jk,js-jpwat) * 1E6
         ENDIF
      END DO

      ! Set options for VODE : banded matrix. SParse option is much more
      ! expensive except if one computes the sparse Jacobian explicitly
      ! To speed up the computation, one way is to reduce ATOL and RTOL
      ! which may be a good option at the beginning of the simulations 
      ! during the spin up
      ! ----------------------------------------------------------------
      CALL ROSK(NROSORDER, NEQ,X,zxin,XEND,H,RTOL,ATOL,ITOL, sed_jac,  &
           &   MLJAC,MUJAC,WORK,LWORK,IDID,ISTAT,RSTAT)

      accmask(:) = 0.0
      CALL sed_func( NEQ, ZXIN, FVAL, ACCMASK )

      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            pwcp(:,jk,js) = zxin(:,jn) * 1E-6
         ELSE
            solcp(:,jk,js-jpwat) = zxin(:,jn) * 1E-6
         ENDIF
      END DO
      rstepros(:) = ISTAT(:,3)

      DEALLOCATE( WORK )
#endif

      preac(:,:) = 0.
      CALL sed_mat_dsri( jpksed, jwpo4, preac, pwcpa(:,:,jwpo4), dtsed, pwcp(:,:,jwpo4) )
      CALL sed_mat_dsri( jpksed, jwalk, preac, pwcpa(:,:,jwalk), dtsed, pwcp(:,:,jwalk) )

      CALL sed_org( kt )

      !----------------------------------
      !   Back to initial geometry
      !-----------------------------

      !---------------------------------------------------------------------
      !   1/ Compensation for ajustement of the bottom water concentrations
      !      (see note n� 1 about *por(2))
      !--------------------------------------------------------------------
      DO jw = 1, jpwat
         pwcp(:,1,jw) = pwcp(:,1,jw) + pwcp(:,2,jw) * dzdep(:) * por(2) / dzkbot(:)
      ENDDO

      ! 2. Change of previous solid fractions (due to volum changes) for k=2
      !---------------------------------------------------------------------
      DO js = 1, jpsol
         solcp(:,2,js) = solcp(:,2,js) * dz3d(:,2) / dz(2)
      END DO

      !--------------------------------
      !    3/ back to initial geometry
      !--------------------------------
      dz3d  (:,2) = dz(2)
      volw3d(:,2) = dz3d(:,2) * por(2)
      vols3d(:,2) = dz3d(:,2) * por1(2)

      IF( ln_timing )  CALL timing_stop('sed_sol')
!      
   END SUBROUTINE sed_sol

   SUBROUTINE JACFAC
  
   END SUBROUTINE JACFAC

END MODULE sedsol
